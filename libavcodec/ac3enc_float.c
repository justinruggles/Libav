/*
 * The simplest AC-3 encoder
 * Copyright (c) 2000 Fabrice Bellard
 * Copyright (c) 2006-2010 Justin Ruggles <justin.ruggles@gmail.com>
 * Copyright (c) 2006-2010 Prakash Punnoor <prakash@punnoor.de>
 *
 * This file is part of Libav.
 *
 * Libav is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * Libav is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with Libav; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/**
 * @file
 * floating-point AC-3 encoder.
 */

#define CONFIG_AC3ENC_FLOAT 1
#include "ac3enc.c"
#include "kbdwin.h"


/**
 * Finalize MDCT and free allocated memory.
 */
static av_cold void mdct_end(AC3MDCTContext *mdct)
{
    av_freep(&mdct->rot_tmp);
    ff_mdct_end(&mdct->fft[0]);
    ff_mdct_end(&mdct->fft[1]);
    av_freep(&mdct->window);
}


/**
 * Initialize MDCT tables.
 * @param nbits log2(MDCT size)
 */
static av_cold int mdct_init(AVCodecContext *avctx, AC3MDCTContext *mdct,
                             int nbits)
{
    float *window;
    int i, n, n2, ret;

    n  = 1 << nbits;
    n2 = n >> 1;

    window = av_malloc(n * sizeof(*window));
    if (!window) {
        av_log(avctx, AV_LOG_ERROR, "Cannot allocate memory.\n");
        return AVERROR(ENOMEM);
    }
    ff_kbd_window_init(window, 5.0, n2);
    for (i = 0; i < n2; i++)
        window[n-1-i] = window[i];
    mdct->window = window;

    FF_ALLOC_OR_GOTO(avctx, mdct->rot_tmp,  n * sizeof(*mdct->rot_tmp),
                     mdct_alloc_fail);

    ret = ff_mdct_init(&mdct->fft[0], nbits,   0, -2.0 / n) ||
          ff_mdct_init(&mdct->fft[1], nbits-1, 0, -2.0 / n2);
    if (ret) {
        mdct_end(mdct);
        return ret;
    }

    return 0;
mdct_alloc_fail:
    mdct_end(mdct);
    return AVERROR(ENOMEM);
}


/**
 * Calculate two 256-point MDCTs
 * @param out 256 output frequency coefficients
 * @param in  512 windowed input audio samples
 */
static void mdct256(AC3MDCTContext *mdct, float *out, const float *in)
{
    float *coef_a = mdct->rot_tmp;
    float *coef_b = mdct->rot_tmp + 128;
    float *rot    = mdct->rot_tmp + 256;
    int i;

    /* part 1 pre-rotation (-N/4) */
    memcpy(rot, in+64, 192 * sizeof(float));
    for (i = 0; i < 64; i++)
        rot[i+192] = -in[i];

    /* part 1 MDCT */
    mdct->fft[1].mdct_calcw(&mdct->fft[1], coef_a, rot);

    /* part 2 pre-rotation (N/4) */
    in += 256;
    for (i = 0; i < 64; i++)
        rot[i] = -in[i+192];
    memcpy(rot+64, in, 192 * sizeof(float));

    /* part 2 MDCT */
    mdct->fft[1].mdct_calcw(&mdct->fft[1], coef_b, rot);

    /* coefficient interleaving */
    for (i = 0; i < 128; i++) {
        out[2*i  ] = coef_a[i];
        out[2*i+1] = coef_b[i];
    }
}


/**
 * Apply KBD window to input samples prior to MDCT.
 */
static void apply_window(DSPContext *dsp, float *output, const float *input,
                         const float *window, unsigned int len)
{
    dsp->vector_fmul(output, input, window, len);
}


/**
 * Normalize the input samples to use the maximum available precision.
 */
static int normalize_samples(AC3EncodeContext *s)
{
    /* Normalization is not needed for floating-point samples, so just return 0 */
    return 0;
}


/**
 * Scale MDCT coefficients from float to 24-bit fixed-point.
 */
static void scale_coefficients(AC3EncodeContext *s)
{
    int start = s->cpl_enabled ? 0 : AC3_MAX_COEFS * AC3_MAX_BLOCKS;
    s->ac3dsp.float_to_fixed24(s->fixed_coef_buffer + start,
                               s->mdct_coef_buffer  + start,
                               AC3_MAX_COEFS * AC3_MAX_BLOCKS *
                               (s->channels + s->cpl_enabled));
}




/**
 * Uninitialize block switching.
 */
static av_cold void block_switch_end(AC3EncodeContext *s)
{
    int ch;
    ff_iir_filter_free_coeffs(s->blksw_coefs);
    ff_iir_filter_free_state(s->temp_state);
    for (ch = 0; ch < s->fbw_channels; ch++) {
        ff_iir_filter_free_state(s->blksw_state[0][ch]);
        ff_iir_filter_free_state(s->blksw_state[1][ch]);
    }
}


/**
 * Initialize block switching.
 */
static av_cold int block_switch_init(AVCodecContext *avctx, AC3EncodeContext *s)
{
    int ch;

    /* initialize high-pass filter with cutoff of 8kHz */
    s->blksw_coefs = ff_iir_filter_init_coeffs(avctx, FF_FILTER_TYPE_BIQUAD,
                                               FF_FILTER_MODE_HIGHPASS, 2,
                                               2.0 * 8000.0 / s->sample_rate,
                                               0, 0);

    if (!s->blksw_coefs)
        return -1;

    s->temp_state = ff_iir_filter_init_state(2);
    if (!s->temp_state) {
        block_switch_end(s);
        return AVERROR(ENOMEM);
    }
    for (ch = 0; ch < s->fbw_channels; ch++) {
        s->blksw_state[0][ch] = ff_iir_filter_init_state(2);
        s->blksw_state[1][ch] = ff_iir_filter_init_state(2);
        if (!s->blksw_state[0][ch] || !s->blksw_state[1][ch]) {
            block_switch_end(s);
            return AVERROR(ENOMEM);
        }
    }

    return 0;
}


/**
 * Find the maximum magnitude in an array of floats.
 */
static float float_absmax(const float *src, int n)
{
    float max = 0.0f;
    n--;
    while (n >= 0) {
        float v = fabs(src[n]);
        if (v > max)
            max = v;
        n--;
    }
    return max;
}


/**
 * Build tree of peak magnitudes for each segment in 3 levels of time division.
 * level 1 = 256 samples x 1
 * level 2 = 128 samples x 2
 * level 3 =  64 samples x 4
 */
static inline void build_peak_tree(const float *src, float peak_tree[10])
{
    /* level 3 peak values */
    peak_tree[6] = float_absmax(src      , 64);
    peak_tree[7] = float_absmax(src +  64, 64);
    peak_tree[8] = float_absmax(src + 128, 64);
    peak_tree[9] = float_absmax(src + 192, 64);
    /* level 2 peak values */
    peak_tree[4] = FFMAX(peak_tree[8], peak_tree[9]);
    peak_tree[3] = FFMAX(peak_tree[6], peak_tree[7]);
    /* level 1 peak value */
    peak_tree[1] = FFMAX(peak_tree[3], peak_tree[4]);
}


/**
 * Saves last segment of each level for comparison with the first segment in
 * each level in the next block.
 */
static inline void save_peak_tree(float peak_tree[10])
{
    peak_tree[5] = peak_tree[9];
    peak_tree[2] = peak_tree[4];
    peak_tree[0] = peak_tree[1];
}


#define SILENCE_THRESHOLD  0.0030517578125f
#define LVL1_THRESHOLD     0.100f
#define LVL2_THRESHOLD     0.075f
#define LVL3_THRESHOLD     0.050f

/**
 * Detect transients to determine block switching flags.
 */
static void block_switch_detection(AC3EncodeContext *s)
{
    int ch, blk, i;
    const float *input_samples;
    float *output_samples;
    float peak_tree[10] = {0.0,};

    for (ch = 0; ch < s->fbw_channels; ch++) {

        /* filter input samples, cascaded biquad high-pass @ 8kHz */
        input_samples  = s->planar_samples[ch];
        for (i = 0; i < 2; i++) {
            output_samples = s->filtered_samples;

            ff_iir_filter_flt(s->blksw_coefs, s->blksw_state[i][ch], AC3_FRAME_SIZE,
                              input_samples, 1, output_samples, 1);
            input_samples  += AC3_FRAME_SIZE;
            output_samples += AC3_FRAME_SIZE;
            ff_iir_filter_copy_state(s->temp_state, s->blksw_state[i][ch], 2);
            ff_iir_filter_flt(s->blksw_coefs, s->temp_state, AC3_BLOCK_SIZE,
                            input_samples, 1, output_samples, 1);

            input_samples  = s->filtered_samples;
        }

        /* first half of first block */
        input_samples = s->filtered_samples;
        build_peak_tree(input_samples, peak_tree);
        save_peak_tree(peak_tree);
        input_samples += AC3_BLOCK_SIZE;

        /* second half of each block */
        for (blk = 0; blk < AC3_MAX_BLOCKS; blk++) {
            AC3Block *block = &s->blocks[blk];

            /* set current block flag */
            build_peak_tree(input_samples, peak_tree);
            block->blk_switch[ch][0] = 0;
            if (peak_tree[1] > SILENCE_THRESHOLD &&
                   (((peak_tree[1] * LVL1_THRESHOLD) > peak_tree[0]) ||
                    ((peak_tree[3] * LVL2_THRESHOLD) > peak_tree[2]) ||
                    ((peak_tree[4] * LVL2_THRESHOLD) > peak_tree[3]) ||
                    ((peak_tree[6] * LVL3_THRESHOLD) > peak_tree[5]) ||
                    ((peak_tree[7] * LVL3_THRESHOLD) > peak_tree[6]) ||
                    ((peak_tree[8] * LVL3_THRESHOLD) > peak_tree[7]) ||
                    ((peak_tree[9] * LVL3_THRESHOLD) > peak_tree[8]))) {
                block->blk_switch[ch][0] = 1;
            }
            save_peak_tree(peak_tree);

            /* set previous block flag */
            block->blk_switch[ch][1] = 0;
            if (!blk)
                block->blk_switch[ch][1] = s->blocks[5].blk_switch[ch][0];
            else
                block->blk_switch[ch][1] = s->blocks[blk-1].blk_switch[ch][0];

            input_samples += AC3_BLOCK_SIZE;
        }
    }
}


AVCodec ff_ac3_encoder = {
    "ac3",
    AVMEDIA_TYPE_AUDIO,
    CODEC_ID_AC3,
    sizeof(AC3EncodeContext),
    ac3_encode_init,
    ac3_encode_frame,
    ac3_encode_close,
    NULL,
    .sample_fmts = (const enum AVSampleFormat[]){AV_SAMPLE_FMT_FLT,AV_SAMPLE_FMT_NONE},
    .long_name = NULL_IF_CONFIG_SMALL("ATSC A/52A (AC-3)"),
    .priv_class = &ac3enc_class,
    .channel_layouts = ac3_channel_layouts,
};
