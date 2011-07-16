/*
 * E-AC-3 encoder
 * Copyright (c) 2011 Justin Ruggles <justin.ruggles@gmail.com>
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
 * E-AC-3 encoder
 */

#include "libavutil/avassert.h"

#define CONFIG_AC3ENC_FLOAT 1
#include "ac3enc.h"
#include "eac3enc.h"
#include "eac3_data.h"


#define AC3ENC_TYPE AC3ENC_TYPE_EAC3
#include "ac3enc_opts_template.c"
static const AVClass eac3enc_class = { "E-AC-3 Encoder", av_default_item_name,
                                       eac3_options, LIBAVUTIL_VERSION_INT };


/**
 * LUT for finding a matching frame exponent strategy index from a set of
 * exponent strategies for a single channel across all 6 blocks.
 */
static int8_t eac3_frame_expstr_index_tab[3][4][4][4][4][4];


void ff_eac3_exponent_init(void)
{
    int i;

    memset(eac3_frame_expstr_index_tab, -1, sizeof(eac3_frame_expstr_index_tab));
    for (i = 0; i < 32; i++) {
        eac3_frame_expstr_index_tab[ff_eac3_frm_expstr[i][0]-1]
                                   [ff_eac3_frm_expstr[i][1]]
                                   [ff_eac3_frm_expstr[i][2]]
                                   [ff_eac3_frm_expstr[i][3]]
                                   [ff_eac3_frm_expstr[i][4]]
                                   [ff_eac3_frm_expstr[i][5]] = i;
    }
}


void ff_eac3_get_frame_exp_strategy(AC3EncodeContext *s)
{
    int ch;

    if (s->num_blocks < 6) {
        s->use_frame_exp_strategy = 0;
        return;
    }

    s->use_frame_exp_strategy = 1;
    for (ch = !s->cpl_on; ch <= s->fbw_channels; ch++) {
        int expstr = eac3_frame_expstr_index_tab[s->exp_strategy[ch][0]-1]
                                                [s->exp_strategy[ch][1]]
                                                [s->exp_strategy[ch][2]]
                                                [s->exp_strategy[ch][3]]
                                                [s->exp_strategy[ch][4]]
                                                [s->exp_strategy[ch][5]];
        if (expstr < 0) {
            s->use_frame_exp_strategy = 0;
            break;
        }
        s->frame_exp_strategy[ch] = expstr;
    }
}


void ff_eac3_compute_aht_strategy(AC3EncodeContext *s)
{
    int blk, ch;

    s->aht_on = 0;
    if (s->cpl_on) {
        s->channel_in_aht[CPL_CH] = 1;
        if (!s->blocks[0].cpl_in_use)
            s->channel_in_aht[CPL_CH] = 0;
        for (blk = 1; blk < AC3_MAX_BLOCKS && s->channel_in_aht[CPL_CH]; blk++)
            if (!s->blocks[blk].cpl_in_use || s->exp_strategy[CPL_CH][blk] != EXP_REUSE)
                s->channel_in_aht[CPL_CH] = 0;
        s->aht_on |= s->channel_in_aht[CPL_CH];
    }
    for (ch = 1; ch <= s->channels; ch++) {
        s->channel_in_aht[ch] = 1;
        if (ch != s->lfe_channel && s->cpl_on && !s->blocks[0].channel_in_cpl[ch])
            s->channel_in_aht[ch] = 0;
        for (blk = 1; blk < AC3_MAX_BLOCKS && s->channel_in_aht[ch]; blk++) {
            if (s->exp_strategy[ch][blk] != EXP_REUSE)
                s->channel_in_aht[ch] = 0;
            if (ch != s->lfe_channel && s->cpl_on && !s->blocks[blk].channel_in_cpl[ch])
                s->channel_in_aht[ch] = 0;
        }
        s->aht_on |= s->channel_in_aht[ch];
    }
#ifdef DEBUG
    for (ch = !s->cpl_on; ch <= s->channels; ch++)
        av_dlog(s->avctx, "aht[%d] = %d\n", ch, s->channel_in_aht[ch]);
    av_dlog(s->avctx, "aht on = %d\n\n", s->aht_on);
#endif
}


void ff_eac3_set_cpl_states(AC3EncodeContext *s)
{
    int ch, blk;
    int first_cpl_coords[AC3_MAX_CHANNELS];

    /* set first cpl coords */
    for (ch = 1; ch <= s->fbw_channels; ch++)
        first_cpl_coords[ch] = 1;
    for (blk = 0; blk < s->num_blocks; blk++) {
        AC3Block *block = &s->blocks[blk];
        for (ch = 1; ch <= s->fbw_channels; ch++) {
            if (block->channel_in_cpl[ch]) {
                if (first_cpl_coords[ch]) {
                    block->new_cpl_coords = 2;
                    first_cpl_coords[ch]  = 0;
                }
            } else {
                first_cpl_coords[ch] = 1;
            }
        }
    }

    /* set first cpl leak */
    for (blk = 0; blk < s->num_blocks; blk++) {
        AC3Block *block = &s->blocks[blk];
        if (block->cpl_in_use) {
            block->new_cpl_leak = 2;
            break;
        }
    }
}


/* sqrt(2)*cos(5*pi/12) */
#define COEF_A 6140887
/* sqrt(2)*cos(2*pi/12)/6 */
#define COEF_C 3424635
/* sqrt(2)*cos(4*pi/12)/6 */
#define COEF_D 1977214
/* 1/6 */
#define ONE_SIXTH 2796203

#define butterfly(a, b) \
{                       \
    tmp0 = a + b;       \
    tmp1 = a - b;       \
    a = tmp0;           \
    b = tmp1;           \
}

static void dct6(int32_t mant[6])
{
    int32_t odd0, odd1;
    int32_t tmp0, tmp1;

    /* stage 1 butterflies */
    butterfly(mant[0], mant[5]);
    butterfly(mant[1], mant[4]);
    butterfly(mant[2], mant[3]);

    /* even */
    butterfly(mant[0], mant[2]);

    /* odd */
    odd0 = MUL64(COEF_A, (mant[3] + mant[5])) >> 24;
    odd1 = mant[3] - mant[4];
    butterfly(mant[5], mant[4]);

    mant[2] = MUL64(COEF_C, mant[2])                    >> 24;
    mant[3] = MUL64((mant[4] - mant[3]), ONE_SIXTH)     >> 24;
    mant[4] = MUL64(COEF_D, (mant[0] - (mant[1] << 1))) >> 24;
    mant[0] = MUL64((mant[0] + mant[1]), ONE_SIXTH)     >> 24;
    mant[1] = MUL64((odd0 + mant[5]),    ONE_SIXTH)     >> 24;
    mant[5] = MUL64((odd0 + odd1),       ONE_SIXTH)     >> 24;
}


static void coefs_to_mantissas_fixed(int32_t *coef, const uint8_t *exp, int len)
{
    int i;
    for (i = 0; i < len; i++)
        coef[i] <<= exp[i];
}


void ff_eac3_aht_processing(AC3EncodeContext *s)
{
    int ch, blk, i;
    AC3Block *block = &s->blocks[0];
    int32_t *pre_mantissa;
    int32_t *block_ptr[AC3_MAX_BLOCKS];

    for (ch = !s->cpl_on; ch <= s->channels; ch++) {
        if (s->channel_in_aht[ch]) {
            int start = s->start_freq[ch] & ~0x3;
            int len   = FFALIGN(block->end_freq[ch] - start, 16);
            start     = FFMIN(AC3_MAX_COEFS, start + len) - len;

            pre_mantissa = s->pre_mantissa[ch] + (start * AC3_MAX_BLOCKS);
            for (blk = 0; blk < AC3_MAX_BLOCKS; blk++) {
                block_ptr[blk] = block->fixed_coef[ch] + (AC3_MAX_COEFS * blk) + start;
                coefs_to_mantissas_fixed(block_ptr[blk], &block->exp[ch][start], len);
            }
            s->fmt_conv.float_interleave((float *)pre_mantissa,
                                         (const float **)block_ptr, len, 6);

            pre_mantissa = s->pre_mantissa[ch] + (s->start_freq[ch] * AC3_MAX_BLOCKS);
            for (i = s->start_freq[ch]; i < block->end_freq[ch]; i++) {
                dct6(pre_mantissa);
                pre_mantissa += AC3_MAX_BLOCKS;
            }
        }
    }
}


static inline int eac3_asym_quant(int mant, int qbits, int clip_min)
{
    int max;

    /* scale */
    mant = ((mant >> (24 - qbits)) + 1) >> 1;

    /* clip */
    max  = (1 << (qbits-1));
    if (mant >= max)
        mant = max - 1;
    /* when Gk=2 or Gk=4 the min value is used to signal large mantissas,
       so we cannot use that value when quantizing small mantissas */
    if (clip_min && mant <= -max)
        mant = -max + 1;
    av_assert2(mant >= -max);

    return mant;
}


static av_always_inline int16_t vec_quant(const int32_t *mant, int bap)
{
    int v, blk;
    uint64_t best_dist;
    int best_index;
    int vec_count = 1 << ff_eac3_bits_vs_hebap[bap];

    best_index = -1;
    best_dist  = UINT64_MAX;
    for (v = 0; v < vec_count; v++) {
        uint64_t dist = 0;
        for (blk = 0; blk < AC3_MAX_BLOCKS; blk++) {
            int64_t diff = mant[blk] - ((int32_t)ff_eac3_mantissa_vq[bap][v][blk] << 9);
            dist += MUL64(diff, diff);
        }
        if (dist < best_dist) {
            best_dist  = dist;
            best_index = v;
        }
    }
    return best_index;
}


/**
 * Mantissa scale factors for Gk=4, starting at bap=8.
 * Stored as a fraction, { num, den }
 */
static const int16_t eac3_gaq_4_scale[9][2] = {
    {    7,    6 },
    {   10,    8 },
    {   31,   24 },
    {   42,   32 },
    {  127,   96 },
    {  170,  128 },
    {  511,  384 },
    {  682,  512 },
    { 2047, 1536 }
};


static  av_always_inline int16_t quantize_large_mantissa(int32_t mant, int bap,
                                                         int gain)
{
    int bits = ff_eac3_bits_vs_hebap[bap];
    int idx  = bap - 8;

    if (mant < 0)
        mant += ff_eac3_gaq_remap_2_4_b[idx][gain-1];
    else
        mant -= 1 << (24 - gain);

    if (gain == 1)
        mant = MUL64(mant, (1 << bits) - 2) >> (bits - 1);
    else
        mant = MUL64(mant, eac3_gaq_4_scale[idx][0]) / eac3_gaq_4_scale[idx][1];

    return eac3_asym_quant(mant, bits - (2 - gain), 0);
}


static av_always_inline int16_t quantize_small_mantissa(int32_t mant, int bap,
                                                        int gain)
{
    int bits = ff_eac3_bits_vs_hebap[bap];

    if (gain == 0)
        mant = MUL64(mant, (1 << bits) - 1) >> bits;

    return eac3_asym_quant(mant << gain, bits-gain, gain);
}


/**
 * Count bits for each coefficient using each gain value.
 * Also set large_mantissa flags for gain=1 and gain=2 for each coefficient.
 */
static void count_bits_gain_coef(int gain_bits[AC3_MAX_COEFS][4],
                                 int large_mantissa[AC3_MAX_COEFS][AC3_MAX_BLOCKS][2],
                                 const int32_t *mant, const uint8_t *bap,
                                 int start_freq, int end_freq)
{
    int i, gain_idx, blk;

    for (i = start_freq; i < end_freq; i++) {
        const int32_t *m = mant + i * AC3_MAX_BLOCKS;
        int b = bap[i];
        int bits;

        if (b > 7) {
            bits = ff_eac3_bits_vs_hebap[b];

            for (gain_idx = 0; gain_idx < 4; gain_idx++) {
                int end_bap, gain;

                /* set gain and end_bap values for the current index */
                switch(gain_idx) {
                case 0: gain = 0;               break;
                case 1: gain = 1; end_bap = 12; break;
                case 2: gain = 1; end_bap = 17; break;
                case 3: gain = 2; end_bap = 17; break;
                }

                /* avoid duplicate counting */
                if (gain_idx == 2 && (b < 12 || b >= 17)) {
                    gain_bits[i][2] = gain_bits[i][1];
                    continue;
                }

                if (gain == 0 || b >= end_bap) {
                    gain_bits[i][gain_idx] = bits * AC3_MAX_BLOCKS;
                } else {
                    gain_bits[i][gain_idx] = 0;

                    for (blk = 0; blk < AC3_MAX_BLOCKS; blk++) {
                        gain_bits[i][gain_idx] += bits - gain;

                        large_mantissa[i][blk][gain-1] = abs(m[blk]) >= (16777216 >> gain);
                        if (large_mantissa[i][blk][gain-1]) {
                            gain_bits[i][gain_idx] += bits - (2 - gain);
                        } else {
                            int ref_dec, gain_dec;
                            /* don't allow worse quantized value than with gain=0 */
                            ref_dec = quantize_small_mantissa(m[blk], b,    0);
                            ref_dec = MUL64(ref_dec, (1 << bits)) / ((1 << bits) - 1);
                            ref_dec <<= (25 - bits);
                            gain_dec = quantize_small_mantissa(m[blk], b, gain) << (25-bits);
                            if (abs(gain_dec - m[blk]) > abs(ref_dec - m[blk])) {
                                gain_bits[i][gain_idx] = INT_MAX;
                                break;
                            }
                        }
                    }
                }
            }
        }
    }
}


static int count_bits_mode(int mode, const uint8_t *bap,
                           int start_freq, int end_freq)
{
    int i;
    int end_bap = mode < EAC3_GAQ_14 ? 12 : 17;
    int gaq_bins = 0;
    int bit_count = 2;

    for (i = start_freq; i < end_freq; i++) {
        int b = bap[i];
        if (b > 0 && b <= 7)
            bit_count += ff_eac3_bits_vs_hebap[b];
        else if (mode != EAC3_GAQ_NO && b > 7 && b < end_bap)
            gaq_bins++;
    }
    if (mode == EAC3_GAQ_124)
        bit_count += 5 * ((gaq_bins + 2) / 3);
    else
        bit_count += gaq_bins;

    return bit_count;
}

int ff_eac3_aht_gaq_analysis_ch(const int32_t *mant, const uint8_t *bap,
                                uint8_t *gaq_mode, uint8_t *gaq_gain,
                                uint8_t *large_mantissa,
                                int start_freq, int end_freq)
{
    int i;
    int mode, end_bap, best_mode;
    int gain_bits[AC3_MAX_COEFS][4];
    int large_mant[AC3_MAX_COEFS][AC3_MAX_BLOCKS][2];
    int mode_bits[4];

    /* check to see if any bins are in the GAQ range */
    mode = EAC3_GAQ_NO;
    for (i = start_freq; i < end_freq; i++) {
        if (bap[i] > 7 && bap[i] < 17) {
            mode = EAC3_GAQ_124;
            break;
        }
    }
    if (mode == EAC3_GAQ_NO) {
        *gaq_mode = EAC3_GAQ_NO;
        mode_bits[EAC3_GAQ_NO] = count_bits_mode(EAC3_GAQ_NO, bap, start_freq,
                                                 end_freq);
        for (i = start_freq; i < end_freq; i++) {
            if (bap[i] > 7)
                mode_bits[EAC3_GAQ_NO] += ff_eac3_bits_vs_hebap[bap[i]] * AC3_MAX_BLOCKS;
        }
        return mode_bits[EAC3_GAQ_NO];
    }

    /* count bits used for each mode */
    count_bits_gain_coef(gain_bits, large_mant, mant, bap, start_freq, end_freq);
    for (mode = EAC3_GAQ_NO; mode <= EAC3_GAQ_124; mode++)
        mode_bits[mode] = count_bits_mode(mode, bap, start_freq, end_freq);
    for (i = start_freq; i < end_freq; i++) {
        if (bap[i] > 7) {
            mode_bits[EAC3_GAQ_NO]  +=        gain_bits[i][0];
            mode_bits[EAC3_GAQ_12]  += FFMIN (gain_bits[i][0], gain_bits[i][1]);
            mode_bits[EAC3_GAQ_14]  += FFMIN (gain_bits[i][0], gain_bits[i][3]);
            mode_bits[EAC3_GAQ_124] += FFMIN3(gain_bits[i][0], gain_bits[i][2], gain_bits[i][3]);
        }
    }

    /* select mode with fewest bits */
    best_mode = EAC3_GAQ_NO;
    for (mode = EAC3_GAQ_NO; mode <= EAC3_GAQ_124; mode++) {
        if (mode_bits[mode] < mode_bits[best_mode])
            best_mode = mode;
    }
    *gaq_mode = mode = best_mode;

    /* set gains and large mantissa flags */
    end_bap = mode < EAC3_GAQ_14 ? 12 : 17;
    for (i = start_freq; i < end_freq; i++) {
        if (bap[i] > 7) {
            if (mode == EAC3_GAQ_NO || bap[i] >= end_bap)
                gaq_gain[i] = 0;
            else {
                switch (mode) {
                case EAC3_GAQ_12:
                    gaq_gain[i] = gain_bits[i][1] <= gain_bits[i][0];
                    break;
                case EAC3_GAQ_14:
                    gaq_gain[i] = gain_bits[i][3] <= gain_bits[i][0] ? 2 : 0;
                    break;
                case EAC3_GAQ_124:
                    if (gain_bits[i][0] < gain_bits[i][2]) {
                        if (gain_bits[i][0] < gain_bits[i][3])
                            gaq_gain[i] = 0;
                        else
                            gaq_gain[i] = 2;
                    } else {
                        if (gain_bits[i][2] < gain_bits[i][3])
                            gaq_gain[i] = 1;
                        else
                            gaq_gain[i] = 2;
                    }
                    break;
                }
                if (gaq_gain[i]) {
                    uint8_t *lm = large_mantissa + i * AC3_MAX_BLOCKS;
                    int blk;
                    for (blk = 0; blk < AC3_MAX_BLOCKS; blk++)
                        lm[blk] = large_mant[i][blk][gaq_gain[i]-1];
                }
            }
        }
    }

    return mode_bits[mode];
}


/**
 * Quantize a set of pre-mantissas for a single channel for all blocks.
 */
void ff_eac3_aht_quantize_mantissas_ch(const int32_t *mant, const uint8_t *bap,
                                       int16_t *qmant, uint8_t gaq_mode,
                                       const uint8_t *gaq_gain,
                                       const uint8_t *large_mantissa,
                                       uint8_t *encoded_gaq_gain,
                                       int start_freq, int end_freq)
{
    int i, blk;
    int end_bap = gaq_mode < EAC3_GAQ_14 ? 12 : 17;

    mant           += start_freq * AC3_MAX_BLOCKS;
    qmant          += start_freq * AC3_MAX_BLOCKS;
    large_mantissa += start_freq * AC3_MAX_BLOCKS;

    for (i = start_freq; i < end_freq; i++) {
        int b = bap[i];

        if (b > 7) {
            /* Gain Adaptive Quantization (GAQ) */
            int gain = 0;
            if (gaq_mode != EAC3_GAQ_NO && b < end_bap)
                gain = gaq_gain[i];
            for (blk = 0; blk < AC3_MAX_BLOCKS; blk++) {
                if (gain && large_mantissa[blk]) {
                    /* large mantissa (Gk=2 or 4) */
                    qmant[blk] = quantize_large_mantissa(mant[blk], b, gain);
                } else {
                    /* small mantissa, Gk=1, or mode=EAC3_GAQ_NO */
                    qmant[blk] = quantize_small_mantissa(mant[blk], b, gain);
                }
            }
        } else if (b > 0) {
            /* Vector Quantization */
            qmant[0] = vec_quant(mant, b);
        }

        large_mantissa += AC3_MAX_BLOCKS;
        mant           += AC3_MAX_BLOCKS;
        qmant          += AC3_MAX_BLOCKS;
    }

    /* encode GAQ gain values */
    if (gaq_mode == EAC3_GAQ_12 || gaq_mode == EAC3_GAQ_14) {
        for (i = start_freq; i < end_freq; i++) {
            if (bap[i] > 7 && bap[i] < end_bap)
                encoded_gaq_gain[i] = !!gaq_gain[i];
        }
    } else if (gaq_mode == EAC3_GAQ_124) {
        int gain_cnt = 0;
        uint8_t *av_uninit(gain_ptr);
        for (i = start_freq; i < end_freq; i++) {
            if (bap[i] > 7 && bap[i] < end_bap) {
                switch (gain_cnt) {
                case 0:
                    gain_ptr = &encoded_gaq_gain[i];
                    *gain_ptr = gaq_gain[i] * 9;
                    gain_cnt = 1;
                    break;
                case 1:
                    *gain_ptr += gaq_gain[i] * 3;
                    encoded_gaq_gain[i] = 128;
                    gain_cnt = 2;
                    break;
                case 2:
                    *gain_ptr += gaq_gain[i];
                    encoded_gaq_gain[i] = 128;
                    gain_cnt = 0;
                    break;
                }
            }
        }
    }
}


void ff_eac3_output_frame_header(AC3EncodeContext *s)
{
    int blk, ch;
    AC3EncOptions *opt = &s->options;

    put_bits(&s->pb, 16, 0x0b77);                   /* sync word */

    /* BSI header */
    put_bits(&s->pb,  2, 0);                        /* stream type = independent */
    put_bits(&s->pb,  3, 0);                        /* substream id = 0 */
    put_bits(&s->pb, 11, (s->frame_size / 2) - 1);  /* frame size */
    if (s->bit_alloc.sr_shift) {
        put_bits(&s->pb, 2, 0x3);                   /* fscod2 */
        put_bits(&s->pb, 2, s->bit_alloc.sr_code);  /* sample rate code */
    } else {
        put_bits(&s->pb, 2, s->bit_alloc.sr_code);  /* sample rate code */
        put_bits(&s->pb, 2, s->num_blks_code);      /* number of blocks */
    }
    put_bits(&s->pb, 3, s->channel_mode);           /* audio coding mode */
    put_bits(&s->pb, 1, s->lfe_on);                 /* LFE channel indicator */
    put_bits(&s->pb, 5, s->bitstream_id);           /* bitstream id (EAC3=16) */
    put_bits(&s->pb, 5, -opt->dialogue_level);      /* dialogue normalization level */
    put_bits(&s->pb, 1, 0);                         /* no compression gain */
    /* mixing metadata*/
    put_bits(&s->pb, 1, opt->eac3_mixing_metadata);
    if (opt->eac3_mixing_metadata) {
        if (s->channel_mode > AC3_CHMODE_STEREO)
            put_bits(&s->pb, 2, opt->preferred_stereo_downmix);
        if (s->has_center) {
            put_bits(&s->pb, 3, s->ltrt_center_mix_level);
            put_bits(&s->pb, 3, s->loro_center_mix_level);
        }
        if (s->has_surround) {
            put_bits(&s->pb, 3, s->ltrt_surround_mix_level);
            put_bits(&s->pb, 3, s->loro_surround_mix_level);
        }
        if (s->lfe_on)
            put_bits(&s->pb, 1, 0);
        put_bits(&s->pb, 1, 0);                     /* no program scale */
        put_bits(&s->pb, 1, 0);                     /* no ext program scale */
        put_bits(&s->pb, 2, 0);                     /* no mixing parameters */
        if (s->channel_mode < AC3_CHMODE_STEREO)
            put_bits(&s->pb, 1, 0);                 /* no pan info */
        put_bits(&s->pb, 1, 0);                     /* no frame mix config info */
    }
    /* info metadata*/
    put_bits(&s->pb, 1, opt->eac3_info_metadata);
    if (opt->eac3_info_metadata) {
        put_bits(&s->pb, 3, s->bitstream_mode);
        put_bits(&s->pb, 1, opt->copyright);
        put_bits(&s->pb, 1, opt->original);
        if (s->channel_mode == AC3_CHMODE_STEREO) {
            put_bits(&s->pb, 2, opt->dolby_surround_mode);
            put_bits(&s->pb, 2, opt->dolby_headphone_mode);
        }
        if (s->channel_mode >= AC3_CHMODE_2F2R)
            put_bits(&s->pb, 2, opt->dolby_surround_ex_mode);
        put_bits(&s->pb, 1, opt->audio_production_info);
        if (opt->audio_production_info) {
            put_bits(&s->pb, 5, opt->mixing_level - 80);
            put_bits(&s->pb, 2, opt->room_type);
            put_bits(&s->pb, 1, opt->ad_converter_type);
        }
        put_bits(&s->pb, 1, 0);
    }
    if (s->num_blocks != 6)
        put_bits(&s->pb, 1, !(s->avctx->frame_number % 6)); /* converter sync flag */
    put_bits(&s->pb, 1, 0);                         /* no additional bit stream info */

    /* frame header */
    if (s->num_blocks == 6) {
    put_bits(&s->pb, 1, !s->use_frame_exp_strategy);/* exponent strategy syntax */
    put_bits(&s->pb, 1, s->aht_on);                 /* aht enabled */
    }
    put_bits(&s->pb, 2, 0);                         /* snr offset strategy = 1 */
    put_bits(&s->pb, 1, 0);                         /* transient pre-noise processing enabled = no */
    put_bits(&s->pb, 1, 0);                         /* block switch syntax enabled = no */
    put_bits(&s->pb, 1, 0);                         /* dither flag syntax enabled = no */
    put_bits(&s->pb, 1, 0);                         /* bit allocation model syntax enabled = no */
    put_bits(&s->pb, 1, 0);                         /* fast gain codes enabled = no */
    put_bits(&s->pb, 1, 0);                         /* dba syntax enabled = no */
    put_bits(&s->pb, 1, 0);                         /* skip field syntax enabled = no */
    put_bits(&s->pb, 1, 0);                         /* spx enabled = no */
    /* coupling strategy use flags */
    if (s->channel_mode > AC3_CHMODE_MONO) {
        put_bits(&s->pb, 1, s->blocks[0].cpl_in_use);
        for (blk = 1; blk < s->num_blocks; blk++) {
            AC3Block *block = &s->blocks[blk];
            put_bits(&s->pb, 1, block->new_cpl_strategy);
            if (block->new_cpl_strategy)
                put_bits(&s->pb, 1, block->cpl_in_use);
        }
    }
    /* exponent strategy */
    if (s->use_frame_exp_strategy) {
        for (ch = !s->cpl_on; ch <= s->fbw_channels; ch++)
            put_bits(&s->pb, 5, s->frame_exp_strategy[ch]);
    } else {
        for (blk = 0; blk < s->num_blocks; blk++)
            for (ch = !s->blocks[blk].cpl_in_use; ch <= s->fbw_channels; ch++)
                put_bits(&s->pb, 2, s->exp_strategy[ch][blk]);
    }
    if (s->lfe_on) {
        for (blk = 0; blk < s->num_blocks; blk++)
            put_bits(&s->pb, 1, s->exp_strategy[s->lfe_channel][blk]);
    }
    /* E-AC-3 to AC-3 converter exponent strategy (not optional when num blocks == 6) */
    if (s->num_blocks != 6) {
        put_bits(&s->pb, 1, 0);
    } else {
    for (ch = 1; ch <= s->fbw_channels; ch++) {
        if (s->use_frame_exp_strategy)
            put_bits(&s->pb, 5, s->frame_exp_strategy[ch]);
        else
            put_bits(&s->pb, 5, 0);
    }
    }
    /* AHT info */
    if (s->aht_on) {
        for (ch = !s->cpl_on; ch <= s->channels; ch++) {
            int write_aht = 1;
            for (blk = 1; blk < AC3_MAX_BLOCKS; blk++)
                if (s->exp_strategy[ch][blk] != EXP_REUSE ||
                    (ch == CPL_CH && s->blocks[blk].new_cpl_strategy))
                    write_aht = 0;
            if (write_aht)
                put_bits(&s->pb, 1, s->channel_in_aht[ch]);
        }
    }
    /* snr offsets */
    put_bits(&s->pb, 6, s->coarse_snr_offset);
    put_bits(&s->pb, 4, s->fine_snr_offset[1]);
    /* block start info */
    if (s->num_blocks > 1)
        put_bits(&s->pb, 1, 0);
}


#if CONFIG_EAC3_ENCODER
AVCodec ff_eac3_encoder = {
    .name            = "eac3",
    .type            = AVMEDIA_TYPE_AUDIO,
    .id              = CODEC_ID_EAC3,
    .priv_data_size  = sizeof(AC3EncodeContext),
    .init            = ff_ac3_encode_init,
    .encode          = ff_ac3_float_encode_frame,
    .close           = ff_ac3_encode_close,
    .sample_fmts     = (const enum AVSampleFormat[]){AV_SAMPLE_FMT_FLT,AV_SAMPLE_FMT_NONE},
    .long_name       = NULL_IF_CONFIG_SMALL("ATSC A/52 E-AC-3"),
    .priv_class      = &eac3enc_class,
    .channel_layouts = ff_ac3_channel_layouts,
};
#endif
