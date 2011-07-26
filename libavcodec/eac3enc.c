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

//#define ASSERT_LEVEL 2
#include "libavutil/avassert.h"

#define CONFIG_AC3ENC_FLOAT 1
#include "ac3enc.h"
#include "eac3enc.h"
#include "eac3.h"
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


void ff_eac3_spx_strategy_init(AC3EncodeContext *s)
{
    int i, bin, bnd, spx_start_subband, spx_end_subband;
    uint8_t *spx_band_sizes = s->spx_band_sizes;

    /* TODO: make start/end tables similar to coupling strategy */
    if (s->avctx->bit_rate / s->fbw_channels > 64000) {
        s->spx_enabled = 0;
        return;
    }
    s->spx_copy_start_code = 0;
    if (s->fbw_channels == 1) {
        s->spx_start_code = 4;
        s->spx_end_code   = 4;
        s->use_spx_default_struct = 0;
    } else if (s->fbw_channels == 2) {
        s->spx_start_code = 6;
        s->spx_end_code   = 4;
        s->use_spx_default_struct = 0;
    } else {
        s->spx_start_code = 5;
        s->spx_end_code   = 5;
        s->use_spx_default_struct = 1;
    }

    spx_start_subband      = ff_eac3_spx_start_subband_tab[s->spx_start_code];
    spx_end_subband        = ff_eac3_spx_end_subband_tab[s->spx_end_code];
    s->spx_copy_start_freq = s->spx_copy_start_code * 12 + 25;
    s->spx_start_freq      = spx_start_subband      * 12 + 25;
    s->spx_end_freq        = spx_end_subband        * 12 + 25;

    av_assert2(s->spx_copy_start_freq < s->spx_start_freq);
    av_assert2(s->spx_start_freq < s->spx_end_freq);

    s->num_spx_subbands = spx_end_subband - spx_start_subband;
    if (s->use_spx_default_struct) {
        memcpy(s->spx_band_struct,
               &ff_eac3_default_spx_band_struct[spx_start_subband + 1],
               s->num_spx_subbands - 1);
    } else {
        i = 0;
        for (bnd = spx_start_subband+1; bnd < spx_end_subband; bnd++, i++)
            s->spx_band_struct[i] = 0;
    }
    s->num_spx_bands = 1;
    *spx_band_sizes  = 12;
    for (bnd = 0; bnd < s->num_spx_subbands-1; bnd++) {
        if (s->spx_band_struct[bnd]) {
            *spx_band_sizes += 12;
        } else {
            s->num_spx_bands++;
            spx_band_sizes++;
            *spx_band_sizes = 12;
        }
    }
    s->bandwidth_code = (s->spx_end_freq - 73) / 3;

    /* pre-calculate blending factors */
    bin = s->spx_start_freq;
    for (bnd = 0; bnd < s->num_spx_bands; bnd++) {
        int band_size = s->spx_band_sizes[bnd];
        float blend = ((bin + (band_size / 2.0)) / s->spx_end_freq);
        for (i = 0; i < 32; i++) {
            float factor_blend = av_clipf(blend - (i / 32.0), 0.0f, 1.0f);
            s->spx_noise_blend [i][bnd] = sqrt(      factor_blend);
            s->spx_signal_blend[i][bnd] = sqrt(1.0 - factor_blend);
        }
        bin += band_size;
    }
}


static av_always_inline float calc_spx_coord(AC3EncodeContext *s,
                                             float energy_src, float energy_spx,
                                             int blend, int bnd)
{
    float coord = 1.0 / s->spx_signal_blend[blend][bnd];
    if (energy_spx > 0)
        coord = FFMIN(32.0, sqrt(energy_src / energy_spx) * coord) / 32.0;
    return coord;
}


void ff_eac3_encode_spectral_extension(AC3EncodeContext *s)
{
    int blk, blk1, ch, bnd, bin, i;
    uint8_t wrap_flags[AC3_MAX_SPX_BANDS];
    int num_copy_sections, copy_sizes[AC3_MAX_SPX_BANDS];
    float spx_energy[AC3_MAX_BLOCKS][AC3_MAX_CHANNELS][AC3_MAX_SPX_BANDS];
    float src_energy[AC3_MAX_BLOCKS][AC3_MAX_CHANNELS][AC3_MAX_SPX_BANDS];
    LOCAL_ALIGNED_16(float,   spx_coords,       [AC3_MAX_BLOCKS], [AC3_MAX_CHANNELS][32]);
    LOCAL_ALIGNED_16(int32_t, fixed_spx_coords, [AC3_MAX_BLOCKS], [AC3_MAX_CHANNELS][32]);

    ff_eac3_spx_get_copy_params(s->spx_copy_start_freq, s->spx_start_freq,
                                s->num_spx_bands, s->spx_band_sizes,
                                &num_copy_sections, copy_sizes, wrap_flags);

    /* copy coeffs from normal bands to spx bands */
    for (blk = 0; blk < s->num_blocks; blk++) {
        AC3Block *block = &s->blocks[blk];
        if (!block->spx_in_use)
            continue;
        for (ch = 1; ch <= s->fbw_channels; ch++) {
            if (!block->channel_in_spx[ch])
                continue;

            /* copy last 2 coeffs in normal range for notch filter selection */
            bin = s->spx_start_freq - 2;
            block->spx_coef[ch][bin] = block->mdct_coef[ch][bin];
            bin++;
            block->spx_coef[ch][bin] = block->mdct_coef[ch][bin];
            bin++;
            /* copy from normal range to spx range */
            for (i = 0; i < num_copy_sections; i++) {
                memcpy(&block->spx_coef[ch][bin],
                       &block->mdct_coef[ch][s->spx_copy_start_freq],
                       copy_sizes[i] * sizeof(float));
                bin += copy_sizes[i];
            }
        }
    }

    /* calculate energy for each band for original and copied coeffs */
    bnd = 0;
    bin = s->spx_start_freq;
    while (bin < s->spx_end_freq) {
        int band_size = s->spx_band_sizes[bnd];
        for (blk = 0; blk < s->num_blocks; blk++) {
            AC3Block *block = &s->blocks[blk];
            if (!block->spx_in_use)
                continue;
            for (ch = 1; ch <= s->fbw_channels; ch++) {
                float sum_x2 = 0;
                float sum_y2 = 0;
                if (!block->channel_in_spx[ch])
                    continue;
                for (i = 0; i < band_size; i++) {
                    float x = block->mdct_coef[ch][bin+i];
                    float y = block->spx_coef [ch][bin+i];
                    sum_x2 += x * x;
                    sum_y2 += y * y;
                }
                src_energy[blk][ch][bnd] = sum_x2;
                spx_energy[blk][ch][bnd] = sum_y2;
            }
        }
        bin += band_size;
        bnd++;
    }

    /* calculate blending factors */
    for (blk = 0; blk < s->num_blocks; blk++) {
        AC3Block *block = &s->blocks[blk];
        if (!block->spx_in_use)
            continue;
        for (ch = 1; ch <= s->fbw_channels; ch++) {
            int blend, best_blend;
            float blend_diff[32];

            if (!block->channel_in_spx[ch])
                continue;

            /* Note: only using 12 to 31 based on analyzed output from
                     commercial E-AC-3 files */
            best_blend = 12;
            for (blend = 12; blend < 32; blend++) {
                float src_bnd_energy = 0;
                float noise_energy   = 0;

                for (bnd = 0; bnd < s->num_spx_bands; bnd++) {
                    src_bnd_energy += src_energy[blk][ch][bnd];
                    noise_energy   += spx_energy[blk][ch][bnd] *
                                      s->spx_noise_blend[blend][bnd] *
                                      s->spx_noise_blend[blend][bnd];
                }
                src_bnd_energy = sqrt(src_bnd_energy   / (s->num_spx_subbands * 12));
                noise_energy   = sqrt(3 * noise_energy / (s->num_spx_subbands * 12));

                blend_diff[blend] = fabs(noise_energy - src_bnd_energy);
                if (blend > 12 && blend_diff[blend] < blend_diff[best_blend])
                    best_blend = blend;
            }
            block->spx_blend[ch] = best_blend;
        }
    }

    /* determine which blocks to send new spectral extension coordinates for */
    for (blk = 0; blk < s->num_blocks; blk++) {
        AC3Block *block  = &s->blocks[blk];
        AC3Block *block0 = blk ? &s->blocks[blk-1] : NULL;
        int new_coords = 0;
        float coord_diff[AC3_MAX_CHANNELS] = {0,};

        if (block->spx_in_use) {
            /* calculate spx coordinates for all blocks and calculate the
               average difference between coordinates in successive blocks */
            for (ch = 1; ch <= s->fbw_channels; ch++) {
                if (!block->channel_in_spx[ch])
                    continue;

                for (bnd = 0; bnd < s->num_spx_bands; bnd++) {
                    spx_coords[blk][ch][bnd] = calc_spx_coord(s, src_energy[blk][ch][bnd],
                                                              spx_energy[blk][ch][bnd],
                                                              block->spx_blend[ch], bnd);
                    if (blk > 0 && block0->spx_in_use &&
                        block0->channel_in_spx[ch]) {
                        coord_diff[ch] += fabs(spx_coords[blk-1][ch][bnd] -
                                               spx_coords[blk  ][ch][bnd]);
                    }
                }
                coord_diff[ch] /= s->num_spx_bands;
            }

            /* send new coordinates if this is the first block, if previous
             * block did not use spx but this block does, the channels
             * using spx has changed from the previous block, or the
             * coordinate difference from the last block for any channel is
             * greater than a threshold value. */
            if (blk == 0) {
                new_coords = 1;
            } else if (!block0->spx_in_use) {
                new_coords = 1;
            } else {
                for (ch = 1; ch <= s->fbw_channels; ch++) {
                    if (block->channel_in_spx[ch] && !block0->channel_in_spx[ch]) {
                        new_coords = 1;
                        break;
                    }
                }
                if (!new_coords) {
                    for (ch = 1; ch <= s->fbw_channels; ch++) {
                        if (block->channel_in_spx[ch] && coord_diff[ch] > 0.003) {
                            new_coords = 1;
                            break;
                        }
                    }
                }
            }
        }
        block->new_spx_coords = new_coords;
    }

    /* calculate final spx coordinates, taking into account reusing of
       coordinates in successive blocks */
    bin = s->spx_start_freq;
    for (bnd = 0; bnd < s->num_spx_bands; bnd++) {
        for (ch = 1; ch <= s->fbw_channels; ch++) {
            blk = 0;
            while (blk < s->num_blocks) {
                AC3Block *block  = &s->blocks[blk];
                int blk1, blend;
                float energy_src, energy_spx, coord;

                if (!block->spx_in_use || !block->channel_in_spx[ch]) {
                    blk++;
                    continue;
                }

                /* sum energy for all blocks in this band using this coordinate */
                energy_src = src_energy[blk][ch][bnd];
                energy_spx = spx_energy[blk][ch][bnd];
                blend = s->blocks[blk].spx_blend[ch];
                blk1 = blk+1;
                while (s->blocks[blk1].spx_in_use &&
                       !s->blocks[blk1].new_spx_coords &&
                       blk1 < s->num_blocks) {
                    energy_src += src_energy[blk1][ch][bnd];
                    energy_spx += spx_energy[blk1][ch][bnd];
                    blend = FFMAX(blend, s->blocks[blk1].spx_blend[ch]);
                    blk1++;
                }

                /* calculate coordinate */
                coord = calc_spx_coord(s, energy_src, energy_spx, blend, bnd);

                spx_coords[blk][ch][bnd] = coord;
                block->spx_blend[ch]     = blend;
                blk = blk1;
            }
        }
        bin += s->spx_band_sizes[bnd];
    }

    /* calculate exponents/mantissas for spx coordinates */
    for (blk = 0; blk < s->num_blocks; blk++) {
        AC3Block *block = &s->blocks[blk];
        if (!block->spx_in_use || !block->new_spx_coords)
            continue;

        s->dsp.vector_clipf(spx_coords[blk][1], spx_coords[blk][1],
                            COEF_MIN, COEF_MAX, s->fbw_channels * 32);
        s->ac3dsp.float_to_fixed24(fixed_spx_coords[blk][1], spx_coords[blk][1],
                                   s->fbw_channels * 32);
        s->ac3dsp.extract_exponents(block->spx_coord_exp[1],
                                    fixed_spx_coords[blk][1],
                                    s->fbw_channels * 32);

        for (ch = 1; ch <= s->fbw_channels; ch++) {
            int bnd, min_exp, max_exp, master_exp;

            if (!block->channel_in_spx)
                continue;

            /* determine master exponent */
            min_exp = max_exp = block->spx_coord_exp[ch][0];
            for (bnd = 1; bnd < s->num_spx_bands; bnd++) {
                int exp = block->spx_coord_exp[ch][bnd];
                min_exp = FFMIN(exp, min_exp);
                max_exp = FFMAX(exp, max_exp);
            }
            master_exp = ((max_exp - 15) + 2) / 3;
            master_exp = FFMAX(master_exp, 0);
            while (min_exp < master_exp * 3)
                master_exp--;
            for (bnd = 0; bnd < s->num_spx_bands; bnd++) {
                block->spx_coord_exp[ch][bnd] = av_clip(block->spx_coord_exp[ch][bnd] -
                                                        master_exp * 3, 0, 15);
            }
            block->spx_master_exp[ch] = master_exp;

            /* quantize mantissas */
            for (bnd = 0; bnd < s->num_spx_bands; bnd++) {
                int spx_exp  = block->spx_coord_exp[ch][bnd];
                int spx_mant = (fixed_spx_coords[blk][ch][bnd] << (3 + spx_exp + master_exp * 3)) >> 24;
                av_assert2(spx_mant < 8);
                if (spx_exp == 15)
                    spx_mant >>= 1;
                else
                    spx_mant -= 4;

                block->spx_coord_mant[ch][bnd] = spx_mant;
            }
        }
    }

    /* reconstruct decoded coefficients for boundary bands */
    blk1 = 0;
    for (blk = 0; blk < s->num_blocks; blk++) {
        AC3Block *block  = &s->blocks[blk];
        AC3Block *block1;
        if (!block->spx_in_use)
            continue;

        if (block->new_spx_coords)
            blk1 = blk;
        block1 = &s->blocks[blk1];

        for (ch = 1; ch <= s->fbw_channels; ch++) {
            if (!block->channel_in_spx[ch])
                continue;

            bin = s->spx_start_freq;
            for (bnd = 0; bnd < s->num_spx_bands; bnd++) {
                float *spx_coef = &block->spx_coef[ch][bin];
                int band_size   = s->spx_band_sizes[bnd];
                float coord, coord_exp, coord_mant, nblend, sblend;

                if (!wrap_flags[bnd])
                    continue;

                /* reconstruct SPX coordinate */
                coord_exp = block1->spx_coord_exp[ch][bnd];
                if (coord_exp == 15)
                    coord_mant = block1->spx_coord_mant[ch][bnd] / 4.0;
                else
                    coord_mant = (block1->spx_coord_mant[ch][bnd] + 4.0) / 8.0;
                coord = coord_mant / (1 << (block1->spx_coord_exp[ch][bnd] + 3 * block1->spx_master_exp[ch]));

                /* calculate noise and signal blending factors */
                nblend = s->spx_noise_blend [block1->spx_blend[ch]][bnd] * coord * spx_energy[blk][ch][bnd];
                sblend = s->spx_signal_blend[block1->spx_blend[ch]][bnd] * coord;

                /* reconstruct coefficients */
                for (i = 0; i < band_size; i++)
                    spx_coef[i] = spx_coef[i] * sblend + nblend * sqrt(3.0);

                bin += band_size;
            }
        }
    }

    /* calculate attenuation codes for notch filter */
    for (ch = 1; ch <= s->fbw_channels; ch++) {
        int has_spx, atten_code, best_code;
        static const float atten_tab_none[3] = { 1.0, 1.0, 1.0 };
        float atten_code_diff[33] = {0};
        float energy0[AC3_MAX_BLOCKS][AC3_MAX_SPX_BANDS];

        /* if this channel doesn't use SPX in any block, set atten code to -1 */
        has_spx = 0;
        for (blk = 0; blk < s->num_blocks; blk++) {
            AC3Block *block = &s->blocks[blk];
            if (block->spx_in_use && block->channel_in_spx[ch]) {
                has_spx = 1;
                break;
            }
        }
        if (!has_spx) {
            s->spx_atten_code[ch] = -1;
            continue;
        }

        /* calculate source energy across boundaries */
        for (blk = 0; blk < s->num_blocks; blk++) {
            AC3Block *block = &s->blocks[blk];

            if (!block->spx_in_use || !block->channel_in_spx[ch])
                continue;

            bin = s->spx_start_freq - 2;
            for (bnd = 0; bnd < s->num_spx_bands; bnd++) {
                if (wrap_flags[bnd]) {
                    float *src_coef = &block->mdct_coef[ch][bin];
                    energy0[blk][bnd] = 0;
                    for (i = 0; i < 5; i++)
                        energy0[blk][bnd] += src_coef[i] *  src_coef[i];
                    energy0[blk][bnd] = sqrt(energy0[blk][bnd] / 5);
                }
                bin += s->spx_band_sizes[bnd];
            }
        }

        /* search for the atten code that minimizes energy difference between
           source and reconstructed coeffs across the filter boundaries */
        best_code = 0;
        for (atten_code = 0; atten_code < 33; atten_code++) {
            const float *atten_tab = atten_code ?
                                     ff_eac3_spx_atten_tab[atten_code-1] :
                                     atten_tab_none;

            for (blk = 0; blk < s->num_blocks; blk++) {
                AC3Block *block = &s->blocks[blk];

                if (!block->spx_in_use || !block->channel_in_spx[ch])
                    continue;

                bin = s->spx_start_freq - 2;
                for (bnd = 0; bnd < s->num_spx_bands; bnd++) {
                    if (wrap_flags[bnd]) {
                        float *spx_coef = &block->spx_coef[ch][bin];
                        float test_coef[5];
                        float energy1;

                        test_coef[0] = spx_coef[0] * atten_tab[0];
                        test_coef[1] = spx_coef[1] * atten_tab[1];
                        test_coef[2] = spx_coef[2] * atten_tab[2];
                        test_coef[3] = spx_coef[3] * atten_tab[1];
                        test_coef[4] = spx_coef[4] * atten_tab[0];

                        /* maintain energy level across the boundary */
                        energy1 = 0;
                        for (i = 0; i < 5; i++)
                            energy1 += test_coef[i] * test_coef[i];
                        energy1 = sqrt(energy1 / 5);
                        atten_code_diff[atten_code] += fabs(energy0[blk][bnd] - energy1);
                    }
                    bin += s->spx_band_sizes[bnd];
                }
            }

            if (atten_code > 0 && atten_code_diff[atten_code] < atten_code_diff[best_code])
                best_code = atten_code;
        }
        s->spx_atten_code[ch] = best_code - 1;
    }
}


void ff_eac3_set_states(AC3EncodeContext *s)
{
    int ch, blk;
    int first_cpl_coords[AC3_MAX_CHANNELS];
    int first_spx_coords[AC3_MAX_CHANNELS];

    /* set first cpl coords */
    for (ch = 1; ch <= s->fbw_channels; ch++)
        first_cpl_coords[ch] = 1;
    for (blk = 0; blk < s->num_blocks; blk++) {
        AC3Block *block = &s->blocks[blk];
        if (block->new_cpl_coords) {
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
    }

    /* set first cpl leak */
    for (blk = 0; blk < s->num_blocks; blk++) {
        AC3Block *block = &s->blocks[blk];
        if (block->cpl_in_use) {
            block->new_cpl_leak = 2;
            break;
        }
    }

    /* set first spx coords */
    for (ch = 1; ch <= s->fbw_channels; ch++)
        first_spx_coords[ch] = 1;
    for (blk = 0; blk < s->num_blocks; blk++) {
        AC3Block *block = &s->blocks[blk];
        if (block->new_spx_coords) {
            for (ch = 1; ch <= s->fbw_channels; ch++) {
                if (block->channel_in_spx[ch]) {
                    if (first_spx_coords[ch]) {
                        block->new_spx_coords = 2;
                        first_spx_coords[ch]  = 0;
                    }
                } else {
                    first_spx_coords[ch] = 1;
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
    put_bits(&s->pb, 1, 0);                         /* aht enabled = no */
    }
    put_bits(&s->pb, 2, 0);                         /* snr offset strategy = 1 */
    put_bits(&s->pb, 1, 0);                         /* transient pre-noise processing enabled = no */
    put_bits(&s->pb, 1, 0);                         /* block switch syntax enabled = no */
    put_bits(&s->pb, 1, 0);                         /* dither flag syntax enabled = no */
    put_bits(&s->pb, 1, 0);                         /* bit allocation model syntax enabled = no */
    put_bits(&s->pb, 1, 0);                         /* fast gain codes enabled = no */
    put_bits(&s->pb, 1, 0);                         /* dba syntax enabled = no */
    put_bits(&s->pb, 1, 0);                         /* skip field syntax enabled = no */
    put_bits(&s->pb, 1, s->spx_on);                 /* spx attenuation data present */
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
    /* snr offsets */
    put_bits(&s->pb, 6, s->coarse_snr_offset);
    put_bits(&s->pb, 4, s->fine_snr_offset[1]);
    /* spx attenuation info */
    if (s->spx_on) {
        for (ch = 1; ch <= s->fbw_channels; ch++) {
            if (s->spx_atten_code[ch] < 0) {
                put_bits(&s->pb, 1, 0);
            } else {
                put_bits(&s->pb, 1, 1);
                put_bits(&s->pb, 5, s->spx_atten_code[ch]);
            }
        }
    }
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
