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

#ifndef AVCODEC_EAC3ENC_H
#define AVCODEC_EAC3ENC_H

#include "ac3enc.h"

/**
 * Initialize E-AC-3 exponent tables.
 */
void ff_eac3_exponent_init(void);

/**
 * Determine frame exponent strategy use and indices.
 */
void ff_eac3_get_frame_exp_strategy(AC3EncodeContext *s);

void ff_eac3_compute_aht_strategy(AC3EncodeContext *s);

/**
 * Set coupling states.
 * This determines whether certain flags must be written to the bitstream or
 * whether they will be implicitly already known by the decoder.
 */
void ff_eac3_set_cpl_states(AC3EncodeContext *s);

void ff_eac3_aht_processing(AC3EncodeContext *s);

int ff_eac3_aht_gaq_analysis_ch(const int32_t *mant, const uint8_t *bap,
                                uint8_t *gaq_mode, uint8_t *gaq_gain,
                                uint8_t *large_mantissa,
                                int start_freq, int end_freq);

void ff_eac3_aht_quantize_mantissas_ch(const int32_t *mant, const uint8_t *bap,
                                       int16_t *qmant, uint8_t gaq_mode,
                                       const uint8_t *gaq_gain,
                                       const uint8_t *large_mantissa,
                                       uint8_t *encoded_gaq_gain,
                                       int start_freq, int end_freq);

/**
 * Write the E-AC-3 frame header to the output bitstream.
 */
void ff_eac3_output_frame_header(AC3EncodeContext *s);

#endif /* AVCODEC_EAC3ENC_H */
