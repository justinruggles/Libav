/*
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
 * E-AC-3 encoder/decoder common code.
 */

#ifndef AVCODEC_EAC3_H
#define AVCODEC_EAC3_H

#include <stdint.h>

/**
 * Initialize spectral extension strategy parameters.
 */
void ff_eac3_spx_get_copy_params(int copy_start, int spx_start,
                                 int num_spx_bands, uint8_t *spx_band_sizes,
                                 int *num_copy_sections, int *copy_sizes,
                                 uint8_t *wrap_flags);

#endif /* AVCODEC_EAC3_H */
