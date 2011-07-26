/*
 * Copyright (c) 2011 Justin Ruggles
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

#include <stdint.h>

#include "avcodec.h"
#include "eac3.h"
#include "eac3_data.h"


void ff_eac3_spx_get_copy_params(int copy_start, int spx_start,
                                 int num_spx_bands, uint8_t *spx_band_sizes,
                                 int *num_copy_sections, int *copy_sizes,
                                 uint8_t *wrap_flags)
{
    int bin, bnd, i;
    int sections;

    memset(wrap_flags, 0, num_spx_bands * sizeof(*wrap_flags));
    wrap_flags[0] = 1;
    bin = copy_start;
    sections = 0;
    for (bnd = 0; bnd < num_spx_bands; bnd++) {
        int copysize;
        int bandsize = spx_band_sizes[bnd];
        if (bin + bandsize > spx_start) {
            copy_sizes[sections++] = bin - copy_start;
            bin = copy_start;
            wrap_flags[bnd] = 1;
        }
        for (i = 0; i < bandsize; i += copysize) {
            if (bin == spx_start) {
                copy_sizes[sections++] = bin - copy_start;
                bin = copy_start;
            }
            copysize = FFMIN(bandsize - i, spx_start - bin);
            bin += copysize;
        }
    }
    copy_sizes[sections++] = bin - copy_start;
    *num_copy_sections = sections;
}
