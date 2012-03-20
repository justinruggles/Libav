/*
 * Audio FIFO
 * Copyright (c) 2012 Justin Ruggles <justin.ruggles@gmail.com>
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
 * Audio FIFO
 * @author Justin Ruggles <justin.ruggles@gmail.com>
 */

#ifndef AVUTIL_AUDIO_FIFO_H
#define AVUTIL_AUDIO_FIFO_H

#include "avutil.h"
#include "fifo.h"
#include "samplefmt.h"

/**
 * FIFO that operates at the sample level rather than the byte level.
 * It supports both planar and packed sample formats.
 */
typedef struct AVAudioFifo {
    AVFifoBuffer **buf;             /**< single buffer for interleaved, per-channel buffers for planar */
    int nb_buffers;                 /**< number of buffers */
    int nb_samples;                 /**< number of samples currently in the FIFO */
    int allocated_samples;          /**< current allocated size, in samples */

    int channels;                   /**< number of channels */
    enum AVSampleFormat sample_fmt; /**< sample format */
    int planar;                     /**< is the sample format planar */
    int sample_size;                /**< size, in bytes, of one sample in a buffer */
} AVAudioFifo;

void av_audio_fifo_free(AVAudioFifo *af);

AVAudioFifo *av_audio_fifo_alloc(enum AVSampleFormat sample_fmt, int channels,
                                 int nb_samples);

int av_audio_fifo_realloc(AVAudioFifo *af, int nb_samples);

int av_audio_fifo_write(AVAudioFifo *af, void **data, int nb_samples);

int av_audio_fifo_read(AVAudioFifo *af, void **data, int nb_samples);

int av_audio_fifo_drain(AVAudioFifo *af, int nb_samples);

void av_audio_fifo_reset(AVAudioFifo *af);

int av_audio_fifo_size(AVAudioFifo *af);

int av_audio_fifo_space(AVAudioFifo *af);

#endif /* AVUTIL_AUDIO_FIFO_H */
