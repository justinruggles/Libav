/*
 * samplerate conversion for both audio and video
 * Copyright (c) 2000 Fabrice Bellard
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
 * samplerate conversion for both audio and video
 */

#include "avcodec.h"
#include "audioconvert.h"
#include "libavutil/opt.h"
#include "libavutil/samplefmt.h"

#define MAX_CHANNELS 8

struct AVResampleContext;

static const char *context_to_name(void *ptr)
{
    return "audioresample";
}

static const AVOption options[] = {{NULL}};
static const AVClass audioresample_context_class = {
    "ReSampleContext", context_to_name, options, LIBAVUTIL_VERSION_INT
};

struct ReSampleContext {
    struct AVResampleContext *resample_context;
    short *temp[MAX_CHANNELS];
    int temp_len;
    int16_t *r_in_buf[MAX_CHANNELS];
    int r_in_buf_size;
    float ratio;
    /* channel convert */
    int input_channels, output_channels, filter_channels;
    AVAudioConvert *convert_ctx[2];
    enum AVSampleFormat sample_fmt[2]; ///< input and output sample format
    unsigned sample_size[2];           ///< size of one sample in sample_fmt
    short *buffer[2];                  ///< buffers used for conversion to S16
    unsigned buffer_size[2];           ///< sizes of allocated buffers
};

static void stereo_to_mono(int16_t *output, int16_t **input, int n)
{
    int i;
    for (i = 0; i < n; i++)
        output[i] = (input[0][i] + input[1][i]) >> 1;
}

static void mono_to_stereo(int16_t **output, int16_t *input, int n)
{
    int i;
    for (i = 0; i < n; i++) {
        int16_t v = input[i];
        output[0][i] = v;
        output[1][i] = v;
    }
}

static void stereo_to_5p1(int16_t **output, int16_t **input, int n)
{
    int i;
    for (i = 0; i < n; i++) {
        int l = input[0][i];
        int r = input[1][i];
        output[0][i] = l;                   /* left */
        output[1][i] = r;                   /* right */
        output[2][i] = (l / 2) + (r / 2);   /* center */
        output[3][i] = 0;                   /* low freq */
        output[4][i] = 0;                   /* left surround */
        output[5][i] = 0;                   /* right surroud */
    }
}

ReSampleContext *av_audio_resample_init(int output_channels, int input_channels,
                                        int output_rate, int input_rate,
                                        enum AVSampleFormat sample_fmt_out,
                                        enum AVSampleFormat sample_fmt_in,
                                        int filter_length, int log2_phase_count,
                                        int linear, double cutoff)
{
    ReSampleContext *s;

    if (input_channels > MAX_CHANNELS) {
        av_log(NULL, AV_LOG_ERROR,
               "Resampling with input channels greater than %d is unsupported.\n",
               MAX_CHANNELS);
        return NULL;
    }
    if (output_channels != input_channels &&
        (input_channels  > 2 ||
         output_channels > 2 &&
         !(output_channels == 6 && input_channels == 2))) {
        av_log(NULL, AV_LOG_ERROR,
               "Resampling output channel count must be 1 or 2 for mono input; 1, 2 or 6 for stereo input; or N for N channel input.\n");
        return NULL;
    }

    s = av_mallocz(sizeof(ReSampleContext));
    if (!s) {
        av_log(NULL, AV_LOG_ERROR, "Can't allocate memory for resample context.\n");
        return NULL;
    }

    s->ratio = (float)output_rate / (float)input_rate;

    s->input_channels = input_channels;
    s->output_channels = output_channels;

    s->filter_channels = s->input_channels;
    if (s->output_channels < s->filter_channels)
        s->filter_channels = s->output_channels;

    /* treat mono as planar */
    s->sample_fmt[0]  = sample_fmt_in;
    s->sample_fmt[1]  = sample_fmt_out;
    if (s->input_channels == 1)
        s->sample_fmt[0] = av_get_alt_sample_fmt(s->sample_fmt[0], 1);
    if (s->output_channels == 1)
        s->sample_fmt[1] = av_get_alt_sample_fmt(s->sample_fmt[1], 1);

    s->sample_size[0] = av_get_bytes_per_sample(s->sample_fmt[0]);
    s->sample_size[1] = av_get_bytes_per_sample(s->sample_fmt[1]);

    if (s->sample_fmt[0] != AV_SAMPLE_FMT_S16P) {
        if (!(s->convert_ctx[0] = av_audio_convert_alloc(AV_SAMPLE_FMT_S16P,
                                                         s->sample_fmt[0],
                                                         s->input_channels))) {
            av_log(s, AV_LOG_ERROR,
                   "Cannot convert %s sample format to s16 sample format\n",
                   av_get_sample_fmt_name(s->sample_fmt[0]));
            av_free(s);
            return NULL;
        }
    }

    if (s->sample_fmt[1] != AV_SAMPLE_FMT_S16P) {
        if (!(s->convert_ctx[1] = av_audio_convert_alloc(s->sample_fmt[1],
                                                         AV_SAMPLE_FMT_S16P,
                                                         s->output_channels))) {
            av_log(s, AV_LOG_ERROR,
                   "Cannot convert s16 sample format to %s sample format\n",
                   av_get_sample_fmt_name(s->sample_fmt[1]));
            av_audio_convert_free(s->convert_ctx[0]);
            av_free(s);
            return NULL;
        }
    }

    s->resample_context = av_resample_init(output_rate, input_rate,
                                           filter_length, log2_phase_count,
                                           linear, cutoff);

    *(const AVClass**)s->resample_context = &audioresample_context_class;

    return s;
}

/* resample audio. 'nb_samples' is the number of input samples */
/* XXX: optimize it ! */
int audio_resample(ReSampleContext *s, void **output, void **input, int in_samples)
{
    int i, nb_samples1;
    int16_t *s16p_buf[MAX_CHANNELS];
    int16_t *r_out_buf[MAX_CHANNELS];
    int out_samples;

    /* if converting input to s16p or downmixing, realloc the input buffer */
    if (s->sample_fmt[0] != AV_SAMPLE_FMT_S16P || s->filter_channels < s->input_channels) {
        unsigned input_size = in_samples * s->input_channels * 2;

        if (!s->buffer_size[0] || s->buffer_size[0] < input_size) {
            av_free(s->buffer[0]);
            s->buffer_size[0] = input_size;
            s->buffer[0] = av_malloc(s->buffer_size[0]);
            if (!s->buffer[0]) {
                av_log(s->resample_context, AV_LOG_ERROR, "Could not allocate buffer\n");
                return 0;
            }
        }

        for (i = 0; i < s->input_channels; i++)
            s16p_buf[i] = s->buffer[0] + i * in_samples;
    } else {
        for (i = 0; i < s->input_channels; i++)
            s16p_buf[i] = input[i];
    }

    /* convert to s16p if needed */
    if (s->sample_fmt[0] != AV_SAMPLE_FMT_S16P) {
        if (av_audio_convert(s->convert_ctx[0], (void *const *)s16p_buf,
                             (const void *const *)input, in_samples) < 0) {
            av_log(s->resample_context, AV_LOG_ERROR,
                   "Audio sample format conversion failed\n");
            return 0;
        }
    }

    /* downmix if needed */
    if (s->filter_channels < s->input_channels) {
        /* only downmixing currently supported is stereo to mono */
        stereo_to_mono(s16p_buf[0], s16p_buf, in_samples);
    }

    /* number of output samples to allocate */
    out_samples = 4 * in_samples * s->ratio + 16;

    /* if converting from s16p to output, realloc the output buffer */
    if (s->sample_fmt[1] != AV_SAMPLE_FMT_S16P) {
        int out_size = out_samples * av_get_bytes_per_sample(s->sample_fmt[1]) *
                       s->output_channels;

        if (!s->buffer_size[1] || s->buffer_size[1] < out_size) {
            av_free(s->buffer[1]);
            s->buffer_size[1] = out_size;
            s->buffer[1] = av_mallocz(s->buffer_size[1]);
            if (!s->buffer[1]) {
                av_log(s->resample_context, AV_LOG_ERROR, "Could not allocate buffer\n");
                return 0;
            }
        }

        for (i = 0; i < s->output_channels; i++)
            r_out_buf[i] = s->buffer[1] + i * out_samples;
    } else {
        for (i = 0; i < s->output_channels; i++)
            r_out_buf[i] = output[i];
    }

    /* reallocate the resample input buffers if needed */
    if (s->r_in_buf_size < (in_samples + s->temp_len) * sizeof(int16_t)) {
        int r_in_buf_size = (in_samples + s->temp_len) * sizeof(int16_t);
        for (i = 0; i < s->filter_channels; i++) {
            int16_t *tmp = av_realloc(s->r_in_buf[i], r_in_buf_size);
            if (!tmp)
                return AVERROR(ENOMEM);
            s->r_in_buf[i] = tmp;
        }
        s->r_in_buf_size = r_in_buf_size;
    }

    /* copy samples from s->temp and input to r_in_buf */
    for (i = 0; i < s->filter_channels; i++) {
        memcpy(s->r_in_buf[i],                s->temp[i], s->temp_len * sizeof(int16_t));
        memcpy(s->r_in_buf[i] + s->temp_len, s16p_buf[i], in_samples  * sizeof(int16_t));
    }
    in_samples += s->temp_len;

    /* resample each channel */
    nb_samples1 = 0; /* avoid warning */
    for (i = 0; i < s->filter_channels; i++) {
        int consumed;
        int is_last = i + 1 == s->filter_channels;

        nb_samples1 = av_resample(s->resample_context, r_out_buf[i],
                                  s->r_in_buf[i], &consumed, in_samples,
                                  out_samples, is_last);
        s->temp_len = in_samples - consumed;
        s->temp[i] = av_realloc(s->temp[i], s->temp_len * sizeof(short));
        memcpy(s->temp[i], s->r_in_buf[i] + consumed, s->temp_len * sizeof(int16_t));
    }
    if (nb_samples1 > 0)
        out_samples = nb_samples1;

    /* upmix if needed */
    if (s->output_channels > s->filter_channels) {
        if (s->output_channels == 2 && s->filter_channels == 1)
            mono_to_stereo(r_out_buf, r_out_buf[0], out_samples);
        else if (s->output_channels == 6 && s->filter_channels == 2)
            stereo_to_5p1(r_out_buf, r_out_buf, out_samples);
    }

    /* convert from s16p to output format if needed */
    if (s->sample_fmt[1] != AV_SAMPLE_FMT_S16P) {
        if (av_audio_convert(s->convert_ctx[1], output,
                             (const void *const *)r_out_buf, out_samples) < 0) {
            av_log(s->resample_context, AV_LOG_ERROR,
                   "Audio sample format convertion failed\n");
            return 0;
        }
    }

    return out_samples;
}

void audio_resample_close(ReSampleContext *s)
{
    int i;
    av_resample_close(s->resample_context);
    for (i = 0; i < s->filter_channels; i++) {
        av_freep(&s->temp[i]);
        av_freep(&s->r_in_buf[i]);
    }
    av_freep(&s->buffer[0]);
    av_freep(&s->buffer[1]);
    av_audio_convert_free(s->convert_ctx[0]);
    av_audio_convert_free(s->convert_ctx[1]);
    av_free(s);
}
