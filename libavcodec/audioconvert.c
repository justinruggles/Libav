/*
 * audio conversion
 * Copyright (c) 2006 Michael Niedermayer <michaelni@gmx.at>
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
 * audio conversion
 * @author Michael Niedermayer <michaelni@gmx.at>
 */

#include "libavutil/avstring.h"
#include "libavutil/libm.h"
#include "libavutil/samplefmt.h"
#include "avcodec.h"
#include "audioconvert.h"

uint64_t avcodec_guess_channel_layout(int nb_channels, enum CodecID codec_id, const char *fmt_name)
{
    switch(nb_channels) {
    case 1: return AV_CH_LAYOUT_MONO;
    case 2: return AV_CH_LAYOUT_STEREO;
    case 3: return AV_CH_LAYOUT_SURROUND;
    case 4: return AV_CH_LAYOUT_QUAD;
    case 5: return AV_CH_LAYOUT_5POINT0;
    case 6: return AV_CH_LAYOUT_5POINT1;
    case 8: return AV_CH_LAYOUT_7POINT1;
    default: return 0;
    }
}

struct AVAudioConvert {
    int channels;
    int fmt_pair;
    enum AVSampleFormat out_fmt;
    int out_planar;
    int out_stride;
    int out_offset;
    enum AVSampleFormat in_fmt;
    int in_planar;
    int in_stride;
    int in_offset;
};

AVAudioConvert *av_audio_convert_alloc(enum AVSampleFormat out_fmt,
                                       enum AVSampleFormat  in_fmt,
                                       int channels)
{
    AVAudioConvert *ctx;
    int isize, osize;
    ctx = av_malloc(sizeof(AVAudioConvert));
    if (!ctx)
        return NULL;
    ctx->channels = channels;
    ctx->fmt_pair   = av_get_alt_sample_fmt(out_fmt, 0) +
                      av_get_alt_sample_fmt( in_fmt, 0) * AV_SAMPLE_FMT_NB;
    ctx->out_fmt    = out_fmt;
    ctx->in_fmt     = in_fmt;
    ctx->out_planar = av_sample_fmt_is_planar(out_fmt);
    ctx->in_planar  = av_sample_fmt_is_planar(in_fmt);
    osize           = av_get_bytes_per_sample(ctx->out_fmt);
    isize           = av_get_bytes_per_sample(ctx->in_fmt);
    ctx->out_stride = osize * (ctx->out_planar ? 1 : ctx->channels);
    ctx->in_stride  = isize * (ctx->in_planar  ? 1 : ctx->channels);
    ctx->out_offset = !ctx->out_planar * osize;
    ctx->in_offset  = !ctx->in_planar  * isize;
    return ctx;
}

void av_audio_convert_free(AVAudioConvert *ctx)
{
    av_free(ctx);
}

int av_audio_convert(AVAudioConvert *ctx,
                           void * const *out,
                     const void * const  *in, int nb_samples)
{
    int ch;

    //FIXME optimize common cases

    const int is         = ctx->in_stride;
    const int os         = ctx->out_stride;
    const int in_offset  = ctx->in_offset;
    const int out_offset = ctx->out_offset;

    for (ch = 0; ch < ctx->channels; ch++) {
        const uint8_t *pi =  in[ctx->in_planar  * ch];
        uint8_t *po       = out[ctx->out_planar * ch];
        uint8_t *end;

        pi += ch * in_offset;
        po += ch * out_offset;
        end = po + os * nb_samples;

#define CONV(ofmt, otype, ifmt, expr)\
if(ctx->fmt_pair == ofmt + AV_SAMPLE_FMT_NB*ifmt){\
    do{\
        *(otype*)po = expr; pi += is; po += os;\
    }while(po < end);\
}

//FIXME put things below under ifdefs so we do not waste space for cases no codec will need
//FIXME rounding ?

             CONV(AV_SAMPLE_FMT_U8 , uint8_t, AV_SAMPLE_FMT_U8 ,  *(const uint8_t*)pi)
        else CONV(AV_SAMPLE_FMT_S16, int16_t, AV_SAMPLE_FMT_U8 , (*(const uint8_t*)pi - 0x80)<<8)
        else CONV(AV_SAMPLE_FMT_S32, int32_t, AV_SAMPLE_FMT_U8 , (*(const uint8_t*)pi - 0x80)<<24)
        else CONV(AV_SAMPLE_FMT_FLT, float  , AV_SAMPLE_FMT_U8 , (*(const uint8_t*)pi - 0x80)*(1.0 / (1<<7)))
        else CONV(AV_SAMPLE_FMT_DBL, double , AV_SAMPLE_FMT_U8 , (*(const uint8_t*)pi - 0x80)*(1.0 / (1<<7)))
        else CONV(AV_SAMPLE_FMT_U8 , uint8_t, AV_SAMPLE_FMT_S16, (*(const int16_t*)pi>>8) + 0x80)
        else CONV(AV_SAMPLE_FMT_S16, int16_t, AV_SAMPLE_FMT_S16,  *(const int16_t*)pi)
        else CONV(AV_SAMPLE_FMT_S32, int32_t, AV_SAMPLE_FMT_S16,  *(const int16_t*)pi<<16)
        else CONV(AV_SAMPLE_FMT_FLT, float  , AV_SAMPLE_FMT_S16,  *(const int16_t*)pi*(1.0 / (1<<15)))
        else CONV(AV_SAMPLE_FMT_DBL, double , AV_SAMPLE_FMT_S16,  *(const int16_t*)pi*(1.0 / (1<<15)))
        else CONV(AV_SAMPLE_FMT_U8 , uint8_t, AV_SAMPLE_FMT_S32, (*(const int32_t*)pi>>24) + 0x80)
        else CONV(AV_SAMPLE_FMT_S16, int16_t, AV_SAMPLE_FMT_S32,  *(const int32_t*)pi>>16)
        else CONV(AV_SAMPLE_FMT_S32, int32_t, AV_SAMPLE_FMT_S32,  *(const int32_t*)pi)
        else CONV(AV_SAMPLE_FMT_FLT, float  , AV_SAMPLE_FMT_S32,  *(const int32_t*)pi*(1.0 / (1U<<31)))
        else CONV(AV_SAMPLE_FMT_DBL, double , AV_SAMPLE_FMT_S32,  *(const int32_t*)pi*(1.0 / (1U<<31)))
        else CONV(AV_SAMPLE_FMT_U8 , uint8_t, AV_SAMPLE_FMT_FLT, av_clip_uint8(  lrintf(*(const float*)pi * (1<<7)) + 0x80))
        else CONV(AV_SAMPLE_FMT_S16, int16_t, AV_SAMPLE_FMT_FLT, av_clip_int16(  lrintf(*(const float*)pi * (1<<15))))
        else CONV(AV_SAMPLE_FMT_S32, int32_t, AV_SAMPLE_FMT_FLT, av_clipl_int32(llrintf(*(const float*)pi * (1U<<31))))
        else CONV(AV_SAMPLE_FMT_FLT, float  , AV_SAMPLE_FMT_FLT, *(const float*)pi)
        else CONV(AV_SAMPLE_FMT_DBL, double , AV_SAMPLE_FMT_FLT, *(const float*)pi)
        else CONV(AV_SAMPLE_FMT_U8 , uint8_t, AV_SAMPLE_FMT_DBL, av_clip_uint8(  lrint(*(const double*)pi * (1<<7)) + 0x80))
        else CONV(AV_SAMPLE_FMT_S16, int16_t, AV_SAMPLE_FMT_DBL, av_clip_int16(  lrint(*(const double*)pi * (1<<15))))
        else CONV(AV_SAMPLE_FMT_S32, int32_t, AV_SAMPLE_FMT_DBL, av_clipl_int32(llrint(*(const double*)pi * (1U<<31))))
        else CONV(AV_SAMPLE_FMT_FLT, float  , AV_SAMPLE_FMT_DBL, *(const double*)pi)
        else CONV(AV_SAMPLE_FMT_DBL, double , AV_SAMPLE_FMT_DBL, *(const double*)pi)
        else return -1;
    }
    return 0;
}
