/*
 * Copyright (C) 2011  Justin Ruggles
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
 * @file Opus decoder using libopus
 *
 * references:
 * Opus Codec           [http://opus-codec.org/]
 * Ogg mapping for Opus [http://wiki.xiph.org/OggOpus/]
 * IETF Codec WG        [http://tools.ietf.org/wg/codec/]
 *
 * TODO: multi-channel
 *           - correctly parse self-delimiting packets in ogg/opus parser
 *           - read/validate channel mapping in the Id header
 *           - proper channel reordering
 *               - can probably be done by manipulating the channel map that is
 *                 sent to the libopus multistream decoder
 */

#include <opus/opus_multistream.h>

#include "libavutil/mathematics.h"
#include "libavutil/opt.h"
#include "avcodec.h"
#include "bytestream.h"
#include "vorbis.h"

typedef struct {
    AVClass *class;
    OpusMSDecoder *st;
    int request_sample_rate;

    int src_channels;
    int src_sample_rate;
    int streams;
    int coupled_streams;
    uint8_t channel_map[2];
} LibOpusDecContext;

static int opus_error_to_averror(int err)
{
    switch (err) {
    case OPUS_OK:             return 0;
    case OPUS_BAD_ARG:        return AVERROR(EINVAL);
    case OPUS_INVALID_PACKET: return AVERROR_INVALIDDATA;
    case OPUS_ALLOC_FAIL:     return AVERROR(ENOMEM);
    default:                  return -1;
    }
}

#if 0
/**
 * Parse channel mapping data from the Id header.
 *
 * @param s    decoder context
 * @param data Id header data
 * @param size Id header size
 * @return 0 = use provided mapping, 1 = use default
 */
static av_cold int parse_channel_mapping(LibOpusDecContext *s, uint8_t *data,
                                         int size)
{
    uint8_t *p = data + 18;
    size -= 18;

    s->channel_mapping = bytestream_get_byte(&p);
    size--;
    if (s->channel_mapping) {
        int map_size;
        if (s->channel_mapping == 1 && s->src_channels > 8)
            return 1;
        map_size = 1 + 1 + s->src_channels;
        if (size >= map_size) {
            s->streams         = bytestream_get_byte(&p);
            s->coupled_streams = bytestream_get_byte(&p);
            if (s->streams  + s->coupled_streams <= 255 &&
                s->streams >= s->coupled_streams) {
                memcpy(s->channel_map, p, s->src_channels);
                return 0;
            }
        }
    }
    return 1;
}
#endif

static av_cold int parse_id_header(LibOpusDecContext *s, uint8_t *data, int size)
{
    const uint8_t *p = data;

    if (memcmp(p, "OpusHead", 8))
        return AVERROR_INVALIDDATA;
    p += 8;
    if (bytestream_get_byte(&p) != 0)
        return AVERROR_INVALIDDATA;
    s->src_channels = bytestream_get_byte(&p);
    if (!s->src_channels)
        return AVERROR_INVALIDDATA;
    if (s->src_channels > 2) {
        av_log(s, AV_LOG_ERROR, "Unsupported number of channels: %d\n",
               s->src_channels);
        return AVERROR_PATCHWELCOME;
    }
    p += 2; // pre-skip
    s->src_sample_rate = bytestream_get_le32(&p);
    if (s->src_sample_rate <= 0)
        return AVERROR_INVALIDDATA;
    p += 2; // output gain

    /* TODO: parse channel mapping info */
    s->streams         = 1;
    s->coupled_streams = (s->src_channels == 2);
    s->channel_map[0]  = 0;
    s->channel_map[1]  = 1;

    return 0;
}

static av_cold int libopus_decode_init(AVCodecContext *avctx)
{
    LibOpusDecContext *s = avctx->priv_data;
    int ret;

    av_log(avctx, AV_LOG_DEBUG, "%s\n", opus_get_version_string());

    /* parse the Id header from extradata */
    if (avctx->extradata_size >= 19) {
        if (parse_id_header(s, avctx->extradata, avctx->extradata_size) < 0) {
            av_log(s, AV_LOG_ERROR, "Error parsing Opus extradata\n");
            return AVERROR_INVALIDDATA;
        }
    } else {
        av_log(s, AV_LOG_WARNING, "missing or incomplete Opus extradata. "
               "using default parameters instead.\n");
        s->src_channels    = 2;
        s->src_sample_rate = 48000;
        s->streams         = 1;
        s->coupled_streams = (s->src_channels == 2);
        s->channel_map[0]  = 0;
        s->channel_map[1]  = 1;
    }

    avctx->sample_fmt = AV_SAMPLE_FMT_FLT;

    /* handle requested sample rate */
    if (s->request_sample_rate) {
        if (s->request_sample_rate < 0)
            s->request_sample_rate = s->src_sample_rate;
    } else {
        s->request_sample_rate = 48000;
    }
    if (s->request_sample_rate <= 8000)
        avctx->sample_rate = 8000;
    else if (s->request_sample_rate <= 12000)
        avctx->sample_rate = 12000;
    else if (s->request_sample_rate <= 16000)
        avctx->sample_rate = 16000;
    else if (s->request_sample_rate <= 24000)
        avctx->sample_rate = 24000;
    else
        avctx->sample_rate = 48000;
    if (avctx->sample_rate != s->request_sample_rate)
        av_log(avctx, AV_LOG_INFO, "libopus cannot output at the requested "
               "sample rate. using %d instead.\n", avctx->sample_rate);

    /* handle requested channel layout */
    avctx->channels = s->src_channels;
    if (avctx->request_channel_layout == AV_CH_LAYOUT_MONO)
        avctx->channels = 1;
    else if (avctx->request_channel_layout == AV_CH_LAYOUT_STEREO)
        avctx->channels = 2;
#if FF_API_REQUEST_CHANNELS
    else if (avctx->request_channels == 1 || avctx->request_channels == 2)
        avctx->channels = avctx->request_channels;
#endif
    avctx->channel_layout = ff_vorbis_channel_layouts[avctx->channels - 1];

    /* initialize libopus */
    ret = opus_multistream_decoder_get_size(s->streams, s->coupled_streams);
    if (!ret) {
        av_log(avctx, AV_LOG_ERROR, "%s\n", opus_strerror(ret));
        return opus_error_to_averror(ret);
    }
    s->st = av_mallocz(ret);
    if (!s->st)
        return AVERROR(ENOMEM);
    ret = opus_multistream_decoder_init(s->st, avctx->sample_rate,
                                        avctx->channels, s->streams,
                                        s->coupled_streams, s->channel_map);
    if (ret != OPUS_OK) {
        av_log(avctx, AV_LOG_ERROR, "%s\n", opus_strerror(ret));
        return opus_error_to_averror(ret);
    }

    return 0;
}

static av_cold int libopus_decode_close(AVCodecContext *avctx)
{
    LibOpusDecContext *s = avctx->priv_data;
    av_freep(&s->st);
    return 0;
}

static int libopus_decode_frame(AVCodecContext *avctx,
                                void *data, int *data_size,
                                AVPacket *avpkt)
{
    LibOpusDecContext *s = avctx->priv_data;
    OpusMSDecoder *st = s->st;
    int sample_size, nb_samples;

    nb_samples  = opus_packet_get_nb_frames(avpkt->data, avpkt->size) *
                  opus_packet_get_samples_per_frame(avpkt->data, 48000);
    sample_size = avctx->channels * av_get_bytes_per_sample(avctx->sample_fmt);
    if (*data_size / sample_size < nb_samples) {
        av_log(avctx, AV_LOG_ERROR, "output buffer is too small\n");
        return AVERROR(EINVAL);
    }

    nb_samples = opus_multistream_decode_float(st, avpkt->data, avpkt->size,
                                               data, nb_samples, 0);
    if (nb_samples < 0) {
        av_log(avctx, AV_LOG_ERROR, "%s\n", opus_strerror(nb_samples));
        return opus_error_to_averror(nb_samples);
    }

    *data_size = nb_samples * sample_size;
    return avpkt->size;
}

static av_cold void libopus_decode_flush(AVCodecContext *avctx)
{
    LibOpusDecContext *s = avctx->priv_data;
    opus_multistream_decoder_ctl(s->st, OPUS_RESET_STATE);
}

#define OFFSET(x) offsetof(LibOpusDecContext, x)
#define AD AV_OPT_FLAG_AUDIO_PARAM | AV_OPT_FLAG_DECODING_PARAM
static const AVOption options[] = {
    { "request_sample_rate", "requested sample rate", OFFSET(request_sample_rate), AV_OPT_TYPE_INT,   { .dbl = 0 }, -1,      48000,   AD, "sample_rate" },
    {     "source",          "source, if known",      0,                           AV_OPT_TYPE_CONST, {.dbl = -1 }, INT_MIN, INT_MAX, AD, "sample_rate" },
    { NULL },
};

static const AVClass class = {
    .class_name = "libopus decoder",
    .item_name  = av_default_item_name,
    .option     = options,
    .version    = LIBAVUTIL_VERSION_INT,
};

AVCodec ff_libopus_decoder = {
    .name           = "libopus",
    .type           = AVMEDIA_TYPE_AUDIO,
    .id             = CODEC_ID_OPUS,
    .priv_data_size = sizeof(LibOpusDecContext),
    .init           = libopus_decode_init,
    .close          = libopus_decode_close,
    .decode         = libopus_decode_frame,
    .flush          = libopus_decode_flush,
    .long_name      = NULL_IF_CONFIG_SMALL("libopus Opus"),
    .priv_class     = &class,
};
