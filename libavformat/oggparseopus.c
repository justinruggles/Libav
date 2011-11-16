/*
 * Copyright (c) 2011  Justin Ruggles
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

#include "libavutil/log.h"
#include "libavutil/avstring.h"
#include "libavcodec/bytestream.h"
#include "avformat.h"
#include "oggdec.h"

struct opus_params {
    int last_granule;
    int page_duration;
    int pre_skip;
    int header_count;
};

static int opus_header(AVFormatContext *s, int idx) {
    struct ogg *ogg = s->priv_data;
    struct ogg_stream *os = ogg->streams + idx;
    struct opus_params *op = os->private;
    AVStream *st = s->streams[idx];
    uint8_t *p = os->buf + os->pstart;

    if (!op) {
        op = av_mallocz(sizeof(*op));
        if (!op)
            return AVERROR(ENOMEM);
        os->private = op;
    }

    if (op->header_count > 1)
        return 0;

    if (os->psize < 8) {
        av_log(s, AV_LOG_ERROR, "header packet is too small\n");
        return AVERROR_INVALIDDATA;
    }

    if (!op->header_count) {
        // id header
        int sample_rate, channels, output_gain;
        char buf[16];

        if (memcmp(p, "OpusHead", 8)) {
            av_log(s, AV_LOG_ERROR, "first packet must be an id header\n");
            return AVERROR_INVALIDDATA;
        }
        if (os->psize < 19) {
            av_log(s, AV_LOG_ERROR, "Invalid OpusHead header packet\n");
            return AVERROR_INVALIDDATA;
        }
        if (AV_RL8(p + 8) != 0) {
            av_log(s, AV_LOG_ERROR, "Invalid Ogg/Opus encapsulation version\n");
            return AVERROR_INVALIDDATA;
        }

        channels = AV_RL8(p + 9);
        if (!channels) {
            av_log(s, AV_LOG_ERROR, "Invalid number of source channels\n");
            return AVERROR_INVALIDDATA;
        }
        snprintf(buf, sizeof(buf), "%d", channels);
        av_dict_set(&s->metadata, "source_channels", buf, 0);

        op->pre_skip = AV_RL16(p + 10);

        sample_rate = AV_RL32(p + 12);
        if (sample_rate > 0) {
            snprintf(buf, sizeof(buf), "%d", sample_rate);
            av_dict_set(&s->metadata, "source_sample_rate", buf, 0);
        }

        output_gain = AV_RL16(p + 16);
        if (output_gain) {
            float fgain = (output_gain >> 8) + (output_gain & 0x7F) / 128.0;
            snprintf(buf, sizeof(buf), "%1.3f dB", fgain);
            av_dict_set(&s->metadata, "output_gain", buf, 0);
        }

        st->codec->codec_type     = AVMEDIA_TYPE_AUDIO;
        st->codec->codec_id       = CODEC_ID_OPUS;
        st->codec->channels       = 2;
        st->codec->channel_layout = AV_CH_LAYOUT_STEREO;
        st->codec->sample_rate    = 48000;

        av_set_pts_info(st, 64, 1, 48000);
        av_strlcpy(st->codec->codec_name, "Opus", 32);

        // pass id header in extradata so that the decoder can optionally apply
        // output gain, use original sample rate and channel count, and
        // correctly map multi-channel output
        st->codec->extradata_size = os->psize;
        st->codec->extradata = av_mallocz(st->codec->extradata_size +
                                          FF_INPUT_BUFFER_PADDING_SIZE);
        if (!st->codec->extradata)
            return AVERROR(ENOMEM);
        memcpy(st->codec->extradata, p, st->codec->extradata_size);
    } else  {
        // comment header
        if (memcmp(p, "OpusTags", 8)) {
            av_log(s, AV_LOG_ERROR, "second packet must be a comment header\n");
            return AVERROR_INVALIDDATA;
        }
        if (ff_vorbis_comment(s, &st->metadata, p + 8, os->psize - 8)) {
            av_log(s, AV_LOG_ERROR, "Invalid OpusTags header packet\n");
            return AVERROR_INVALIDDATA;
        }
    }

    op->header_count++;
    return 1;
}

static uint16_t config_frame_duration[32] = {
    480, 960, 1920, 2880,
    480, 960, 1920, 2880,
    480, 960, 1920, 2880,
    480, 960,
    480, 960,
    120, 240,  480,  960,
    120, 240,  480,  960,
    120, 240,  480,  960,
    120, 240,  480,  960,
};

#define MAX_FRAME_SIZE 1275
#define MAX_FRAMES     48

enum OpusMode {
    OPUS_MODE_SILK,
    OPUS_MODE_HYBRID,
    OPUS_MODE_CELT
};

#ifdef DEBUG
static const char *opus_mode_str[3] = {
    "silk", "hybrid", "celt"
};
#endif

enum OpusBandwidth {
    OPUS_BANDWIDTH_NARROWBAND,
    OPUS_BANDWIDTH_MEDIUMBAND,
    OPUS_BANDWIDTH_WIDEBAND,
    OPUS_BANDWIDTH_SUPERWIDEBAND,
    OPUS_BANDWIDTH_FULLBAND
};

#ifdef DEBUG
static const char *opus_bandwidth_str[5] = {
    "narrowband", "medium-band", "wideband", "super-wideband", "fullband"
};
#endif

typedef struct {
    const uint8_t *data;            /** packet data */
    int size;                       /** packet size */
    int code;                       /** packet code: specifies the frame layout */
    int stereo;                     /** stereo flag */
    int vbr;                        /** vbr flag */
    int config;                     /** configuration: tells the audio mode, bandwidth, and frame duration */
    int count;                      /** frame count */
    int frame_offset[MAX_FRAMES];   /** frame offsets */
    int frame_size[MAX_FRAMES];     /** frame sizes */
    int padding;                    /** padding size */
    int frame_duration;             /** frame duration, in samples @ 48kHz */
    enum OpusMode mode;             /** mode */
    enum OpusBandwidth bandwidth;   /** bandwidth */
} OpusPacket;

static void opus_dprint_packet(AVFormatContext *s, OpusPacket *pkt)
{
#ifdef DEBUG
    int i;
    av_dlog(s, "[OPUS_PACKET]\n");
    av_dlog(s, "size=%d\n",            pkt->size);
    av_dlog(s, "code=%d\n",            pkt->code);
    av_dlog(s, "stereo=%d\n",          pkt->stereo);
    av_dlog(s, "vbr=%d\n",             pkt->vbr);
    av_dlog(s, "config=%d\n",          pkt->config);
    av_dlog(s, "mode=%s\n",            opus_mode_str[pkt->mode]);
    av_dlog(s, "bandwidth=%s\n",       opus_bandwidth_str[pkt->bandwidth]);
    av_dlog(s, "frame duration=%d\n",  pkt->frame_duration);
    av_dlog(s, "count=%d\n",           pkt->count);
    av_dlog(s, "packet duration=%d\n", pkt->frame_duration * pkt->count);
    for (i = 0; i < pkt->count; i++)
        av_dlog(s, "frame %d : size=%d offset=%d\n", i, pkt->frame_size[i],
                pkt->frame_offset[i]);
    av_dlog(s, "[/OPUS_PACKET]\n");
#endif
}

/**
 * Read a 1- or 2-byte frame length
 */
static inline int read_2byte_length(const uint8_t **ptr, const uint8_t *end)
{
    int val;

    if (*ptr >= end)
        return AVERROR_INVALIDDATA;
    val = *(*ptr)++;
    if (val >= 252) {
        if (*ptr >= end)
            return AVERROR_INVALIDDATA;
        val += 4 * *(*ptr)++;
    }
    return val;
}

/**
 * Read a multi-byte length (used for code 3 packet padding size)
 */
static inline int read_multibyte_length(const uint8_t **ptr, const uint8_t *end)
{
    int val = 0;
    int next;

    while (1) {
        if (*ptr >= end || val > INT_MAX - 254)
            return AVERROR_INVALIDDATA;
        next = *(*ptr)++;
        val += next;
        if (next < 255)
            break;
        else
            val--;
    }
    return val;
}

/**
 * Parse Opus packet info from raw packet data
 *
 * TODO: add option for self-delimited packets
 */
static int opus_packet_parse(const uint8_t *data, int len, OpusPacket *pkt)
{
    int frame_bytes, i;
    const uint8_t *ptr = data;
    const uint8_t *end = data + len;

    if (len < 1)
        return AVERROR_INVALIDDATA;

    pkt->data    = data;
    pkt->size    = len;
    pkt->padding = 0;

    /* TOC byte */
    i = *ptr++;
    pkt->code   = (i     ) & 0x3;
    pkt->stereo = (i >> 2) & 0x1;
    pkt->config = (i >> 3);

    /* code 2 and code 3 packets have at least 1 byte after the TOC */
    if (pkt->code >= 2 && len < 1)
        return AVERROR_INVALIDDATA;

    switch (pkt->code) {
    case 0:
        /* 1 frame */
        pkt->count = 1;
        pkt->vbr   = 0;
        frame_bytes = end - ptr;
        if (frame_bytes > MAX_FRAME_SIZE)
            return AVERROR_INVALIDDATA;
        pkt->frame_offset[0] = ptr - data;
        pkt->frame_size[0]   = frame_bytes;
        break;
    case 1:
        /* 2 frames, equal size */
        pkt->count = 2;
        pkt->vbr   = 0;
        frame_bytes = end - ptr;
        if (frame_bytes & 1 || frame_bytes / 2 > MAX_FRAME_SIZE)
            return AVERROR_INVALIDDATA;
        pkt->frame_offset[0] = ptr - data;
        pkt->frame_size[0]   = frame_bytes / 2;
        pkt->frame_offset[1] = pkt->frame_offset[0] + pkt->frame_size[0];
        pkt->frame_size[1]   = frame_bytes / 2;
        break;
    case 2:
        /* 2 frames, different sizes */
        pkt->count = 2;
        pkt->vbr   = 1;

        /* read 1st frame size */
        frame_bytes = read_2byte_length(&ptr, end);
        if (frame_bytes < 0)
            return AVERROR_INVALIDDATA;
        pkt->frame_offset[0] = ptr - data;
        pkt->frame_size[0]   = frame_bytes;

        /* calculate 2nd frame size */
        frame_bytes = len - frame_bytes;
        if (frame_bytes < 0 || frame_bytes > MAX_FRAME_SIZE)
            return AVERROR_INVALIDDATA;
        pkt->frame_offset[1] = pkt->frame_offset[0] + pkt->frame_size[0];
        pkt->frame_size[1]   = frame_bytes;
        break;
    case 3:
        /* 1 to 48 frames, can be different sizes */
        i = *ptr++;
        pkt->count   = (i     ) & 0x3F;
        pkt->padding = (i >> 6) & 0x01;
        pkt->vbr     = (i >> 7) & 0x01;

        if (!pkt->count)
            return AVERROR_INVALIDDATA;

        /* read padding size */
        if (pkt->padding) {
            pkt->padding = read_multibyte_length(&ptr, end);
            if (pkt->padding < 0)
                return AVERROR_INVALIDDATA;
        }
        if (end - ptr < pkt->padding)
            return AVERROR_INVALIDDATA;
        end -= pkt->padding;

        /* read frame sizes */
        if (pkt->vbr) {
            /* for VBR, all frames except the final one have their size coded
               in the bitstream. the last frame size is implicit. */
            int total_bytes = 0;
            for (i = 0; i < pkt->count - 1; i++) {
                frame_bytes = read_2byte_length(&ptr, end);
                if (frame_bytes < 0)
                    return AVERROR_INVALIDDATA;
                pkt->frame_size[i] = frame_bytes;
                total_bytes += frame_bytes;
            }
            frame_bytes = end - ptr;
            if (total_bytes > frame_bytes)
                return AVERROR_INVALIDDATA;
            pkt->frame_offset[0] = ptr - data;
            for (i = 1; i < pkt->count; i++)
                pkt->frame_offset[i] = pkt->frame_offset[i-1] + pkt->frame_size[i-1];
            pkt->frame_size[pkt->count-1] = frame_bytes - total_bytes;
        } else {
            /* for CBR, the remaining packet bytes are divided evenly between
               the frames */
            frame_bytes = end - ptr;
            if (frame_bytes % pkt->count || frame_bytes / pkt->count > MAX_FRAME_SIZE)
                return AVERROR_INVALIDDATA;
            frame_bytes /= pkt->count;
            pkt->frame_offset[0] = ptr - data;
            pkt->frame_size[0]   = frame_bytes;
            for (i = 1; i < pkt->count; i++) {
                pkt->frame_offset[i] = pkt->frame_offset[i-1] + pkt->frame_size[i-1];
                pkt->frame_size[i]   = frame_bytes;
            }
        }
    }

    /* total packet duration cannot be larger than 120ms */
    pkt->frame_duration = config_frame_duration[pkt->config];
    if (pkt->frame_duration * pkt->count > 5760)
        return AVERROR_INVALIDDATA;

    /* set mode */
    if (pkt->config < 12)
        pkt->mode = OPUS_MODE_SILK;
    else if (pkt->config < 16)
        pkt->mode = OPUS_MODE_HYBRID;
    else
        pkt->mode = OPUS_MODE_CELT;

    /* set bandwidth */
    switch (pkt->mode) {
    case OPUS_MODE_SILK:
        pkt->bandwidth = pkt->config / 4;
        break;
    case OPUS_MODE_HYBRID:
        if (pkt->config < 14)
            pkt->bandwidth = OPUS_BANDWIDTH_SUPERWIDEBAND;
        else
            pkt->bandwidth = OPUS_BANDWIDTH_FULLBAND;
        break;
    case OPUS_MODE_CELT:
        pkt->bandwidth = (pkt->config - 16) / 4;
        /* skip mediumband */
        if (pkt->bandwidth > 0)
            pkt->bandwidth++;
    }

    return 0;
}

static int opus_packet(AVFormatContext *s, int idx)
{

    struct ogg *ogg        = s->priv_data;
    struct ogg_stream *os  = ogg->streams + idx;
    struct opus_params *op = os->private;
    OpusPacket pkt;
    int ret;

    if ((ret = opus_packet_parse(os->buf + os->pstart, os->psize, &pkt)) < 0) {
        av_log(s, AV_LOG_ERROR, "error parsing Opus packet\n");
        return ret;
    }

    os->pduration = pkt.frame_duration * pkt.count;

    /* keep track of remaining page duration for use in final packet */
    if (os->lastpts != AV_NOPTS_VALUE) {
        op->page_duration = os->granule - op->last_granule;
        op->last_granule  = os->granule;
    } else {
       op->page_duration -= os->pduration;
    }

    /* first packet */
    if (!os->lastpts)
        os->lastpts = os->lastdts = -op->pre_skip;

    /* final packet */
    if (os->flags & OGG_FLAG_EOS && os->segp == os->nsegs)
        os->pduration = op->page_duration;

    opus_dprint_packet(s, &pkt);

    return 0;
}

const struct ogg_codec ff_opus_codec = {
    .magic     = "OpusHead",
    .magicsize = 8,
    .header    = opus_header,
    .packet    = opus_packet
};
