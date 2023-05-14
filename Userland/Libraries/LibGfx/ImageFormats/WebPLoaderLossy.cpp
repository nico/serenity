/*
 * Copyright (c) 2023, Nico Weber <thakis@chromium.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#define WEBP_DEBUG 1

#include <AK/BitStream.h>
#include <AK/Debug.h>
#include <AK/Endian.h>
#include <AK/Format.h>
#include <AK/MemoryStream.h>
#include <AK/Vector.h>
#include <LibGfx/ImageFormats/WebPLoaderLossy.h>

// Lossy format: https://datatracker.ietf.org/doc/html/rfc6386

namespace Gfx {

// https://developers.google.com/speed/webp/docs/riff_container#simple_file_format_lossy
// https://datatracker.ietf.org/doc/html/rfc6386#section-19 "Annex A: Bitstream Syntax"
ErrorOr<VP8Header> decode_webp_chunk_VP8_header(ReadonlyBytes vp8_data)
{
    if (vp8_data.size() < 10)
        return Error::from_string_literal("WebPImageDecoderPlugin: 'VP8 ' chunk too small");

    // FIXME: Eventually, this should probably call into LibVideo/VP8,
    // and image decoders should move into LibImageDecoders which depends on both LibGfx and LibVideo.
    // (LibVideo depends on LibGfx, so LibGfx can't depend on LibVideo itself.)

    // https://datatracker.ietf.org/doc/html/rfc6386#section-4 "Overview of Compressed Data Format"
    // "The decoder is simply presented with a sequence of compressed frames [...]
    //  The first frame presented to the decompressor is [...] a key frame.  [...]
    //  [E]very compressed frame has three or more pieces. It begins with an uncompressed data chunk comprising 10 bytes in the case of key frames"

    u8 const* data = vp8_data.data();

    // https://datatracker.ietf.org/doc/html/rfc6386#section-9.1 "Uncompressed Data Chunk"
    u32 frame_tag = data[0] | (data[1] << 8) | (data[2] << 16);
    bool is_key_frame = (frame_tag & 1) == 0; // https://www.rfc-editor.org/errata/eid5534
    u8 version = (frame_tag & 0xe) >> 1;
    bool show_frame = (frame_tag & 0x10) != 0;
    u32 size_of_first_partition = frame_tag >> 5;

    if (!is_key_frame)
        return Error::from_string_literal("WebPImageDecoderPlugin: 'VP8 ' chunk not a key frame");

    // FIXME: !show_frame does not make sense in a webp file either, probably?

    // FIXME: reject version > 3

    u32 start_code = data[3] | (data[4] << 8) | (data[5] << 16);
    if (start_code != 0x2a019d) // https://www.rfc-editor.org/errata/eid7370
        return Error::from_string_literal("WebPImageDecoderPlugin: 'VP8 ' chunk invalid start_code");

    // "The scaling specifications for each dimension are encoded as follows.
    //   0     | No upscaling (the most common case).
    //   1     | Upscale by 5/4.
    //   2     | Upscale by 5/3.
    //   3     | Upscale by 2."
    // This is a display-time operation and doesn't affect decoding."
    u16 width_and_horizontal_scale = data[6] | (data[7] << 8);
    u16 width = width_and_horizontal_scale & 0x3fff;
    u8 horizontal_scale = width_and_horizontal_scale >> 14;

    u16 heigth_and_vertical_scale = data[8] | (data[9] << 8);
    u16 height = heigth_and_vertical_scale & 0x3fff;
    u8 vertical_scale = heigth_and_vertical_scale >> 14;

    dbgln_if(WEBP_DEBUG, "version {}, show_frame {}, size_of_first_partition {}, width {}, horizontal_scale {}, height {}, vertical_scale {}",
        version, show_frame, size_of_first_partition, width, horizontal_scale, height, vertical_scale);

    if (vp8_data.size() < 10 + size_of_first_partition)
        return Error::from_string_literal("WebPImageDecoderPlugin: 'VP8 ' chunk too small for full first partition");

    return VP8Header { version, show_frame, size_of_first_partition, width, horizontal_scale, height, vertical_scale, vp8_data.slice(10, size_of_first_partition), vp8_data.slice(10 + size_of_first_partition) };
}

namespace {

// https://datatracker.ietf.org/doc/html/rfc6386#section-7 "Boolean Entropy Decoder"
// XXX code copied from LibVideo/VP9/BooleanDecoder.{h,cpp} and tweaked minorly
class BooleanEntropyDecoder {
public:
    static ErrorOr<BooleanEntropyDecoder> initialize(BigEndianInputBitStream& bit_stream);

    template<Unsigned T = u64>
    ErrorOr<T> read_bits(size_t count)
    {
        return m_bit_stream.read_bits<T>(count);
    }

    ErrorOr<bool> read_bool(u8 probability);
    ErrorOr<u32> read_literal(u8 bits);
    ErrorOr<i32> read_signed_literal(u8 bits);
    size_t bits_remaining() const;

private:
    BooleanEntropyDecoder(BigEndianInputBitStream& bit_stream, u32 value, u32 range)
        : m_bit_stream(bit_stream)
        , m_value(value)
        , m_range(range)
    {
    }

    BigEndianInputBitStream& m_bit_stream;
    u32 m_value { 0 };
    u32 m_range { 0 };
};

ErrorOr<BooleanEntropyDecoder> BooleanEntropyDecoder::initialize(BigEndianInputBitStream& bit_stream)
{
    VERIFY(bit_stream.is_aligned_to_byte_boundary());
    u16 value = TRY(bit_stream.read_value<u8>());
    value = (value << 8) | TRY(bit_stream.read_value<u8>());
    u8 range = 255;
    BooleanEntropyDecoder decoder { bit_stream, value, range };
    return decoder;
}

ErrorOr<bool> BooleanEntropyDecoder::read_bool(u8 probability)
{
    auto split = 1u + (((m_range - 1u) * probability) >> 8u);
    u32 SPLIT = split << 8;

    bool return_bool;

    if (m_value >= SPLIT) {
//fprintf(stderr, "bit 1 range %d split %d value %d SPLIT %d\n", m_range, split, m_value, SPLIT);
        return_bool = true;
        m_range -= split;
        m_value -= SPLIT;
    } else {
//fprintf(stderr, "bit 0 range %d split %d value %d SPLIT %d\n", m_range, split, m_value, SPLIT);
        return_bool = false;
        m_range = split;
    }

#if 1
    if (m_range < 128) {
        VERIFY(m_range != 0);
        u8 bits_to_shift_into_range = count_leading_zeroes((u8)m_range);
        m_range <<= bits_to_shift_into_range;
        m_value = (m_value << bits_to_shift_into_range) | TRY(m_bit_stream.read_bits<u32>(bits_to_shift_into_range));
    }
#else
     while (m_range < 128) {  /* shift out irrelevant value bits */
       m_value = (m_value << 1) | TRY(m_bit_stream.read_bits<u32>(1));
       m_range <<= 1;
     }
#endif

    return return_bool;
}

ErrorOr<u32> BooleanEntropyDecoder::read_literal(u8 bits)
{
    u32 result = 0;
    for (size_t i = 0; i < bits; i++)
        result = 2 * result + TRY(read_bool(128));
    return result;
}

ErrorOr<i32> BooleanEntropyDecoder::read_signed_literal(u8 bits)
{
    if (!bits)
        return 0;
    i32 result = 0;
    if (TRY(read_bool(128)))
        result = -1;
    for (size_t i = 1; i < bits; i++)
        result = 2 * result + TRY(read_bool(128));
    return result;
}

// https://datatracker.ietf.org/doc/html/rfc6386#section-8.1 "Tree Coding Implementation"
class TreeDecoder {
public:
    using tree_index = i8;

    TreeDecoder(ReadonlySpan<tree_index> tree)
        : m_tree(tree)
    {
    }

    ErrorOr<int> read(BooleanEntropyDecoder&, ReadonlyBytes probabilities);

private:
   // "A tree may then be compactly represented as an array of (pairs of)
   //  8-bit integers.  Each (even) array index corresponds to an interior
   //  node of the tree; the 0th index of course corresponds to the root of
   //  the tree.  The array entries come in pairs corresponding to the left
   //  (0) and right (1) branches of the subtree below the interior node.
   //  We use the convention that a positive (even) branch entry is the
   //  index of a deeper interior node, while a nonpositive entry v
   //  corresponds to a leaf whose value is -v."
   ReadonlySpan<tree_index> m_tree;

};

ErrorOr<int> TreeDecoder::read(BooleanEntropyDecoder& decoder, ReadonlyBytes probabilities)
{
    tree_index i = 0;
#if 0
    while ((i = m_tree[i + TRY(decoder.read_bool(probabilities[i >> 1]))]) > 0) {
    }
    return -i;
#else
    while (true) {
      u8 b = TRY(decoder.read_bool(probabilities[i >> 1]));
      i = m_tree[i + b];
      if (i <= 0) {
//dbgln_if(WEBP_DEBUG, "found {} bit {}", -i, b);
          return -i;
}
//dbgln_if(WEBP_DEBUG, "tree i {} prob {}", i, probabilities[i >> 1]);
    }
#endif
}

}

ErrorOr<NonnullRefPtr<Bitmap>> decode_webp_chunk_VP8_contents(VP8Header const& vp8_header, bool include_alpha_channel)
{
    auto bitmap_format = include_alpha_channel ? BitmapFormat::BGRA8888 : BitmapFormat::BGRx8888;

    // The first partition stores header, per-segment state, and macroblock metadata.

    FixedMemoryStream memory_stream { vp8_header.lossy_data };
    BigEndianInputBitStream bit_stream { MaybeOwned<Stream>(memory_stream) };
    auto decoder = TRY(BooleanEntropyDecoder::initialize(bit_stream));

    // https://datatracker.ietf.org/doc/html/rfc6386#section-19 "Annex A: Bitstream Syntax"
    //auto f = [&decoder](u32 n) { return decoder.read_bits(n); };
    auto L = [&decoder](u32 n) { return decoder.read_literal(n); };
    auto B = [&decoder](u8 prob) { return decoder.read_bool(prob); };

    // https://datatracker.ietf.org/doc/html/rfc6386#section-19.2 "Frame Header"

    // https://datatracker.ietf.org/doc/html/rfc6386#section-9.2 "Color Space and Pixel Type (Key Frames Only)"
    enum class ColorSpaceAndPixelType {
        YUV = 0,
        ReservedForFutureUse = 1,
    };
    auto color_space = static_cast<ColorSpaceAndPixelType>(TRY(L(1)));

    enum class ClampingSpecification {
        DecoderMustClampTo0To255 = 0,
        NoClampingNecessary = 1,
    };
    auto clamping_type = static_cast<ClampingSpecification>(TRY(L(1)));

    dbgln_if(WEBP_DEBUG, "color_space {} clamping_type {}", (int)color_space, (int)clamping_type);

    u8 mb_segment_tree_probs[3] = { 255, 255, 255 };

    // https://datatracker.ietf.org/doc/html/rfc6386#section-9.3 "Segment-Based Adjustments"
    u8 update_mb_segmentation_map = false;

    u8 segmentation_enabled = TRY(L(1));
    dbgln_if(WEBP_DEBUG, "segmentation_enabled {}", (int)segmentation_enabled);
    if (segmentation_enabled) {
        // "update_segmentation()" in 19.2

        // FIXME: Is this always true for keyframes in webp files? Should we return an Error if this is 0 instead?
        update_mb_segmentation_map = TRY(L(1));
        u8 update_segment_feature_data = TRY(L(1));

        dbgln_if(WEBP_DEBUG, "update_mb_segmentation_map {} update_segment_feature_data {}",
            update_mb_segmentation_map, update_segment_feature_data);

        if (update_segment_feature_data) {
            enum class SegmentFeatureMode {
                AbsoluteValueMode = 0,
                DeltaValueMode = 1,
            };
            auto segment_feature_mode = static_cast<SegmentFeatureMode>(TRY(L(1)));
            dbgln_if(WEBP_DEBUG, "segment_feature_mode {}", (int)segment_feature_mode);

            for (int i = 0; i < 4; ++i) {
                u8 quantizer_update = TRY(L(1));
                dbgln_if(WEBP_DEBUG, "quantizer_update {}", quantizer_update);
                if (quantizer_update) {
                    u8 quantizer_update_value = TRY(L(7));
                    u8 quantizer_update_sign = TRY(L(1));
                    dbgln_if(WEBP_DEBUG, "quantizer_update_value {} quantizer_update_sign {}", quantizer_update_value, quantizer_update_sign);
                }
            }
            for (int i = 0; i < 4; ++i) {
                u8 loop_filter_update = TRY(L(1));
                dbgln_if(WEBP_DEBUG, "loop_filter_update {}", loop_filter_update);
                if (loop_filter_update) {
                    u8 loop_filter_update_value = TRY(L(6));
                    u8 loop_filter_update_sign = TRY(L(1));
                    dbgln_if(WEBP_DEBUG, "loop_filter_update_value {} loop_filter_update_sign {}", loop_filter_update_value, loop_filter_update_sign);
                }
            }
        }

        if (update_mb_segmentation_map) {
            // This reads mb_segment_tree_probs for https://datatracker.ietf.org/doc/html/rfc6386#section-10.
            for (int i = 0; i < 3; ++i) {
                u8 segment_prob_update = TRY(L(1));
                dbgln_if(WEBP_DEBUG, "segment_prob_update {}", segment_prob_update);
                if (segment_prob_update) {
                    u8 segment_prob = TRY(L(8));
                    dbgln_if(WEBP_DEBUG, "segment_prob {}", segment_prob);
                    mb_segment_tree_probs[i] = segment_prob;
                }
            }
        }
    }

    // https://datatracker.ietf.org/doc/html/rfc6386#section-9.4 "Loop Filter Type and Levels"
    u8 filter_type = TRY(L(1));
    u8 loop_filter_level = TRY(L(6));
    u8 sharpness_level = TRY(L(3));
    using Prob = u8;
    dbgln_if(WEBP_DEBUG, "filter_type {} loop_filter_level {} sharpness_level {}", filter_type, loop_filter_level, sharpness_level);

    // "mb_lf_adjustments()" in 19.2
    u8 loop_filter_adj_enable = TRY(L(1));
    dbgln_if(WEBP_DEBUG, "loop_filter_adj_enable {}", loop_filter_adj_enable);
    if (loop_filter_adj_enable) {
        u8 mode_ref_lf_delta_update = TRY(L(1));
        dbgln_if(WEBP_DEBUG, "mode_ref_lf_delta_update {}", mode_ref_lf_delta_update);
        if (mode_ref_lf_delta_update) {
            for (int i = 0; i < 4; ++i) {
                u8 ref_frame_delta_update_flag = TRY(L(1));
                dbgln_if(WEBP_DEBUG, "ref_frame_delta_update_flag {}", ref_frame_delta_update_flag);
                if (ref_frame_delta_update_flag) {
                    u8 delta_magnitude = TRY(L(6));
                    u8 delta_sign = TRY(L(1)); // 0 - positive, 1 - negative
                    dbgln_if(WEBP_DEBUG, "delta_magnitude {} loop_filter_update_sign {}", delta_magnitude, delta_sign);
                }
            }
            for (int i = 0; i < 4; ++i) {
                u8 mb_mode_delta_update_flag = TRY(L(1));
                dbgln_if(WEBP_DEBUG, "mb_mode_delta_update_flag {}", mb_mode_delta_update_flag);
                if (mb_mode_delta_update_flag) {
                    u8 delta_magnitude = TRY(L(6));
                    u8 delta_sign = TRY(L(1));
                    dbgln_if(WEBP_DEBUG, "delta_magnitude {} loop_filter_update_sign {}", delta_magnitude, delta_sign);
                }
            }
        }
    }

    // https://datatracker.ietf.org/doc/html/rfc6386#section-9.5 "Token Partition and Partition Data Offsets"
    u8 log2_nbr_of_dct_partitions = TRY(L(2));
    dbgln_if(WEBP_DEBUG, "log2_nbr_of_dct_partitions {}", log2_nbr_of_dct_partitions);

    // https://datatracker.ietf.org/doc/html/rfc6386#section-4 says:
    // "If there is more than one partition
    //  for these coefficients, the sizes of the partitions -- except the
    //  last partition -- in bytes are also present in the bitstream right
    //  after the above first partition."
    // 19.2 completely omits them for that reason.
    // Let's read them now.
    u8 number_of_dct_partitions = 1 << log2_nbr_of_dct_partitions;

    // XXX read partition offsets
    auto data = vp8_header.second_partition;
    u32 offset = vp8_header.size_of_first_partition;
    for (size_t i = 0; i < number_of_dct_partitions - 1; ++i) {
        u32 size_of_partition = data[3 * i + 0] | (data[3 * i + 1] << 8) | (data[3 * i + 2] << 16);
        dbgln_if(WEBP_DEBUG, "offset {} size_of_partition {}", offset, size_of_partition);
        offset += size_of_partition;
    }

    // "quant_indices()" in 19.2
    // Also https://datatracker.ietf.org/doc/html/rfc6386#section-9.6 "Dequantization Indices"
    u8 y_ac_qi = TRY(L(7));
    dbgln_if(WEBP_DEBUG, "y_ac_qi {}", y_ac_qi);

    u8 y_dc_delta_present = TRY(L(1));
    dbgln_if(WEBP_DEBUG, "y_dc_delta_present {}", y_dc_delta_present);
    if (y_dc_delta_present) {
      u8 y_dc_delta_magnitude = TRY(L(4));
      u8 y_dc_delta_sign = TRY(L(1));
      dbgln_if(WEBP_DEBUG, "y_dc_delta_magnitude {} y_dc_delta_sign {}", y_dc_delta_magnitude, y_dc_delta_sign);
    }
    u8 y2_dc_delta_present = TRY(L(1));
    dbgln_if(WEBP_DEBUG, "y2_dc_delta_present {}", y2_dc_delta_present);
    if (y2_dc_delta_present) {
      u8 y2_dc_delta_magnitude = TRY(L(4));
      u8 y2_dc_delta_sign = TRY(L(1));
      dbgln_if(WEBP_DEBUG, "y2_dc_delta_magnitude {} y2_dc_delta_sign {}", y2_dc_delta_magnitude, y2_dc_delta_sign);
    }
    u8 y2_ac_delta_present = TRY(L(1));
    dbgln_if(WEBP_DEBUG, "y2_ac_delta_present {}", y2_ac_delta_present);
    if (y2_ac_delta_present) {
      u8 y2_ac_delta_magnitude = TRY(L(4));
      u8 y2_ac_delta_sign = TRY(L(1));
      dbgln_if(WEBP_DEBUG, "y2_ac_delta_magnitude {} y2_ac_delta_sign {}", y2_ac_delta_magnitude, y2_ac_delta_sign);
    }
    u8 uv_dc_delta_present = TRY(L(1));
    dbgln_if(WEBP_DEBUG, "uv_dc_delta_present {}", uv_dc_delta_present);
    if (uv_dc_delta_present) {
      u8 uv_dc_delta_magnitude = TRY(L(4));
      u8 uv_dc_delta_sign = TRY(L(1));
      dbgln_if(WEBP_DEBUG, "uv_dc_delta_magnitude {} uv_dc_delta_sign {}", uv_dc_delta_magnitude, uv_dc_delta_sign);
    }
    u8 uv_ac_delta_present = TRY(L(1));
    dbgln_if(WEBP_DEBUG, "uv_ac_delta_present {}", uv_ac_delta_present);
    if (uv_ac_delta_present) {
      u8 uv_ac_delta_magnitude = TRY(L(4));
      u8 uv_ac_delta_sign = TRY(L(1));
      dbgln_if(WEBP_DEBUG, "uv_ac_delta_magnitude {} uv_ac_delta_sign {}", uv_ac_delta_magnitude, uv_ac_delta_sign);
    }

    // Always key_frame in webp.
    u8 refresh_entropy_probs = TRY(L(1));
    dbgln_if(WEBP_DEBUG, "refresh_entropy_probs {}", refresh_entropy_probs);

    // "token_prob_update()" in 19.2
    // XXX token_prob_update says L(1) and L(8), but they should be B(p) and L(8) (?!)
    // https://datatracker.ietf.org/doc/html/rfc6386#section-13.4 "Token Probability Updates"

    enum dct_token {
        DCT_0,      /* value 0 */
        DCT_1,      /* 1 */
        DCT_2,      /* 2 */
        DCT_3,      /* 3 */
        DCT_4,      /* 4 */
        dct_cat1,   /* range 5 - 6  (size 2) */
        dct_cat2,   /* 7 - 10   (4) */
        dct_cat3,   /* 11 - 18  (8) */
        dct_cat4,   /* 19 - 34  (16) */
        dct_cat5,   /* 35 - 66  (32) */
        dct_cat6,   /* 67 - 2048  (1982) */
        dct_eob,    /* end of block */

        num_dct_tokens   /* 12 */
    };

    const Prob coeff_update_probs[4][8][3][num_dct_tokens - 1] = {
        {
            {
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
            },
            {
                { 176, 246, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 223, 241, 252, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 249, 253, 253, 255, 255, 255, 255, 255, 255, 255, 255 }
            },
            {
                { 255, 244, 252, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 234, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 253, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
            },
            {
                { 255, 246, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 239, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 254, 255, 254, 255, 255, 255, 255, 255, 255, 255, 255 }
            },
            {
                { 255, 248, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 251, 255, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
            },
            {
                { 255, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 251, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 254, 255, 254, 255, 255, 255, 255, 255, 255, 255, 255 }
            },
            {
                { 255, 254, 253, 255, 254, 255, 255, 255, 255, 255, 255 },
                { 250, 255, 254, 255, 254, 255, 255, 255, 255, 255, 255 },
                { 254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
            },
            {
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
            }
        },
        {
            {
                { 217, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 225, 252, 241, 253, 255, 255, 254, 255, 255, 255, 255 },
                { 234, 250, 241, 250, 253, 255, 253, 254, 255, 255, 255 }
            },
            {
                { 255, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 223, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 238, 253, 254, 254, 255, 255, 255, 255, 255, 255, 255 }
            },
            {
                { 255, 248, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 249, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
            },
            {
                { 255, 253, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 247, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
            },
            {
                { 255, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 252, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
            },
            {
                { 255, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 253, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
            },
            {
                { 255, 254, 253, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 250, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
            },
            {
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
            }
        },
        {
            {
                { 186, 251, 250, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 234, 251, 244, 254, 255, 255, 255, 255, 255, 255, 255 },
                { 251, 251, 243, 253, 254, 255, 254, 255, 255, 255, 255 }
            },
            {
                { 255, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 236, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 251, 253, 253, 254, 254, 255, 255, 255, 255, 255, 255 }
            },
            {
                { 255, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 254, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
            },
            {
                { 255, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 254, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
            },
            {
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
            },
            {
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
            },
            {
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
            },
            {
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
            }
        },
        {
            {
                { 248, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 250, 254, 252, 254, 255, 255, 255, 255, 255, 255, 255 },
                { 248, 254, 249, 253, 255, 255, 255, 255, 255, 255, 255 }
            },
            {
                { 255, 253, 253, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 246, 253, 253, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 252, 254, 251, 254, 254, 255, 255, 255, 255, 255, 255 }
            },
            {
                { 255, 254, 252, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 248, 254, 253, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 253, 255, 254, 254, 255, 255, 255, 255, 255, 255, 255 }
            },
            {
                { 255, 251, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 245, 251, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 253, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255 }
            },
            {
                { 255, 251, 253, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 252, 253, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 255, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
            },
            {
                { 255, 252, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 249, 255, 254, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 255, 255, 254, 255, 255, 255, 255, 255, 255, 255, 255 }
            },
            {
                { 255, 255, 253, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 250, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
            },
            {
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 },
                { 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255 }
            }
        }
    };

    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 8; j++) {
            for (int k = 0; k < 3; k++) {
                for (int l = 0; l < 11; l++) {
                    //u8 coeff_prob_update_flag = TRY(L(1));
                    u8 coeff_prob_update_flag = TRY(B(coeff_update_probs[i][j][k][l]));
                    //dbgln_if(WEBP_DEBUG, "coeff_prob_update_flag {}", coeff_prob_update_flag);
                    if (coeff_prob_update_flag) {
                        u8 coeff_prob = TRY(L(8));
                        //dbgln_if(WEBP_DEBUG, "coeff_prob {}", coeff_prob);
                        (void)coeff_prob;
                    }
                }
            }
        }
    }

    // https://datatracker.ietf.org/doc/html/rfc6386#section-9.11 "Remaining Frame Header Data (Key Frame)"
    u8 mb_no_skip_coeff = TRY(L(1));
    u8 prob_skip_false;
    dbgln_if(WEBP_DEBUG, "mb_no_skip_coeff {}", mb_no_skip_coeff);
    if (mb_no_skip_coeff) {
        prob_skip_false = TRY(L(8));
        dbgln_if(WEBP_DEBUG, "prob_skip_false {}", prob_skip_false);
    }
    // Non-keyframes read prob_intra etc here.

    // "This completes the layout of the frame header.  The remainder of the
    //  first data partition consists of macroblock-level prediction data."

    // https://datatracker.ietf.org/doc/html/rfc6386#section-19.3

    // "macroblock_header()" in 19.3
    // Corresponds to vp8_dixie_modemv_process_row()? And ParseIntraMode().

    // Key frames must use intra prediction, that is new macroblocks are predicted from old macroblocks in the same frame.
    // (Inter prediction on the other hand predicts new macroblocks from the corresponding macroblock in the previous frame.)

    // https://datatracker.ietf.org/doc/html/rfc6386#section-2 "Format Overview"
    // "Internally, VP8 decomposes each output frame into an array of
    //  macroblocks.  A macroblock is a square array of pixels whose Y
    //  dimensions are 16x16 and whose U and V dimensions are 8x8."
    int macroblock_width = (vp8_header.width + 15) / 16;
    int macroblock_height = (vp8_header.height + 15) / 16;

    // https://datatracker.ietf.org/doc/html/rfc6386#section-8.2 "Tree Coding Example"
    // Repeated in https://datatracker.ietf.org/doc/html/rfc6386#section-11.2 "Luma Modes"
    enum intra_mbmode {
        DC_PRED, /* predict DC using row above and column to the left */
#if 0
// from spec
        V_PRED,  /* predict rows using row above */
        H_PRED,  /* predict columns using column to the left */
        TM_PRED, /* propagate second differences a la "True Motion" */
#else
// from libwebp
        TM_PRED, /* propagate second differences a la "True Motion" */
        V_PRED,  /* predict rows using row above */
        H_PRED,  /* predict columns using column to the left */
#endif
 
        B_PRED,  /* each Y subblock is independently predicted */

        num_uv_modes = B_PRED,  /* first four modes apply to chroma */
        num_ymodes   /* all modes apply to luma */
    };

    // https://datatracker.ietf.org/doc/html/rfc6386#section-19.3 says "intra_y_mode selects the luminance intra-prediction mode (Section 16.1)",
    // but for keyframes the correct reference is actually https://datatracker.ietf.org/doc/html/rfc6386#section-11.2 "Luma Modes".
    // That is, we want "kf_ymode_tree", not "ymode_tree", and "kf_ymode_prob", not "ymode_prob".
    // See "decode_kf_mb_mode" in the reference decoder in the spec.
    const TreeDecoder::tree_index kf_ymode_tree[2 * (num_ymodes - 1) ] = {
        -B_PRED, 2,        /* root: B_PRED = "0", "1" subtree */
        4, 6,              /* "1" subtree has 2 descendant subtrees */
        -DC_PRED, -V_PRED, /* "10" subtree: DC_PRED = "100", V_PRED = "101" */
        -H_PRED, -TM_PRED  /* "11" subtree: H_PRED = "110", TM_PRED = "111" */
    };

    enum intra_bmode {
        B_DC_PRED,  /* predict DC using row above and column
                       to the left */
        B_TM_PRED,  /* propagate second differences a la
                       "True Motion" */

        B_VE_PRED,  /* predict rows using row above */
        B_HE_PRED,  /* predict columns using column to the left */

#if 0
// This is the order in the spec
        B_LD_PRED,  /* southwest (left and down) 45 degree diagonal
                       prediction */
        B_RD_PRED,  /* southeast (right and down) "" */

        B_VR_PRED,  /* SSE (vertical right) diagonal prediction */
        B_VL_PRED,  /* SSW (vertical left) "" */
#else
// This is the order in libwebp
// Looks like libwebp commit f67b5939a accidentally (?) changed this (?)
        B_RD_PRED,  /* southeast (right and down) "" */

        B_VR_PRED,  /* SSE (vertical right) diagonal prediction */

        B_LD_PRED,  /* southwest (left and down) 45 degree diagonal
                       prediction */
        B_VL_PRED,  /* SSW (vertical left) "" */
#endif

        B_HD_PRED,  /* ESE (horizontal down) "" */
        B_HU_PRED,  /* ENE (horizontal up) "" */

        num_intra_bmodes
    };

    const TreeDecoder::tree_index bmode_tree[2 * (num_intra_bmodes - 1)] = {
     -B_DC_PRED, 2,                   /* B_DC_PRED = "0" */
      -B_TM_PRED, 4,                  /* B_TM_PRED = "10" */
       -B_VE_PRED, 6,                 /* B_VE_PRED = "110" */
        8, 12,
         -B_HE_PRED, 10,              /* B_HE_PRED = "11100" */
          -B_RD_PRED, -B_VR_PRED,     /* B_RD_PRED = "111010",
                                         B_VR_PRED = "111011" */
         -B_LD_PRED, 14,              /* B_LD_PRED = "111110" */
           -B_VL_PRED, 16,            /* B_VL_PRED = "1111110" */
             -B_HD_PRED, -B_HU_PRED   /* HD = "11111110",
                                         HU = "11111111" */
    };

    // https://datatracker.ietf.org/doc/html/rfc6386#section-11.5 "Subblock Mode Probability Table"
    // The table here does NOT match the tables from the spec! libwebp commit f67b5939a changed these
    // tables in libwebp without updating the spec. This matches the tables in libwebp.
    const Prob kf_bmode_prob[num_intra_bmodes][num_intra_bmodes][num_intra_bmodes - 1] = {
        {
            { 231, 120,  48,  89, 115, 113, 120, 152, 112 },
            { 152, 179,  64, 126, 170, 118,  46,  70,  95 },
            { 175,  69, 143,  80,  85,  82,  72, 155, 103 },
            {  56,  58,  10, 171, 218, 189,  17,  13, 152 },
            { 114,  26,  17, 163,  44, 195,  21,  10, 173 },
            { 121,  24,  80, 195,  26,  62,  44,  64,  85 },
            { 144,  71,  10,  38, 171, 213, 144,  34,  26 },
            { 170,  46,  55,  19, 136, 160,  33, 206,  71 },
            {  63,  20,   8, 114, 114, 208,  12,   9, 226 },
            {  81,  40,  11,  96, 182,  84,  29,  16,  36 }
        },
        {
            { 134, 183,  89, 137,  98, 101, 106, 165, 148 },
            {  72, 187, 100, 130, 157, 111,  32,  75,  80 },
            {  66, 102, 167,  99,  74,  62,  40, 234, 128 },
            {  41,  53,   9, 178, 241, 141,  26,   8, 107 },
            {  74,  43,  26, 146,  73, 166,  49,  23, 157 },
            {  65,  38, 105, 160,  51,  52,  31, 115, 128 },
            { 104,  79,  12,  27, 217, 255,  87,  17,   7 },
            {  87,  68,  71,  44, 114,  51,  15, 186,  23 },
            {  47,  41,  14, 110, 182, 183,  21,  17, 194 },
            {  66,  45,  25, 102, 197, 189,  23,  18,  22 }
        },
        {
            {  88,  88, 147, 150,  42,  46,  45, 196, 205 },
            {  43,  97, 183, 117,  85,  38,  35, 179,  61 },
            {  39,  53, 200,  87,  26,  21,  43, 232, 171 },
            {  56,  34,  51, 104, 114, 102,  29,  93,  77 },
            {  39,  28,  85, 171,  58, 165,  90,  98,  64 },
            {  34,  22, 116, 206,  23,  34,  43, 166,  73 },
            { 107,  54,  32,  26,  51,   1,  81,  43,  31 },
            {  68,  25, 106,  22,  64, 171,  36, 225, 114 },
            {  34,  19,  21, 102, 132, 188,  16,  76, 124 },
            {  62,  18,  78,  95,  85,  57,  50,  48,  51 }
        },
        {
            { 193, 101,  35, 159, 215, 111,  89,  46, 111 },
            {  60, 148,  31, 172, 219, 228,  21,  18, 111 },
            { 112, 113,  77,  85, 179, 255,  38, 120, 114 },
            {  40,  42,   1, 196, 245, 209,  10,  25, 109 },
            {  88,  43,  29, 140, 166, 213,  37,  43, 154 },
            {  61,  63,  30, 155,  67,  45,  68,   1, 209 },
            { 100,  80,   8,  43, 154,   1,  51,  26,  71 },
            { 142,  78,  78,  16, 255, 128,  34, 197, 171 },
            {  41,  40,   5, 102, 211, 183,   4,   1, 221 },
            {  51,  50,  17, 168, 209, 192,  23,  25,  82 }
        },
        {
            { 138,  31,  36, 171,  27, 166,  38,  44, 229 },
            {  67,  87,  58, 169,  82, 115,  26,  59, 179 },
            {  63,  59,  90, 180,  59, 166,  93,  73, 154 },
            {  40,  40,  21, 116, 143, 209,  34,  39, 175 },
            {  47,  15,  16, 183,  34, 223,  49,  45, 183 },
            {  46,  17,  33, 183,   6,  98,  15,  32, 183 },
            {  57,  46,  22,  24, 128,   1,  54,  17,  37 },
            {  65,  32,  73, 115,  28, 128,  23, 128, 205 },
            {  40,   3,   9, 115,  51, 192,  18,   6, 223 },
            {  87,  37,   9, 115,  59,  77,  64,  21,  47 }
        },
        {
            { 104,  55,  44, 218,   9,  54,  53, 130, 226 },
            {  64,  90,  70, 205,  40,  41,  23,  26,  57 },
            {  54,  57, 112, 184,   5,  41,  38, 166, 213 },
            {  30,  34,  26, 133, 152, 116,  10,  32, 134 },
            {  39,  19,  53, 221,  26, 114,  32,  73, 255 },
            {  31,   9,  65, 234,   2,  15,   1, 118,  73 },
            {  75,  32,  12,  51, 192, 255, 160,  43,  51 },
            {  88,  31,  35,  67, 102,  85,  55, 186,  85 },
            {  56,  21,  23, 111,  59, 205,  45,  37, 192 },
            {  55,  38,  70, 124,  73, 102,   1,  34,  98 }
        },
        {
            { 125,  98,  42,  88, 104,  85, 117, 175,  82 },
            {  95,  84,  53,  89, 128, 100, 113, 101,  45 },
            {  75,  79, 123,  47,  51, 128,  81, 171,   1 },
            {  57,  17,   5,  71, 102,  57,  53,  41,  49 },
            {  38,  33,  13, 121,  57,  73,  26,   1,  85 },
            {  41,  10,  67, 138,  77, 110,  90,  47, 114 },
            { 115,  21,   2,  10, 102, 255, 166,  23,   6 },
            { 101,  29,  16,  10,  85, 128, 101, 196,  26 },
            {  57,  18,  10, 102, 102, 213,  34,  20,  43 },
            { 117,  20,  15,  36, 163, 128,  68,   1,  26 }
        },
        {
            { 102,  61,  71,  37,  34,  53,  31, 243, 192 },
            {  69,  60,  71,  38,  73, 119,  28, 222,  37 },
            {  68,  45, 128,  34,   1,  47,  11, 245, 171 },
            {  62,  17,  19,  70, 146,  85,  55,  62,  70 },
            {  37,  43,  37, 154, 100, 163,  85, 160,   1 },
            {  63,   9,  92, 136,  28,  64,  32, 201,  85 },
            {  75,  15,   9,   9,  64, 255, 184, 119,  16 },
            {  86,   6,  28,   5,  64, 255,  25, 248,   1 },
            {  56,   8,  17, 132, 137, 255,  55, 116, 128 },
            {  58,  15,  20,  82, 135,  57,  26, 121,  40 }
        },
        {
            { 164,  50,  31, 137, 154, 133,  25,  35, 218 },
            {  51, 103,  44, 131, 131, 123,  31,   6, 158 },
            {  86,  40,  64, 135, 148, 224,  45, 183, 128 },
            {  22,  26,  17, 131, 240, 154,  14,   1, 209 },
            {  45,  16,  21,  91,  64, 222,   7,   1, 197 },
            {  56,  21,  39, 155,  60, 138,  23, 102, 213 },
            {  83,  12,  13,  54, 192, 255,  68,  47,  28 },
            {  85,  26,  85,  85, 128, 128,  32, 146, 171 },
            {  18,  11,   7,  63, 144, 171,   4,   4, 246 },
            {  35,  27,  10, 146, 174, 171,  12,  26, 128 }
        },
        {
            { 190,  80,  35,  99, 180,  80, 126,  54,  45 },
            {  85, 126,  47,  87, 176,  51,  41,  20,  32 },
            { 101,  75, 128, 139, 118, 146, 116, 128,  85 },
            {  56,  41,  15, 176, 236,  85,  37,   9,  62 },
            {  71,  30,  17, 119, 118, 255,  17,  18, 138 },
            { 101,  38,  60, 138,  55,  70,  43,  26, 142 },
            { 146,  36,  19,  30, 171, 255,  97,  27,  20 },
            { 138,  45,  61,  62, 219,   1,  81, 188,  64 },
            {  32,  41,  20, 117, 151, 142,  20,  21, 163 },
            { 112,  19,  12,  61, 195, 128,  48,   4,  24 }
        }
    };


    const TreeDecoder::tree_index uv_mode_tree[2 * (num_uv_modes - 1)] = {
        -DC_PRED, 2,              /* root: DC_PRED = "0", "1" subtree */
            -V_PRED, 4,           /* "1" subtree:  V_PRED = "10", "11" subtree */
                -H_PRED, -TM_PRED /* "11" subtree: H_PRED = "110", TM_PRED = "111" */
    };
    const Prob kf_uv_mode_prob [num_uv_modes - 1] = { 142, 114, 183 };

    // "For macroblocks on the top row or left edge of the image, some of
    //  the predictors will be non-existent.  Such predictors are taken
    //  to have had the value B_DC_PRED, which, perhaps conveniently,
    //  takes the value 0 in the enumeration above.
    //  A simple management scheme for these contexts might maintain a row
    //  of above predictors and four left predictors.  Before decoding the
    //  frame, the entire row is initialized to B_DC_PRED; before decoding
    //  each row of macroblocks, the four left predictors are also set to
    //  B_DC_PRED.  After decoding a macroblock, the bottom four subblock
    //  modes are copied into the row predictor (at the current position,
    //  which then advances to be above the next macroblock), and the
    //  right four subblock modes are copied into the left predictor."
    Vector<intra_bmode> above;
    TRY(above.try_resize(macroblock_width * 4)); // One per 4x4 subblock.
    Vector<intra_bmode, 4> left; // One per 4x4 subblock.
    TRY(left.try_resize(4)); // One per 4x4 subblock.

    struct MacroblockMetadata {
        // https://datatracker.ietf.org/doc/html/rfc6386#section-10 "Segment-Based Feature Adjustments"
        int segment_id; // 0, 1, 2, or 3. Fits in two bits.

        // https://datatracker.ietf.org/doc/html/rfc6386#section-11.1 "mb_skip_coeff"
        bool skip_coefficients;

        intra_mbmode intra_y_mode;
        intra_mbmode uv_mode;
    };
    Vector<MacroblockMetadata> macroblock_metadata;

    for (int mb_y = 0; mb_y < macroblock_height; ++mb_y) {
        for (int i = 0; i < 4; ++i) left[i] = B_DC_PRED;

        for (int mb_x = 0; mb_x < macroblock_width; ++mb_x) {
            int segment_id = 0;
            bool skip_coefficients = false;

            if (update_mb_segmentation_map) {
                // https://datatracker.ietf.org/doc/html/rfc6386#section-10 "Segment-Based Feature Adjustments"
                const TreeDecoder::tree_index mb_segment_tree [2 * (4 - 1)] = {
                     2,  4, /* root: "0", "1" subtrees */
                    -0, -1, /* "00" = 0th value, "01" = 1st value */
                    -2, -3  /* "10" = 2nd value, "11" = 3rd value */
                };
                segment_id = TRY(TreeDecoder(mb_segment_tree).read(decoder, mb_segment_tree_probs));
                dbgln_if(WEBP_DEBUG, "segment_id {}", segment_id);
            }
            if (mb_no_skip_coeff) {
                u8 mb_skip_coeff = TRY(B(prob_skip_false));
                dbgln_if(WEBP_DEBUG, "mb_skip_coeff {}", mb_skip_coeff);
                skip_coefficients = mb_skip_coeff;
            }

            const Prob kf_ymode_prob [num_ymodes - 1] = { 145, 156, 163, 128};
            int intra_y_mode = TRY(TreeDecoder(kf_ymode_tree).read(decoder, kf_ymode_prob));
            dbgln_if(WEBP_DEBUG, "intra_y_mode {} mb_y {} mb_x {}", intra_y_mode, mb_y, mb_x);

            // "If the Ymode is B_PRED, it is followed by a (tree-coded) mode for each of the 16 Y subblocks."
            if (intra_y_mode == B_PRED) {
                //for (int i = 0; i < 16; ++i) {
                for (int y = 0; y < 4; ++y) {
                    for (int x = 0; x < 4; ++x) {
                        // "The outer two dimensions of this array are indexed by the already-
                        //  coded subblock modes above and to the left of the current block,
                        //  respectively."
                        int A = above[mb_x * 4 + x];
                        int L = left[y];

                        auto intra_b_mode = static_cast<intra_bmode>(TRY(TreeDecoder(bmode_tree).read(decoder, kf_bmode_prob[A][L])));
                        dbgln_if(WEBP_DEBUG, "A {} L {} intra_b_mode {} y {} x {}", A, L, (int)intra_b_mode, y, x);

                        above[mb_x * 4 + x] = intra_b_mode;
                        left[y] = intra_b_mode;
                    }
                }
            } else {
                VERIFY(intra_y_mode < B_PRED);
#if 0
                constexpr intra_bmode b_mode_from_y_mode[] = { B_DC_PRED, B_VE_PRED, B_HE_PRED, B_TM_PRED };
                intra_bmode intra_b_mode = b_mode_from_y_mode[intra_y_mode];
#else
// With libwebp enum changes, the modes match exactly.
                intra_bmode intra_b_mode = static_cast<intra_bmode>(intra_y_mode);
#endif
                for (int i = 0; i < 4; ++i) {
                    above[mb_x * 4 + i] = intra_b_mode;
                    left[i] = intra_b_mode;
                }
            }

            int uv_mode = TRY(TreeDecoder(uv_mode_tree).read(decoder, kf_uv_mode_prob));
            dbgln_if(WEBP_DEBUG, "uv_mode {} mb_y {} mb_x {}", uv_mode, mb_y, mb_x);

            TRY(macroblock_metadata.try_append(MacroblockMetadata { segment_id, skip_coefficients, static_cast<intra_mbmode>(intra_y_mode), static_cast<intra_mbmode>(uv_mode) }));
        }
    }

    dbgln_if(WEBP_DEBUG, "stream offset {} size {}", TRY(memory_stream.tell()), TRY(memory_stream.size()));

    // Done with the first partition!

    if (number_of_dct_partitions > 1)
        return Error::from_string_literal("WebPImageDecoderPlugin: decoding lossy webps with more than one dct partition not yet implemented");

    // The second partition stores coefficients for all macroblocks.
    {
        FixedMemoryStream memory_stream { vp8_header.second_partition };
        BigEndianInputBitStream bit_stream { MaybeOwned<Stream>(memory_stream) };
        auto decoder = TRY(BooleanEntropyDecoder::initialize(bit_stream));

        // https://datatracker.ietf.org/doc/html/rfc6386#section-13.2 "Coding of Individual Coefficient Values"
        enum dct_token {
            DCT_0,      /* value 0 */
            DCT_1,      /* 1 */
            DCT_2,      /* 2 */
            DCT_3,      /* 3 */
            DCT_4,      /* 4 */
            dct_cat1,   /* range 5 - 6  (size 2) */
            dct_cat2,   /* 7 - 10   (4) */
            dct_cat3,   /* 11 - 18  (8) */
            dct_cat4,   /* 19 - 34  (16) */
            dct_cat5,   /* 35 - 66  (32) */
            dct_cat6,   /* 67 - 2048  (1982) */
            dct_eob,    /* end of block */

            num_dct_tokens   /* 12 */
        };

        const TreeDecoder::tree_index coeff_tree[2 * (num_dct_tokens - 1)] = {
         -dct_eob, 2,               /* eob = "0"   */
          -DCT_0, 4,                /* 0   = "10"  */
           -DCT_1, 6,               /* 1   = "110" */
            8, 12,
             -DCT_2, 10,            /* 2   = "11100" */
              -DCT_3, -DCT_4,       /* 3   = "111010", 4 = "111011" */
             14, 16,
              -dct_cat1, -dct_cat2, /* cat1 =  "111100",
                                       cat2 = "111101" */
             18, 20,
              -dct_cat3, -dct_cat4, /* cat3 = "1111100",
                                       cat4 = "1111101" */
              -dct_cat5, -dct_cat6  /* cat4 = "1111110",
                                       cat4 = "1111111" */
        };

        for (int mb_y = 0, i = 0; mb_y < macroblock_height; ++mb_y) {
            for (int mb_x = 0; mb_x < macroblock_width; ++mb_x, ++i) {
                // See also https://datatracker.ietf.org/doc/html/rfc6386#section-19.3, residual_data() and residual_block()

                auto const& metadata = macroblock_metadata[i];
                // "firstCoeff is 1 for luma blocks of macroblocks containing Y2 subblock; otherwise 0"

                // https://datatracker.ietf.org/doc/html/rfc6386#section-13

                // "For all intra- and inter-prediction modes apart from B_PRED (intra:
                //  whose Y subblocks are independently predicted) and SPLITMV (inter),
                //  each macroblock's residue record begins with the Y2 component of the
                //  residue, coded using a WHT.  B_PRED and SPLITMV coded macroblocks
                //  omit this WHT and specify the 0th DCT coefficient in each of the 16 Y
                //  subblocks."
                bool have_y2 = metadata.intra_y_mode != B_PRED;

                // "After the optional Y2 block, the residue record continues with 16
                //  DCTs for the Y subblocks, followed by 4 DCTs for the U subblocks,
                //  ending with 4 DCTs for the V subblocks.  The subblocks occur in the
                //  usual order."

                /* (1 Y2)?, 16 Y, 4 U, 4 V */
                for (int i = have_y2 ? 0 : 1; i < 25; ++i) {

                    // https://datatracker.ietf.org/doc/html/rfc6386#section-13.2 "Coding of Individual Coefficient Values"
                    int token = TRY(TreeDecoder(coeff_tree).read(decoder, {}/* XXX prob */));
                    (void)token;
                }
            }
        }
    }

    (void)bitmap_format;
    return Error::from_string_literal("WebPImageDecoderPlugin: decoding lossy webps not yet implemented");
}

}
