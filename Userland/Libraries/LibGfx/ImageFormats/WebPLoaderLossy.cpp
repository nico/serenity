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
#include <LibGfx/ImageFormats/WebPLoaderLossyTables.h>

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

    ErrorOr<int> read(BooleanEntropyDecoder&, ReadonlyBytes probabilities, int initial_i = 0);

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

ErrorOr<int> TreeDecoder::read(BooleanEntropyDecoder& decoder, ReadonlyBytes probabilities, int initial_i)
{
    tree_index i = initial_i;
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

static void vp8_short_inv_walsh4x4_c(i16* input, i16* output);
static void short_idct4x4llm_c(i16* input, i16* output, int pitch);

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

    // Reads n bits followed by a sign bit (0: positive, 1: negative).
    auto L_signed = [&decoder](u32 n) -> ErrorOr<i32> {
        i32 i = TRY(decoder.read_literal(n));
        if (TRY(decoder.read_literal(1)))
            i = -i;
        return i;
    };

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

    u8 update_segment_feature_data = false;
    enum class SegmentFeatureMode {
        AbsoluteValueMode = 0,
        DeltaValueMode = 1,
    };
    auto segment_feature_mode = SegmentFeatureMode::AbsoluteValueMode;

    struct SegmentData {
        Optional<i8> quantizer_update_value;
        Optional<i8> loop_filter_update_value;
    };
    SegmentData segment_data[4] = {};

    if (segmentation_enabled) {
        // "update_segmentation()" in 19.2

        // FIXME: Is this always true for keyframes in webp files? Should we return an Error if this is 0 instead?
        update_mb_segmentation_map = TRY(L(1));
        update_segment_feature_data = TRY(L(1));

        dbgln_if(WEBP_DEBUG, "update_mb_segmentation_map {} update_segment_feature_data {}",
            update_mb_segmentation_map, update_segment_feature_data);

        if (update_segment_feature_data) {
            segment_feature_mode = static_cast<SegmentFeatureMode>(TRY(L(1)));
            dbgln_if(WEBP_DEBUG, "segment_feature_mode {}", (int)segment_feature_mode);

            for (int i = 0; i < 4; ++i) {
                u8 quantizer_update = TRY(L(1));
                dbgln_if(WEBP_DEBUG, "quantizer_update {}", quantizer_update);
                if (quantizer_update) {
                    i8 quantizer_update_value = TRY(L_signed(7));
                    dbgln_if(WEBP_DEBUG, "quantizer_update_value {}", quantizer_update_value);
                    segment_data[i].quantizer_update_value = quantizer_update_value;
                }
            }
            for (int i = 0; i < 4; ++i) {
                u8 loop_filter_update = TRY(L(1));
                dbgln_if(WEBP_DEBUG, "loop_filter_update {}", loop_filter_update);
                if (loop_filter_update) {
                    i8 loop_filter_update_value = TRY(L_signed(6));
                    dbgln_if(WEBP_DEBUG, "loop_filter_update_value {}", loop_filter_update_value);
                    segment_data[i].loop_filter_update_value = loop_filter_update_value;
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
                    i8 delta = TRY(L_signed(6));
                    dbgln_if(WEBP_DEBUG, "delta {}", delta);
                }
            }
            for (int i = 0; i < 4; ++i) {
                u8 mb_mode_delta_update_flag = TRY(L(1));
                dbgln_if(WEBP_DEBUG, "mb_mode_delta_update_flag {}", mb_mode_delta_update_flag);
                if (mb_mode_delta_update_flag) {
                    i8 delta = TRY(L_signed(6));
                    dbgln_if(WEBP_DEBUG, "delta {}", delta);
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
    struct QuantizationIndices {
        u8 y_ac;
        u8 y_dc;

        u8 y2_dc;
        u8 y2_ac;

        u8 uv_dc;
        u8 uv_ac;
    };

    // "The first 7-bit index gives the dequantization table index for
    //  Y-plane AC coefficients, called yac_qi.  It is always coded and acts
    //  as a baseline for the other 5 quantization indices, each of which is
    //  represented by a delta from this baseline index."
    u8 y_ac_qi = TRY(L(7));
    dbgln_if(WEBP_DEBUG, "y_ac_qi {}", y_ac_qi);

    auto quantization_indices = QuantizationIndices { y_ac_qi, y_ac_qi, y_ac_qi, y_ac_qi, y_ac_qi, y_ac_qi };
    auto read_delta = [&L, &L_signed, y_ac_qi](StringView name, u8* destination) -> ErrorOr<void> {
        u8 is_present = TRY(L(1));
        dbgln_if(WEBP_DEBUG, "{}_present {}", name, is_present);
        if (is_present) {
            i8 delta = TRY(L_signed(4));
            dbgln_if(WEBP_DEBUG, "{}_delta {}", name, delta);
            if (y_ac_qi + delta < 0)
                return Error::from_string_literal("WebPImageDecoderPlugin: got negative quantization index");
            *destination = y_ac_qi + delta;
        }
        return {};
    };
    TRY(read_delta("y_dc_delta"sv, &quantization_indices.y_dc));
    TRY(read_delta("y2_dc_delta"sv, &quantization_indices.y2_dc));
    TRY(read_delta("y2_ac_delta"sv, &quantization_indices.y2_ac));
    TRY(read_delta("uv_dc_delta"sv, &quantization_indices.uv_dc));
    TRY(read_delta("uv_ac_delta"sv, &quantization_indices.uv_ac));

    // Always key_frame in webp.
    u8 refresh_entropy_probs = TRY(L(1));
    dbgln_if(WEBP_DEBUG, "refresh_entropy_probs {}", refresh_entropy_probs);

    // "token_prob_update()" in 19.2
    // XXX token_prob_update says L(1) and L(8), but they should be B(p) and L(8) (?!)
    // https://datatracker.ietf.org/doc/html/rfc6386#section-13.4 "Token Probability Updates"

    Prob coeff_probs[4][8][3][num_dct_tokens - 1];
    memcpy(coeff_probs, default_coeff_probs, sizeof(coeff_probs));

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
                        coeff_probs[i][j][k][l] = coeff_prob;
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


    const TreeDecoder::tree_index uv_mode_tree[2 * (num_uv_modes - 1)] = {
        -DC_PRED, 2,              /* root: DC_PRED = "0", "1" subtree */
            -V_PRED, 4,           /* "1" subtree:  V_PRED = "10", "11" subtree */
                -H_PRED, -TM_PRED /* "11" subtree: H_PRED = "110", TM_PRED = "111" */
    };
    const Prob kf_uv_mode_prob [num_uv_modes - 1] = { 142, 114, 183 };

    // https://datatracker.ietf.org/doc/html/rfc6386#section-11.3 "Subblock Mode Contexts"
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

    // Similar to BlockContext in LibVideo/VP9/Context.h
    struct MacroblockMetadata {
        // https://datatracker.ietf.org/doc/html/rfc6386#section-10 "Segment-Based Feature Adjustments"
        // Read only if `update_mb_segmentation_map` is set.
        int segment_id; // 0, 1, 2, or 3. Fits in two bits.

        // https://datatracker.ietf.org/doc/html/rfc6386#section-11.1 "mb_skip_coeff"
        bool skip_coefficients;

        intra_mbmode intra_y_mode;
        intra_mbmode uv_mode;

        intra_bmode intra_b_modes[16];
    };
    Vector<MacroblockMetadata> macroblock_metadata;

    for (int mb_y = 0; mb_y < macroblock_height; ++mb_y) {
        for (int i = 0; i < 4; ++i) left[i] = B_DC_PRED;

        for (int mb_x = 0; mb_x < macroblock_width; ++mb_x) {
            MacroblockMetadata metadata;

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
                //dbgln_if(WEBP_DEBUG, "segment_id {}", segment_id);
            }
            if (mb_no_skip_coeff) {
                u8 mb_skip_coeff = TRY(B(prob_skip_false));
                dbgln_if(WEBP_DEBUG, "mb_skip_coeff {}", mb_skip_coeff);
                skip_coefficients = mb_skip_coeff;
            }

            metadata.segment_id = segment_id;
            metadata.skip_coefficients = skip_coefficients;

            const Prob kf_ymode_prob [num_ymodes - 1] = { 145, 156, 163, 128};
            int intra_y_mode = TRY(TreeDecoder(kf_ymode_tree).read(decoder, kf_ymode_prob));
            //dbgln_if(WEBP_DEBUG, "intra_y_mode {} mb_y {} mb_x {}", intra_y_mode, mb_y, mb_x);

            metadata.intra_y_mode = (intra_mbmode)intra_y_mode;

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
                        //dbgln_if(WEBP_DEBUG, "A {} L {} intra_b_mode {} y {} x {}", A, L, (int)intra_b_mode, y, x);
                        metadata.intra_b_modes[y * 4 + x] = intra_b_mode;

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
            //dbgln_if(WEBP_DEBUG, "uv_mode {} mb_y {} mb_x {}", uv_mode, mb_y, mb_x);
            metadata.uv_mode = (intra_mbmode)uv_mode;

            TRY(macroblock_metadata.try_append(metadata));
        }
    }

    dbgln_if(WEBP_DEBUG, "stream offset {} size {}", TRY(memory_stream.tell()), TRY(memory_stream.size()));

    // Done with the first partition!

    if (number_of_dct_partitions > 1)
        return Error::from_string_literal("WebPImageDecoderPlugin: decoding lossy webps with more than one dct partition not yet implemented");


    // this can make the bitmap a bit too big; it's shrunk later if needed.
    auto bitmap = TRY(Bitmap::create(bitmap_format, { macroblock_width * 16, macroblock_height * 16 }));

    // The second partition stores coefficients for all macroblocks.
    {
        FixedMemoryStream memory_stream { vp8_header.second_partition };
        BigEndianInputBitStream bit_stream { MaybeOwned<Stream>(memory_stream) };
        auto decoder = TRY(BooleanEntropyDecoder::initialize(bit_stream));

        // https://datatracker.ietf.org/doc/html/rfc6386#section-13.2 "Coding of Individual Coefficient Values"
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

        using Coefficients = i16[16];

        // Store if each plane has nonzero coefficients in the block above and to the left of the current block.
        Vector<bool> y2_above;
        TRY(y2_above.try_resize(macroblock_width));

        Vector<bool> y_above;
        TRY(y_above.try_resize(macroblock_width * 4));

        Vector<bool> u_above;
        TRY(u_above.try_resize(macroblock_width * 2));

        Vector<bool> v_above;
        TRY(v_above.try_resize(macroblock_width * 2));

        Vector<i16> predicted_y_above;
        TRY(predicted_y_above.try_resize(macroblock_width * 16));
        for (size_t i = 0; i < predicted_y_above.size(); ++i)
            predicted_y_above[i] = 127;

        for (int mb_y = 0, macroblock_index = 0; mb_y < macroblock_height; ++mb_y) {

            bool y2_left {};
            bool y_left[4] {};
            bool u_left[2] {};
            bool v_left[2] {};

            i16 predicted_y_left[16] { 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129 };
            i16 y_truemotion_corner = 127;  // XXX spec doesn't say if this should be 127, 129, or something else :/

            for (int mb_x = 0; mb_x < macroblock_width; ++mb_x, ++macroblock_index) {
                dbgln_if(WEBP_DEBUG, "VP8DecodeMB {} {}", mb_x, mb_y);

                Coefficients y2_coeffs {};
                Coefficients y_coeffs[16] {};
                Coefficients u_coeffs[4] {};
                Coefficients v_coeffs[4] {};

                // See also https://datatracker.ietf.org/doc/html/rfc6386#section-19.3, residual_data() and residual_block()

                auto const& metadata = macroblock_metadata[macroblock_index];

                // XXX test this (add an Error:: return, check it gets hit during decoding, etc)
                if (metadata.skip_coefficients) {
                    // XXX when implementing this, update *_above / *_left here.
                    return Error::from_string_literal("XXX impl and test coefficient skipping");
                    //continue;
                }

                // "firstCoeff is 1 for luma blocks of macroblocks containing Y2 subblock; otherwise 0"

                // https://datatracker.ietf.org/doc/html/rfc6386#section-13

                // "For all intra- and inter-prediction modes apart from B_PRED (intra:
                //  whose Y subblocks are independently predicted) and SPLITMV (inter),
                //  each macroblock's residue record begins with the Y2 component of the
                //  residue, coded using a WHT.  B_PRED and SPLITMV coded macroblocks
                //  omit this WHT and specify the 0th DCT coefficient in each of the 16 Y
                //  subblocks."
                bool have_y2 = metadata.intra_y_mode != B_PRED;

                // "for Y2, because macroblocks above and to the left may or may not have
                //  a Y2 block, the block above is determined by the most recent
                //  macroblock in the same column that has a Y2 block, and the block to
                //  the left is determined by the most recent macroblock in the same row
                //  that has a Y2 block."
                // We only write to y2_above / y2_left when it's present, so we don't need to do any explicit work to get the right behavior.

                // "After the optional Y2 block, the residue record continues with 16
                //  DCTs for the Y subblocks, followed by 4 DCTs for the U subblocks,
                //  ending with 4 DCTs for the V subblocks.  The subblocks occur in the
                //  usual order."

                /* (1 Y2)?, 16 Y, 4 U, 4 V */
                // Corresponds to `residual_data()` in https://datatracker.ietf.org/doc/html/rfc6386#section-19.3
                for (int i = have_y2 ? 0 : 1; i < 25; ++i) {

                    bool subblock_has_nonzero_coefficients = false;

                    u8 sub_x, sub_y;
                    if (i >= 1 && i <= 16) {
                        sub_x = (i - 1) % 4;
                        sub_y = (i - 1) / 4;
                    } else if (i >= 17 && i <= 20) {
                        sub_x = (i - 17) % 2;
                        sub_y = (i - 17) / 2;
                    } else if (i >= 21) {
                        sub_x = (i - 21) % 2;
                        sub_y = (i - 21) / 2;
                    }

                    bool is_u = i >= 17 && i <= 20;
                    bool is_v = i >= 21;
                    bool is_y2 = i == 0;

                    if (is_y2) dbg("y2:");
                    else if (is_u || is_v) dbg("uv {}:", (i - 17) % 4);
                    else  dbg("y {:2}:", (i - 1));

                    // Corresponds to `residual_block()` in https://datatracker.ietf.org/doc/html/rfc6386#section-19.3
                    // "firstCoeff is 1 for luma blocks of macroblocks containing Y2 subblock; otherwise 0"
                    int firstCoeff = have_y2 && (i >= 1 && i <= 16) ? 1 : 0;
                    i16 last_decoded_value = num_dct_tokens; // Start with an invalid value
                    for (int j = firstCoeff; j < 16; ++j) {
                        // https://datatracker.ietf.org/doc/html/rfc6386#section-13.2 "Coding of Individual Coefficient Values"
                        // https://datatracker.ietf.org/doc/html/rfc6386#section-13.3 "Token Probabilities"

                        // "Working from the outside in, the outermost dimension is indexed by
                        //  the type of plane being decoded:
                        //  o  0 - Y beginning at coefficient 1 (i.e., Y after Y2)
                        //  o  1 - Y2
                        //  o  2 - U or V
                        //  o  3 - Y beginning at coefficient 0 (i.e., Y in the absence of Y2)."
                        bool is_y_after_y2 = i >= 1 && i <= 16 && have_y2;
                        bool is_u_or_v = i > 16;
                        bool is_y_without_y2 = i >= 1 && i <= 16 && 1 && !have_y2;
                        int plane;
                        if (is_y_after_y2)
                            plane = 0;
                        else if (is_y2)
                            plane = 1;
                        else if (is_u_or_v)
                            plane = 2;
                        else {
                            VERIFY(is_y_without_y2);
                            plane = 3;
                        }

                        // "The next dimension is selected by the position of the coefficient
                        //  being decoded.  That position, c, steps by ones up to 15, starting
                        //  from zero for block types 1, 2, or 3 and starting from one for block
                        //  type 0.  The second array index is then"
                        // "block type" here seems to refer to the "type of plane" in the previous paragraph.
                        const int coeff_bands[16] = { 0, 1, 2, 3, 6, 4, 5, 6, 6, 6, 6, 6, 6, 6, 6, 7 };
                        int band = coeff_bands[j];

                        // "The third dimension is the trickiest."
                        int tricky = 0;

                        // "For the first coefficient (DC, unless the block type is 0), we
                        //  consider the (already encoded) blocks within the same plane (Y2, Y,
                        //  U, or V) above and to the left of the current block.  The context
                        //  index is then the number (0, 1, or 2) of these blocks that had at
                        //  least one non-zero coefficient in their residue record.  Specifically
                        //  for Y2, because macroblocks above and to the left may or may not have
                        //  a Y2 block, the block above is determined by the most recent
                        //  macroblock in the same column that has a Y2 block, and the block to
                        //  the left is determined by the most recent macroblock in the same row
                        //  that has a Y2 block.
                        //  [...]
                        //  As with other contexts used by VP8, the "neighboring block" context
                        //  described here needs a special definition for subblocks lying along
                        //  the top row or left edge of the frame.  These "non-existent"
                        //  predictors above and to the left of the image are simply taken to be
                        //  empty -- that is, taken to contain no non-zero coefficients."
                        if (j == firstCoeff) {
                            if (is_y2) {
                                if (y2_left) ++tricky;
                                if (y2_above[mb_x]) ++tricky;
                            } else if (is_u) {
                                if (u_left[sub_y]) ++tricky;
                                if (u_above[mb_x * 2 + sub_x]) ++tricky;
                            } else if (is_v) {
                                if (v_left[sub_y]) ++tricky;
                                if (v_above[mb_x * 2 + sub_x]) ++tricky;
                            } else { // Y
                                if (y_left[sub_y]) ++tricky;
                                if (y_above[mb_x * 4 + sub_x]) ++tricky;
                            }
                        }

                        // "Beyond the first coefficient, the context index is determined by the
                        //  absolute value of the most recently decoded coefficient (necessarily
                        //  within the current block) and is 0 if the last coefficient was a
                        //  zero, 1 if it was plus or minus one, and 2 if its absolute value
                        //  exceeded one."
                        if (j > firstCoeff) {
                            if (last_decoded_value == 0)
                                tricky = 0;
                            else if (last_decoded_value == 1 || last_decoded_value == -1)
                                tricky = 1;
                            else
                                tricky = 2;
                        }

                        // "In general, all DCT coefficients are decoded using the same tree.
                        //  However, if the preceding coefficient is a DCT_0, decoding will skip
                        //  the first branch, since it is not possible for dct_eob to follow a
                        //  DCT_0."

//dbg("coeffs at {} {} {}:", plane, band, tricky);
//for (size_t i = 0; i < sizeof(coeff_probs[plane][band][tricky]); ++i)
//dbg(" {}", coeff_probs[plane][band][tricky][i]);
//dbgln();

                        int token;
                        if (last_decoded_value == DCT_0)
                            token = TRY(TreeDecoder(coeff_tree).read(decoder, coeff_probs[plane][band][tricky], 2));
                        else
                            token = TRY(TreeDecoder(coeff_tree).read(decoder, coeff_probs[plane][band][tricky]));
//dbgln_if(WEBP_DEBUG, "token {} at j {} i {} mb_y {} mb_x {}", token, j, i, mb_y, mb_x);

                        if (token == dct_eob) {
                            dbg(" eob");
                            break;
                        }

                        int v = (int)token; // For DCT_0 to DCT4

                        if (token >= dct_cat1 && token <= dct_cat6) {
                            int starts[] = { 5, 7, 11, 19, 35, 67 };
                            //int sizes[] = { 2, 4, 8, 16, 32, 1982 };
                            int bits[] = { 1, 2, 3, 4, 5, 11 };

                            Prob const Pcat1[] = { 159 };
                            Prob const Pcat2[] = { 165, 145 };
                            Prob const Pcat3[] = { 173, 148, 140 };
                            Prob const Pcat4[] = { 176, 155, 140, 135 };
                            Prob const Pcat5[] = { 180, 157, 141, 134, 130 };
                            Prob const Pcat6[] = { 254, 254, 243, 230, 196, 177, 153, 140, 133, 130, 129 };
                            Prob const* const Pcats[] = { Pcat1, Pcat2, Pcat3, Pcat4, Pcat5, Pcat6 };

                            v = 0;
                            for (int i = 0; i < bits[token - dct_cat1]; ++i)
                                v = (v << 1) | TRY(decoder.read_bool(Pcats[token - dct_cat1][i]));

                            v += starts[token - dct_cat1];

//dbgln_if(WEBP_DEBUG, "v {}", v);
                        }

                        if (v) {
                            // Subblock has non-0 coefficients. Store that, so that `tricky` on the next subblock is initialized correctly.
                            subblock_has_nonzero_coefficients = true;

                            if (TRY(decoder.read_bool(128)))
                                v = -v;
                        }

                        // last_decoded_value is used for setting `tricky`. It needs to be set to the last decoded token, not to the last dequantized value.
                        last_decoded_value = v;

                        // https://datatracker.ietf.org/doc/html/rfc6386#section-9.6 "Dequantization Indices"
                        // "before inverting the transform, each decoded coefficient
                        //  is multiplied by one of six dequantization factors, the choice of
                        //  which depends on the plane (Y, chroma = U or V, Y2) and coefficient
                        //  position (DC = coefficient 0, AC = coefficients 1-15).  The six
                        //  values are specified using 7-bit indices into six corresponding fixed
                        //  tables (the tables are given in Section 14)."
                        // Section 14 then lists two fixed tables (which are in WebPLoaderLossyTables.h)

                        // "Lookup values from the above two tables are directly used in the DC
                        //  and AC coefficients in Y1, respectively.  For Y2 and chroma, values
                        //  from the above tables undergo either scaling or clamping before the
                        //  multiplies.  Details regarding these scaling and clamping processes
                        //  can be found in related lookup functions in dixie.c (Section 20.4)."
                        // Apparently spec writing became too much work at this point. In section 20.4, in dequant_init():
                        // * For y2, the output (!) of dc_qlookup is multiplied by 2, the output of ac_qlookup is multiplied by 155 / 100
                        // * Also for y2, ac_qlookup is at least 8 for lower table entries
                        // * For uv, the dc_qlookup index is clamped to 117 (instead of 127 for everything else)
                        //   (or, alternatively, the value is clamped to 132 at most)

                        u8 dequantization_index;
                        if (is_y2)
                            dequantization_index = j == 0 ? quantization_indices.y2_dc : quantization_indices.y2_ac;
                        else if (is_u_or_v)
                            dequantization_index = j == 0 ? quantization_indices.uv_dc : quantization_indices.uv_ac;
                        else
                            dequantization_index = j == 0 ? quantization_indices.y_dc : quantization_indices.y_ac;

    #if 0
                        if (update_mb_segmentation_map) {
                            auto const& data = segment_data[metadata.segment_id];
                            if (data.quantizer_update_value.has_value()) {
                                if (segment_feature_mode == SegmentFeatureMode::DeltaValueMode)
                                    dequantization_index += data.quantizer_update_value.value();
                                else
                                    dequantization_index = data.quantizer_update_value.value();
                            }
                        }
    #endif

                        // clamp index
                        if (is_u_or_v && j == 0)
                            dequantization_index = min(dequantization_index, 117);
                        else
                            dequantization_index = min(dequantization_index, 127);

                        // "the multiplies are computed and stored using 16-bit signed integers."
                        i16 dequantized_value;
                        if (j == 0)
                            dequantized_value = (i16)dc_qlookup[dequantization_index] * (i16)v;
                        else
                            dequantized_value = (i16)ac_qlookup[dequantization_index] * (i16)v;

                        if (is_y2) {
                            if (j == 0)
                                dequantized_value *= 2;
                            else
                                dequantized_value = (dequantized_value * 155) / 100;  // XXX this is wrong somehow, see `VP8DecodeMB 0 1` (but that doesn't affect bitstream decoding)
                        }

// XXX v is off in VP8DecodeMB 22 1

//dbgln_if(WEBP_DEBUG, "dequantized {} index {}", dequantized_value, dequantization_index);
                        dbg(" {}", dequantized_value);

                        if (is_y2)
                            y2_coeffs[j] = dequantized_value;
                        else if (is_u)
                            u_coeffs[i - 17][j] = dequantized_value;
                        else if (is_v)
                            v_coeffs[i - 21][j] = dequantized_value;
                        else // Y
                            y_coeffs[i - 1][j] = dequantized_value;


                    }
                    dbgln();

                    if (is_y2) {
                        y2_left = subblock_has_nonzero_coefficients;
                        y2_above[mb_x] = subblock_has_nonzero_coefficients;
                    } else if (is_u) {
                        u_left[sub_y] = subblock_has_nonzero_coefficients;
                        u_above[mb_x * 2 + sub_x] = subblock_has_nonzero_coefficients;
                    } else if (is_v) {
                        v_left[sub_y] = subblock_has_nonzero_coefficients;
                        v_above[mb_x * 2 + sub_x] = subblock_has_nonzero_coefficients;
                    } else { // Y
                        y_left[sub_y] = subblock_has_nonzero_coefficients;
                        y_above[mb_x * 4 + sub_x] = subblock_has_nonzero_coefficients;
                    }
                }

                // https://datatracker.ietf.org/doc/html/rfc6386#section-14.2 "Inverse Transforms"
                // "If the Y2 residue block exists (i.e., the macroblock luma mode is not
                //  SPLITMV or B_PRED), it is inverted first (using the inverse WHT) and
                //  the element of the result at row i, column j is used as the 0th
                //  coefficient of the Y subblock at position (i, j), that is, the Y
                //  subblock whose index is (i * 4) + j."
                if (have_y2) {
                    Coefficients wht_output;
                    vp8_short_inv_walsh4x4_c(y2_coeffs, wht_output);
                    for (size_t i = 0; i < 16; ++i)
                        y_coeffs[i][0] = wht_output[0];
                }

                i16 y_prediction[16 * 16] {};
                if (metadata.intra_y_mode == DC_PRED) {
                    if (mb_x == 0 && mb_y == 0) {
                        for (size_t i = 0; i < sizeof(y_prediction) / sizeof(y_prediction[0]); ++i)
                            y_prediction[i] = 128;
                    } else {
                        int sum = 0, n = 0;
                        if (mb_x > 0) {
                            for (int i = 0; i < 16; ++i)
                                sum += predicted_y_left[i];
                            n += 16;
                        }
                        if (mb_y > 0) {
                            for (int i = 0; i < 16; ++i)
                                sum += predicted_y_above[mb_x * 16 + i];
                            n += 16;
                        }
                        i16 average = (sum + n/2) / n;
                        for (size_t i = 0; i < sizeof(y_prediction) / sizeof(y_prediction[0]); ++i)
                            y_prediction[i] = average;
                    }
                } else if (metadata.intra_y_mode == H_PRED) {
                    for (int y = 0; y < 16; ++y)
                        for (int x = 0; x < 16; ++x)
                            y_prediction[y * 16 + x] = predicted_y_left[y];
                } else if (metadata.intra_y_mode == V_PRED) {
                    for (int y = 0; y < 16; ++y)
                        for (int x = 0; x < 16; ++x)
                            y_prediction[y * 16 + x] = predicted_y_above[mb_x * 16 + x];
                } else if (metadata.intra_y_mode == TM_PRED) {
                    for (int y = 0; y < 16; ++y)
                        for (int x = 0; x < 16; ++x)
                            y_prediction[y * 16 + x] = predicted_y_left[y] + predicted_y_above[mb_x * 16 + x] - y_truemotion_corner;
                } else {
                    VERIFY(metadata.intra_y_mode == B_PRED);
                    // Loop over the 4x4 subblocks
                    for (int y = 0, i = 0; y < 4; ++y) {
                        for (int x = 0; x < 4; ++x, ++i) {
                            i16 left[4], above[4];
                            if (x == 0) {
                                for (int i = 0; i < 4; ++i)
                                    left[i] = predicted_y_left[4 * y + i];
                            } else {
                                for (int i = 0; i < 4; ++i)
                                    left[i] = y_prediction[(4 * y + i) * 16 + 4 * x - 1];
                            }
                            if (y == 0) {
                                for (int i = 0; i < 4; ++i)
                                    above[i] = predicted_y_above[mb_x * 16 + 4 * x + i];
                            } else {
                                for (int i = 0; i < 4; ++i)
                                    above[i] = y_prediction[(4 * y - 1) * 16 + 4 * x + i];
                            }

                            auto mode = metadata.intra_b_modes[y * 4 + x];
                            if (mode == B_DC_PRED) {
                                // XXX spec text says this is like DC_PRED but predict_dc_nxn() in the sample impl looks like it doesn't do the "oob isn't read" part. what's right?
                                // DC16NoTopLeft_C vs DC4_C in libwebp dec.c / common_dec.h suggests the spec text is incomplete :/
                                int sum = 0, n = 8;
                                for (int i = 0; i < 4; ++i)
                                    sum += left[i] + above[i];
                                i16 average = (sum + n/2) / n;
                                for (int py = 0; py < 4; ++py)
                                    for (int px = 0; px < 4; ++px)
                                        y_prediction[(4 * y + py) * 16 + 4 * x + px] = average;
                            } else if (mode == B_HE_PRED) {
                                // XXX this should be using averages
                                for (int py = 0; py < 4; ++py)
                                    for (int px = 0; px < 4; ++px)
                                        y_prediction[(4 * y + py) * 16 + 4 * x + px] = left[py];
                            } else if (mode == B_VE_PRED) {
                                // XXX this should be using averages
                                for (int py = 0; py < 4; ++py)
                                    for (int px = 0; px < 4; ++px)
                                        y_prediction[(4 * y + py) * 16 + 4 * x + px] = above[px];
                            } else if (mode == B_TM_PRED) {
                                i16 corner = y_truemotion_corner;
                                if (x > 0 && y == 0)
                                    corner = predicted_y_above[mb_x * 16 + 4 * x - 1];
                                else if (x > 0 && y > 0)
                                    corner = y_prediction[(4 * y - 1) * 16 + 4 * x - 1];

                                for (int py = 0; py < 4; ++py)
                                    for (int px = 0; px < 4; ++px)
                                        y_prediction[(4 * y + py) * 16 + 4 * x + px] = left[py] + above[px] - corner;
                            } else {
                                dbgln("unhandled {}", (int)mode);
                                //for (int py = 0; py < 4; ++py)
                                    //for (int px = 0; px < 4; ++px)
                                        //y_prediction[(4 * y + py) * 16 + 4 * x + px] = left[py];
                            }
                        }
                    }
                }

                // https://datatracker.ietf.org/doc/html/rfc6386#section-14.4 "Implementation of the DCT Inversion"
                // Loop over the 4x4 subblocks
                for (int y = 0, i = 0; y < 4; ++y) {
                    for (int x = 0; x < 4; ++x, ++i) {
                        Coefficients idct_output;
                        short_idct4x4llm_c(y_coeffs[i], idct_output, 4 * sizeof(i16));

                        // https://datatracker.ietf.org/doc/html/rfc6386#section-14.5 "Summation of Predictor and Residue"
                        for (int py = 0; py < 4; ++py) { // Loop over 4x4 pixels in subblock
                            for (int px = 0; px < 4; ++px) {
                                // sum with prediction
                                i16& p = y_prediction[(4 * y + py) * 16 + (4 * x + px)];
                                p += idct_output[py * 4 + px];
                                // "is then saturated to 8-bit unsigned range (using, say, the
                                //  clamp255 function defined above) before being stored as an 8-bit
                                //  unsigned pixel value."
                                u8 Y = clamp(p, 0, 255);
                                bitmap->scanline(mb_y * 16 + y * 4 + py)[mb_x * 16 + x * 4 + px] = Color(Y, Y, Y).value();
                            }
                        }
                    }
                }

                y_truemotion_corner = predicted_y_above[mb_x * 16 + 15];
                for (int i = 0; i < 16; ++i)
                    predicted_y_left[i] = y_prediction[15 + i * 16];
                for (int i = 0; i < 16; ++i)
                    predicted_y_above[mb_x * 16 + i] = y_prediction[15 * 16 + i];
            }
        }

        dbgln_if(WEBP_DEBUG, "stream 2 offset {} size {}", TRY(memory_stream.tell()), TRY(memory_stream.size()));
    }

    // XXX sink this check into Bitmap::cropped()
    if (bitmap->physical_width() != (int)vp8_header.width || bitmap->physical_height() != (int)vp8_header.height)
        return bitmap->cropped({ 0, 0, (int)vp8_header.width, (int)vp8_header.height });
    return bitmap;
}

// https://datatracker.ietf.org/doc/html/rfc6386#section-14.3 "Implementation of the WHT Inversion"
static void vp8_short_inv_walsh4x4_c(i16* input, i16* output)
{
    i16 *ip = input;
    i16 *op = output;

    for(int i = 0; i < 4; i++) {
        int a1 = ip[0] + ip[12];
        int b1 = ip[4] + ip[8];
        int c1 = ip[4] - ip[8];
        int d1 = ip[0] - ip[12];

        op[0] = a1 + b1;
        op[4] = c1 + d1;
        op[8] = a1 - b1;
        op[12]= d1 - c1;
        ip++;
        op++;
    }

    ip = output;
    op = output;
    for(int i = 0; i < 4; i++) {
        int a1 = ip[0] + ip[3];
        int b1 = ip[1] + ip[2];
        int c1 = ip[1] - ip[2];
        int d1 = ip[0] - ip[3];

        int a2 = a1 + b1;
        int b2 = c1 + d1;
        int c2 = a1 - b1;
        int d2 = d1 - c1;
        op[0] = (a2 + 3) >> 3;
        op[1] = (b2 + 3) >> 3;
        op[2] = (c2 + 3) >> 3;
        op[3] = (d2 + 3) >> 3;

        ip += 4;
        op += 4;
    }
}

// https://datatracker.ietf.org/doc/html/rfc6386#section-14.4 "Implementation of the DCT Inversion"
static void short_idct4x4llm_c(i16* input, i16* output, int pitch)
{
    static constexpr int cospi8sqrt2minus1 = 20091;
    static constexpr int sinpi8sqrt2       = 35468;

    i16* ip = input;
    i16* op = output;
    int shortpitch = pitch >> 1;

    for(int i = 0; i < 4; i++) {
        int a1 = ip[0] + ip[8];
        int b1 = ip[0] - ip[8];

        int temp1 = (ip[4] * sinpi8sqrt2) >> 16;
        int temp2 = ip[12] + ((ip[12] * cospi8sqrt2minus1) >> 16);
        int c1 = temp1 - temp2;

        temp1 = ip[4] + ((ip[4] * cospi8sqrt2minus1) >> 16);
        temp2 = (ip[12] * sinpi8sqrt2) >> 16;
        int d1 = temp1 + temp2;

        op[shortpitch * 0] = a1 + d1;
        op[shortpitch * 3] = a1 - d1;
        op[shortpitch * 1] = b1 + c1;
        op[shortpitch * 2] = b1 - c1;

        ip++;
        op++;
    }

    ip = output;
    op = output;
    for(int i = 0; i < 4; i++) {
        int a1 = ip[0] + ip[2];
        int b1 = ip[0] - ip[2];

        int temp1 = (ip[1] * sinpi8sqrt2) >> 16;
        int temp2 = ip[3] + ((ip[3] * cospi8sqrt2minus1) >> 16);
        int c1 = temp1 - temp2;

        temp1 = ip[1] + ((ip[1] * cospi8sqrt2minus1) >> 16);
        temp2 = (ip[3] * sinpi8sqrt2) >> 16;
        int d1 = temp1 + temp2;

        op[0] = (a1 + d1 + 4) >> 3;
        op[3] = (a1 - d1 + 4) >> 3;
        op[1] = (b1 + c1 + 4) >> 3;
        op[2] = (b1 - c1 + 4) >> 3;

        ip += shortpitch;
        op += shortpitch;
    }
}

}
