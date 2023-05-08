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

    return VP8Header { version, show_frame, size_of_first_partition, width, horizontal_scale, height, vertical_scale, vp8_data.slice(10) };
}

ErrorOr<NonnullRefPtr<Bitmap>> decode_webp_chunk_VP8_contents(VP8Header const& vp8_header, bool include_alpha_channel)
{
    auto bitmap_format = include_alpha_channel ? BitmapFormat::BGRA8888 : BitmapFormat::BGRx8888;

    FixedMemoryStream memory_stream { vp8_header.lossy_data };
    LittleEndianInputBitStream bit_stream { MaybeOwned<Stream>(memory_stream) };

    // https://datatracker.ietf.org/doc/html/rfc6386#section-19.2

    // https://datatracker.ietf.org/doc/html/rfc6386#section-9.2
    enum class ColorSpaceAndPixelType {
        YUV = 0,
        ReservedForFutureUse = 1,
    };
    auto color_space = static_cast<ColorSpaceAndPixelType>(TRY(bit_stream.read_bits(1)));

    enum class ClampingSpecification {
        DecoderMustClampTo0To255 = 0,
        NoClampingNecessary = 1,
    };
    auto clamping_type = static_cast<ClampingSpecification>(TRY(bit_stream.read_bits(1)));

    dbgln_if(WEBP_DEBUG, "color_space {} clamping_type {}", (int)color_space, (int)clamping_type);


    // https://datatracker.ietf.org/doc/html/rfc6386#section-9.3
    u8 segmentation_enabled = TRY(bit_stream.read_bits(1));
    if (segmentation_enabled) {
        // "update_segmentation()" in 19.2

        // FIXME: Is this always true for keyframes in webp files? Should we return an Error if this is 0 instead?
        u8 update_mb_segmentation_map = TRY(bit_stream.read_bits(1));
        u8 update_segment_feature_data = TRY(bit_stream.read_bits(1));

        dbgln_if(WEBP_DEBUG, "update_mb_segmentation_map {} update_segment_feature_data {}",
            update_mb_segmentation_map, update_segment_feature_data);

        if (update_segment_feature_data) {
            enum class SegmentFeatureMode {
                AbsoluteValueMode = 0,
                DeltaValueMode = 1,
            };
            auto segment_feature_mode = static_cast<SegmentFeatureMode>(TRY(bit_stream.read_bits(1)));
            dbgln_if(WEBP_DEBUG, "segment_feature_mode {}", (int)segment_feature_mode);

            for (int i = 0; i < 4; ++i) {
                u8 quantizer_update = TRY(bit_stream.read_bits(1));
                dbgln_if(WEBP_DEBUG, "quantizer_update {}", quantizer_update);
                if (quantizer_update) {
                    u8 quantizer_update_value = TRY(bit_stream.read_bits(7));
                    u8 quantizer_update_sign = TRY(bit_stream.read_bits(1));
                    dbgln_if(WEBP_DEBUG, "quantizer_update_value {} quantizer_update_sign {}", quantizer_update_value, quantizer_update_sign);
                }
            }
            for (int i = 0; i < 4; ++i) {
                u8 loop_filter_update = TRY(bit_stream.read_bits(1));
                dbgln_if(WEBP_DEBUG, "loop_filter_update {}", loop_filter_update);
                if (loop_filter_update) {
                    u8 loop_filter_update_value = TRY(bit_stream.read_bits(6));
                    u8 loop_filter_update_sign = TRY(bit_stream.read_bits(1));
                    dbgln_if(WEBP_DEBUG, "loop_filter_update_value {} loop_filter_update_sign {}", loop_filter_update_value, loop_filter_update_sign);
                }
            }
        }

        if (update_mb_segmentation_map) {
            for (int i = 0; i < 3; ++i) {
                u8 segment_prob_update = TRY(bit_stream.read_bits(1));
                dbgln_if(WEBP_DEBUG, "segment_prob_update {}", segment_prob_update);
                if (segment_prob_update) {
                    u8 segment_prob = TRY(bit_stream.read_bits(8));
                    dbgln_if(WEBP_DEBUG, "segment_prob {}", segment_prob);
                }
            }
        }
    }

    // https://datatracker.ietf.org/doc/html/rfc6386#section-9.4
    u8 filter_type = TRY(bit_stream.read_bits(1));
    u8 loop_filter_level = TRY(bit_stream.read_bits(6));
    u8 sharpness_level = TRY(bit_stream.read_bits(3));
    dbgln_if(WEBP_DEBUG, "filter_type {} loop_filter_level {} sharpness_level {}", filter_type, loop_filter_level, sharpness_level);

    // "mb_lf_adjustments()" in 19.2
    u8 loop_filter_adj_enable = TRY(bit_stream.read_bits(1));
    dbgln_if(WEBP_DEBUG, "loop_filter_adj_enable {}", loop_filter_adj_enable);
    if (loop_filter_adj_enable) {
        u8 mode_ref_lf_delta_update = TRY(bit_stream.read_bits(1));
        dbgln_if(WEBP_DEBUG, "mode_ref_lf_delta_update {}", mode_ref_lf_delta_update);
        if (mode_ref_lf_delta_update) {
            for (int i = 0; i < 4; ++i) {
                u8 ref_frame_delta_update_flag = TRY(bit_stream.read_bits(1));
                dbgln_if(WEBP_DEBUG, "ref_frame_delta_update_flag {}", ref_frame_delta_update_flag);
                if (ref_frame_delta_update_flag) {
                    u8 delta_magnitude = TRY(bit_stream.read_bits(6));
                    u8 delta_sign = TRY(bit_stream.read_bits(1));
                    dbgln_if(WEBP_DEBUG, "delta_magnitude {} loop_filter_update_sign {}", delta_magnitude, delta_sign);
                }
            }
            for (int i = 0; i < 4; ++i) {
                u8 mb_mode_delta_update_flag = TRY(bit_stream.read_bits(1));
                dbgln_if(WEBP_DEBUG, "mb_mode_delta_update_flag {}", mb_mode_delta_update_flag);
                if (mb_mode_delta_update_flag) {
                    u8 delta_magnitude = TRY(bit_stream.read_bits(6));
                    u8 delta_sign = TRY(bit_stream.read_bits(1));
                    dbgln_if(WEBP_DEBUG, "delta_magnitude {} loop_filter_update_sign {}", delta_magnitude, delta_sign);
                }
            }
        }
    }

    u8 log2_nbr_of_dct_partitions = TRY(bit_stream.read_bits(2));
    dbgln_if(WEBP_DEBUG, "log2_nbr_of_dct_partitions {}", log2_nbr_of_dct_partitions);

    // "quant_indices()" in 19.2
    u8 y_ac_qi = TRY(bit_stream.read_bits(7));
    dbgln_if(WEBP_DEBUG, "y_ac_qi {}", y_ac_qi);

    u8 y_dc_delta_present = TRY(bit_stream.read_bits(1));
    dbgln_if(WEBP_DEBUG, "y_dc_delta_present {}", y_dc_delta_present);
    if (y_dc_delta_present) {
      u8 y_dc_delta_magnitude = TRY(bit_stream.read_bits(4));
      u8 y_dc_delta_sign = TRY(bit_stream.read_bits(1));
      dbgln_if(WEBP_DEBUG, "y_dc_delta_magnitude {} y_dc_delta_sign {}", y_dc_delta_magnitude, y_dc_delta_sign);
    }
    u8 y2_dc_delta_present = TRY(bit_stream.read_bits(1));
    dbgln_if(WEBP_DEBUG, "y2_dc_delta_present {}", y2_dc_delta_present);
    if (y2_dc_delta_present) {
      u8 y2_dc_delta_magnitude = TRY(bit_stream.read_bits(4));
      u8 y2_dc_delta_sign = TRY(bit_stream.read_bits(1));
      dbgln_if(WEBP_DEBUG, "y2_dc_delta_magnitude {} y2_dc_delta_sign {}", y2_dc_delta_magnitude, y2_dc_delta_sign);
    }
    u8 y2_ac_delta_present = TRY(bit_stream.read_bits(1));
    dbgln_if(WEBP_DEBUG, "y2_ac_delta_present {}", y2_ac_delta_present);
    if (y2_ac_delta_present) {
      u8 y2_ac_delta_magnitude = TRY(bit_stream.read_bits(4));
      u8 y2_ac_delta_sign = TRY(bit_stream.read_bits(1));
      dbgln_if(WEBP_DEBUG, "y2_ac_delta_magnitude {} y2_ac_delta_sign {}", y2_ac_delta_magnitude, y2_ac_delta_sign);
    }
    u8 uv_dc_delta_present = TRY(bit_stream.read_bits(1));
    dbgln_if(WEBP_DEBUG, "uv_dc_delta_present {}", uv_dc_delta_present);
    if (uv_dc_delta_present) {
      u8 uv_dc_delta_magnitude = TRY(bit_stream.read_bits(4));
      u8 uv_dc_delta_sign = TRY(bit_stream.read_bits(1));
      dbgln_if(WEBP_DEBUG, "uv_dc_delta_magnitude {} uv_dc_delta_sign {}", uv_dc_delta_magnitude, uv_dc_delta_sign);
    }
    u8 uv_ac_delta_present = TRY(bit_stream.read_bits(1));
    dbgln_if(WEBP_DEBUG, "uv_ac_delta_present {}", uv_ac_delta_present);
    if (uv_ac_delta_present) {
      u8 uv_ac_delta_magnitude = TRY(bit_stream.read_bits(4));
      u8 uv_ac_delta_sign = TRY(bit_stream.read_bits(1));
      dbgln_if(WEBP_DEBUG, "uv_ac_delta_magnitude {} uv_ac_delta_sign {}", uv_ac_delta_magnitude, uv_ac_delta_sign);
    }

    // Always key_frame in webp.
    u8 refresh_entropy_probs = TRY(bit_stream.read_bits(1));
    dbgln_if(WEBP_DEBUG, "refresh_entropy_probs {}", refresh_entropy_probs);

    // "refresh_entropy_probs()" in 19.2
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 8; j++) {
            for (int k = 0; k < 3; k++) {
                for (int l = 0; l < 11; l++) {
                    u8 coeff_prob_update_flag = TRY(bit_stream.read_bits(1));
                    dbgln_if(WEBP_DEBUG, "coeff_prob_update_flag {}", coeff_prob_update_flag);
                    if (coeff_prob_update_flag) {
                        u8 coeff_prob = TRY(bit_stream.read_bits(8));
                        dbgln_if(WEBP_DEBUG, "coeff_prob {}", coeff_prob);
                    }
                }
            }
        }
    }

    u8 mb_no_skip_coeff = TRY(bit_stream.read_bits(1));
    dbgln_if(WEBP_DEBUG, "mb_no_skip_coeff {}", mb_no_skip_coeff);
    if (mb_no_skip_coeff) {
        u8 prob_skip_false = TRY(bit_stream.read_bits(8));
        dbgln_if(WEBP_DEBUG, "prob_skip_false {}", prob_skip_false);
    }

    (void)bitmap_format;
    return Error::from_string_literal("WebPImageDecoderPlugin: decoding lossy webps not yet implemented");
}

}
