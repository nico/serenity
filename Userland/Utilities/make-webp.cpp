/*
 * Copyright (c) 2024, Nico Weber <thakis@chromium.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

// Container: https://developers.google.com/speed/webp/docs/riff_container
// Lossless format: https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification

#include <AK/Endian.h>
#include <AK/BitStream.h>
#include <LibCore/ArgsParser.h>
#include <LibCore/MappedFile.h>
#include <LibCore/File.h>
#include <LibCompress/DeflateTables.h>
#include <LibGfx/Bitmap.h>

struct Options {
    StringView out_path;
    int width { 512 };
    int height { 512 };
};

// https://developers.google.com/speed/webp/docs/riff_container#webp_file_header
static ErrorOr<void> write_webp_header(Stream& stream, unsigned data_size)
{
    TRY(stream.write_until_depleted("RIFF"sv));
    TRY(stream.write_value<LittleEndian<u32>>(4 + data_size)); // Including size of "WEBP" and the data size itself.
    TRY(stream.write_until_depleted("WEBP"sv));
    return {};
}

static ErrorOr<void> write_chunk_header(Stream& stream, StringView chunk_fourcc, unsigned vp8l_data_size)
{
    TRY(stream.write_until_depleted(chunk_fourcc));
    TRY(stream.write_value<LittleEndian<u32>>(vp8l_data_size));
    return {};
}

// https://developers.google.com/speed/webp/docs/riff_container#simple_file_format_lossless
// https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#7_overall_structure_of_the_format
static ErrorOr<void> write_VP8L_header(Stream& stream, unsigned width, unsigned height, bool alpha_hint)
{
    // "The 14-bit precision for image width and height limits the maximum size of a WebP lossless image to 16384✕16384 pixels."
    if (width > 16384 || height > 16384)
        return Error::from_string_literal("WebP lossless images can't be larger than 16384x16384 pixels");

    if (width == 0 || height == 0)
        return Error::from_string_literal("WebP lossless images must be at least one pixel wide and tall");

    LittleEndianOutputBitStream bit_stream { MaybeOwned<Stream>(stream) };

    // Signature byte.
    TRY(bit_stream.write_bits(0x2fu, 8u)); // Signature byte

    // 14 bits width-1, 14 bits height-1, 1 bit alpha hint, 3 bit version_number.
    TRY(bit_stream.write_bits(width - 1, 14u));
    TRY(bit_stream.write_bits(height - 1, 14u));

    // "The alpha_is_used bit is a hint only, and should not impact decoding.
    //  It should be set to 0 when all alpha values are 255 in the picture, and 1 otherwise."
    TRY(bit_stream.write_bits(alpha_hint, 1u));

    // "The version_number is a 3 bit code that must be set to 0."
    TRY(bit_stream.write_bits(0u, 3u));

    // FIXME: Make ~LittleEndianOutputBitStream do this, or make it VERIFY() that it has happened at least.
    TRY(bit_stream.flush_buffer_to_stream());

    return {};
}

static ErrorOr<void> write_VP8L_image_data(Stream& stream)
{
    LittleEndianOutputBitStream bit_stream { MaybeOwned<Stream>(stream) };

    // optional-transform   =  (%b1 transform optional-transform) / %b0
    TRY(bit_stream.write_bits(0u, 1u)); // No transform for now.

    // https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#5_image_data
    // spatially-coded-image =  color-cache-info meta-prefix data

    // color-cache-info      =  %b0
    // color-cache-info      =/ (%b1 4BIT) ; 1 followed by color cache size
    TRY(bit_stream.write_bits(0u, 1u)); // No color cache for now.

    // meta-prefix           =  %b0 / (%b1 entropy-image)
    TRY(bit_stream.write_bits(0u, 1u)); // No meta prefix for now.

    u8 r = 255;
    u8 g = 0;
    u8 b = 0;
    u8 a = 255;

    // G
    TRY(bit_stream.write_bits(1u, 1u)); // Simple code length code.
    TRY(bit_stream.write_bits(0u, 1u)); // num_symbols - 1
    TRY(bit_stream.write_bits(1u, 1u)); // is_first_8bits
    TRY(bit_stream.write_bits(g, 8u)); // symbol0

    // R
    TRY(bit_stream.write_bits(1u, 1u)); // Simple code length code.
    TRY(bit_stream.write_bits(0u, 1u)); // num_symbols - 1
    TRY(bit_stream.write_bits(1u, 1u)); // is_first_8bits
    TRY(bit_stream.write_bits(r, 8u)); // symbol0

    // B
    TRY(bit_stream.write_bits(1u, 1u)); // Simple code length code.
    TRY(bit_stream.write_bits(0u, 1u)); // num_symbols - 1
    TRY(bit_stream.write_bits(1u, 1u)); // is_first_8bits
    TRY(bit_stream.write_bits(b, 8u)); // symbol0

    // A
    TRY(bit_stream.write_bits(1u, 1u)); // Simple code length code.
    TRY(bit_stream.write_bits(0u, 1u)); // num_symbols - 1
    TRY(bit_stream.write_bits(1u, 1u)); // is_first_8bits
    TRY(bit_stream.write_bits(a, 8u)); // symbol0

    // Distance codes (unused).
    TRY(bit_stream.write_bits(1u, 1u)); // Simple code length code.
    TRY(bit_stream.write_bits(0u, 1u)); // num_symbols - 1
    TRY(bit_stream.write_bits(0u, 1u)); // is_first_8bits
    TRY(bit_stream.write_bits(0u, 1u)); // symbol0

    // FIXME: Make ~LittleEndianOutputBitStream do this, or make it VERIFY() that it has happened at least.
    TRY(bit_stream.align_to_byte_boundary());
    TRY(bit_stream.flush_buffer_to_stream());

    return {};
}

// FIXME: Consider using LibRIFF for RIFF writing details. (It currently has no writing support.)
static ErrorOr<void> align_to_two(AllocatingMemoryStream& stream)
{
    // https://developers.google.com/speed/webp/docs/riff_container
    // "If Chunk Size is odd, a single padding byte -- which MUST be 0 to conform with RIFF -- is added."
    if (stream.used_buffer_size() % 2 != 0)
        TRY(stream.write_value<u8>(0));
    return {};
}

ErrorOr<void> write_webp(Stream& stream, Options const& options)
{
    // The chunk headers need to know their size, so we either need a SeekableStream or need to buffer the data. We're doing the latter.
    // FIXME: The whole writing-and-reading-into-buffer over-and-over is awkward and inefficient.
    AllocatingMemoryStream vp8l_header_stream;
    TRY(write_VP8L_header(vp8l_header_stream, options.width, options.height, true));
    auto vp8l_header_bytes = TRY(vp8l_header_stream.read_until_eof());

    AllocatingMemoryStream vp8l_data_stream;
    TRY(write_VP8L_image_data(vp8l_data_stream));
    auto vp8l_data_bytes = TRY(vp8l_data_stream.read_until_eof());

    AllocatingMemoryStream vp8l_chunk_stream;
    TRY(write_chunk_header(vp8l_chunk_stream, "VP8L"sv, vp8l_header_bytes.size() + vp8l_data_bytes.size()));
    TRY(vp8l_chunk_stream.write_until_depleted(vp8l_header_bytes));
    TRY(vp8l_chunk_stream.write_until_depleted(vp8l_data_bytes));
    TRY(align_to_two(vp8l_chunk_stream));
    auto vp8l_chunk_bytes = TRY(vp8l_chunk_stream.read_until_eof());

    TRY(write_webp_header(stream, vp8l_chunk_bytes.size()));
    TRY(stream.write_until_depleted(vp8l_chunk_bytes));
    return {};
}

static ErrorOr<Options> parse_options(Main::Arguments arguments)
{
    Options options;
    Core::ArgsParser args_parser;
    args_parser.add_option(options.out_path, "Path to output image file", "output", 'o', "FILE");
    args_parser.parse(arguments);

    if (options.out_path.is_empty())
        return Error::from_string_view("-o is required"sv);

    return options;
}

ErrorOr<int> serenity_main(Main::Arguments arguments)
{
    Options options = TRY(parse_options(arguments));

    auto output_stream = TRY(Core::File::open(options.out_path, Core::File::OpenMode::Write));

    TRY(write_webp(*TRY(Core::OutputBufferedFile::create(move(output_stream))), options));

    return 0;
}
