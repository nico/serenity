/*
 * Copyright (c) 2024, Nico Weber <thakis@chromium.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

// Container: https://developers.google.com/speed/webp/docs/riff_container
// Lossless format: https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification

#include <AK/Endian.h>
#include <AK/Hex.h>
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
    Gfx::Color color { Color::Red };
    int predictor { -1 }; // -1 means none; valid values are 0-13.
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

static ErrorOr<void> write_VP8L_image_data(Stream& stream, Options const& options)
{
    LittleEndianOutputBitStream bit_stream { MaybeOwned<Stream>(stream) };

    // optional-transform   =  (%b1 transform optional-transform) / %b0

    // XXX try reordering color index transform and predictor transform
    bool write_color_index_transform = true;
    if (write_color_index_transform) {
        TRY(bit_stream.write_bits(1u, 1u)); // Transform present.
        TRY(bit_stream.write_bits(3u, 2u)); // COLOR_INDEXING_TRANSFORM

        // int color_table_size = ReadBits(8) + 1;
        u32 color_table_size = 256; // Interesting values (due to bundling): 1, 2, 4, 16, 256.
        TRY(bit_stream.write_bits(color_table_size - 1, 8u));

        size_t const color_cache_size = 0;
        TRY(bit_stream.write_bits(0u, 1u)); // No color cache.

        // Meta prefix presence is only stored for spatially-coded images (i.e. the main image), not for entropy images (e.g. the color index image).

        constexpr Array alphabet_sizes = to_array<size_t>({ 256 + 24 + static_cast<size_t>(color_cache_size), 256, 256, 256, 40 }); // XXX Shared?

        for (int i = 0; i < 4; ++i) {
            TRY(bit_stream.write_bits(0u, 1u)); // Normal code length code.

            // Write code length codes.
            constexpr int kCodeLengthCodes = 19;
            Array<int, kCodeLengthCodes> kCodeLengthCodeOrder = { 17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
            int num_code_lengths = max(4u, find_index(kCodeLengthCodeOrder.begin(), kCodeLengthCodeOrder.end(), 8) + 1);

            // "int num_code_lengths = 4 + ReadBits(4);"
            TRY(bit_stream.write_bits(num_code_lengths - 4u, 4u));

            for (int i = 0; i < num_code_lengths - 1; ++i)
                TRY(bit_stream.write_bits(0u, 3u));
            TRY(bit_stream.write_bits(1u, 3u));

            // Write code lengths.
            if (alphabet_sizes[i] == 256) {
                TRY(bit_stream.write_bits(0u, 1u)); // max_symbol is alphabet_size
            } else {
                TRY(bit_stream.write_bits(1u, 1u)); // max_symbol is explicitly coded
                // "int length_nbits = 2 + 2 * ReadBits(3);"
                TRY(bit_stream.write_bits(3u, 3u));   // length_nbits = 2 + 2 * 3
                // "int max_symbol = 2 + ReadBits(length_nbits);"
                TRY(bit_stream.write_bits(254u, 8u)); // max_symbol = 2 + 254
            }
        }

        // Don't use #5 to write the color index.
        TRY(bit_stream.write_bits(1u, 1u)); // Simple code length code.
        TRY(bit_stream.write_bits(0u, 1u)); // num_symbols - 1
        TRY(bit_stream.write_bits(0u, 1u)); // is_first_8bits
        TRY(bit_stream.write_bits(0u, 1u)); // symbol0

        u8 last_r = 0, last_g = 0, last_b = 0, last_a = 0;
        for (u32 i = 0; i < color_table_size; ++i) {
            u8 r = i;
            u8 g = 255 - i;
            u8 b = 128;
            u8 a = 255;

            TRY(bit_stream.write_bits(Compress::reverse8_lookup_table[(u8)(g - last_g)], 8u));
            TRY(bit_stream.write_bits(Compress::reverse8_lookup_table[(u8)(r - last_r)], 8u));
            TRY(bit_stream.write_bits(Compress::reverse8_lookup_table[(u8)(b - last_b)], 8u));
            TRY(bit_stream.write_bits(Compress::reverse8_lookup_table[(u8)(a - last_a)], 8u));

            last_r = r;
            last_g = g;
            last_b = b;
            last_a = a;
        }
    }

    bool write_predictor_transform = options.predictor != -1;
    if (write_predictor_transform) {
        TRY(bit_stream.write_bits(1u, 1u)); // Transform present.
        TRY(bit_stream.write_bits(0u, 2u)); // PREDICTOR_TRANSFORM

        // int size_bits = ReadBits(3) + 2;
        TRY(bit_stream.write_bits(7u, 3u)); // 7 + 2; value doesn't matter.

        TRY(bit_stream.write_bits(0u, 1u)); // No color cache.

        // Meta prefix presence is only stored for spatially-coded images (i.e. the main image), not for entropy images (e.g. the predictor image).

        // G stores the predictor image.
        TRY(bit_stream.write_bits(1u, 1u)); // Simple code length code.
        TRY(bit_stream.write_bits(0u, 1u)); // num_symbols - 1
        TRY(bit_stream.write_bits(1u, 1u)); // is_first_8bits
        TRY(bit_stream.write_bits((unsigned)options.predictor, 8u)); // symbol0

        for (int i = 0; i < 4; ++i) {
            // The other 4 channels are unused.
            TRY(bit_stream.write_bits(1u, 1u)); // Simple code length code.
            TRY(bit_stream.write_bits(0u, 1u)); // num_symbols - 1
            TRY(bit_stream.write_bits(0u, 1u)); // is_first_8bits
            TRY(bit_stream.write_bits(0u, 1u)); // symbol0
        }

        // No actual data to store since it's 0 bits per entry again.
    }

    TRY(bit_stream.write_bits(0u, 1u)); // No further transforms.

    // https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#5_image_data
    // spatially-coded-image =  color-cache-info meta-prefix data

    // color-cache-info      =  %b0
    // color-cache-info      =/ (%b1 4BIT) ; 1 followed by color cache size
    TRY(bit_stream.write_bits(0u, 1u)); // No color cache for now.
    // XXX try using a color cache and making the constant value a color cache entry?

    // meta-prefix           =  %b0 / (%b1 entropy-image)
    TRY(bit_stream.write_bits(0u, 1u)); // No meta prefix for now.

    // G
    TRY(bit_stream.write_bits(1u, 1u)); // Simple code length code.
    TRY(bit_stream.write_bits(0u, 1u)); // num_symbols - 1
    TRY(bit_stream.write_bits(1u, 1u)); // is_first_8bits
    TRY(bit_stream.write_bits(options.color.green(), 8u)); // symbol0

    // R
    TRY(bit_stream.write_bits(1u, 1u)); // Simple code length code.
    TRY(bit_stream.write_bits(0u, 1u)); // num_symbols - 1
    TRY(bit_stream.write_bits(1u, 1u)); // is_first_8bits
    TRY(bit_stream.write_bits(options.color.red(), 8u)); // symbol0

    // B
    TRY(bit_stream.write_bits(1u, 1u)); // Simple code length code.
    TRY(bit_stream.write_bits(0u, 1u)); // num_symbols - 1
    TRY(bit_stream.write_bits(1u, 1u)); // is_first_8bits
    TRY(bit_stream.write_bits(options.color.blue(), 8u)); // symbol0

    // A
    TRY(bit_stream.write_bits(1u, 1u)); // Simple code length code.
    TRY(bit_stream.write_bits(0u, 1u)); // num_symbols - 1
    TRY(bit_stream.write_bits(1u, 1u)); // is_first_8bits
    TRY(bit_stream.write_bits(options.color.alpha(), 8u)); // symbol0

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
    TRY(write_VP8L_image_data(vp8l_data_stream, options));
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

#if 0
// Let's use Color::from_string() instead, thanks Mr Flynn!
static ErrorOr<Gfx::Color> parse_hex_color(StringView color_string)
{
    if (color_string.length() != 7 && color_string.length() != 9)
        return Error::from_string_view("Color must be in the format #RRGGBB or #RRGGBBAA"sv);

    if (color_string[0] != '#')
        return Error::from_string_view("Color must start with #"sv);

    auto bytes = TRY(decode_hex(color_string.substring_view(1)));
    VERIFY(bytes.size() == 3 || bytes.size() == 4);

    return Gfx::Color(bytes[0], bytes[1], bytes[2], bytes.size() == 4 ? bytes[3] : 255);
}
#endif

static ErrorOr<Options> parse_options(Main::Arguments arguments)
{
    Options options;
    Core::ArgsParser args_parser;
    args_parser.add_option(options.out_path, "Path to output image file", "output", 'o', "FILE");
    args_parser.add_option(options.width, "Width of the image (default: 512)", "width", {}, "WIDTH");
    args_parser.add_option(options.height, "Height of the image (default: 512)", "height", {}, "HEIGHT");
    args_parser.add_option(options.predictor, "Predictor to use (valid values: 0-13, -1 for None; default: -1)", "predictor", {}, "PREDICTOR");
    StringView color_string = "#FF0000"sv;
    args_parser.add_option(color_string, "Color to use (format: #RRGGBB or #RRGGBBAA; default: #FF0000)", "color", {}, "COLOR");
    args_parser.parse(arguments);

    auto color = Color::from_string(color_string);
    if (!color.has_value())
        return Error::from_string_view("Invalid color"sv);
    options.color = color.value();

    if (options.out_path.is_empty())
        return Error::from_string_view("-o is required"sv);

    if (options.predictor < -1 || options.predictor > 13)
        return Error::from_string_view("Predictor must be -1 or between 0 and 13"sv);

    return options;
}

ErrorOr<int> serenity_main(Main::Arguments arguments)
{
    Options options = TRY(parse_options(arguments));

    auto output_stream = TRY(Core::File::open(options.out_path, Core::File::OpenMode::Write));

    TRY(write_webp(*TRY(Core::OutputBufferedFile::create(move(output_stream))), options));

    return 0;
}
