/*
 * Copyright (c) 2023, Nico Weber <thakis@chromium.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/BitStream.h>
#include <AK/Debug.h>
#include <AK/Endian.h>
#include <AK/Format.h>
#include <AK/MemoryStream.h>
#include <AK/String.h>
#include <AK/Vector.h>
#include <LibCompress/Deflate.h>
#include <LibCore/File.h>
#include <LibGfx/ImageFormats/PNGWriter.h>
#include <LibGfx/ImageFormats/WebPLoader.h>

// Overview: https://developers.google.com/speed/webp/docs/compression
// Container: https://developers.google.com/speed/webp/docs/riff_container
// Lossless format: https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification
// Lossy format: https://datatracker.ietf.org/doc/html/rfc6386

namespace Gfx {

namespace {

ErrorOr<void> save_one_bitmap(const Bitmap& bitmap, StringView path)
{
    auto bytes = TRY(Gfx::PNGWriter::encode(bitmap));
    auto output_stream = TRY(Core::File::open(path, Core::File::OpenMode::Write));
    TRY(output_stream->write_until_depleted(bytes));
    return {};
}

ErrorOr<void> save_bitmap(const Bitmap& bitmap, StringView path)
{
    TRY(save_one_bitmap(bitmap, path));

    {
        auto clone = TRY(bitmap.clone());
        for (ARGB32& pixel : *clone)
            pixel |= 0xff'00'00'00;
        TRY(save_one_bitmap(clone, TRY(String::formatted("{}-rgb.png", path))));
    }

    {
        auto clone = TRY(bitmap.clone());
        for (ARGB32& pixel : *clone) {
            u8 a = pixel >> 24;
            pixel = 0xff'00'00'00 | (a << 16) | (a << 8) | a;
        }
        TRY(save_one_bitmap(clone, TRY(String::formatted("{}-a.png", path))));
    }

    return {};
}

struct FourCC {
    constexpr FourCC(char const* name)
    {
        cc[0] = name[0];
        cc[1] = name[1];
        cc[2] = name[2];
        cc[3] = name[3];
    }

    bool operator==(FourCC const&) const = default;
    bool operator!=(FourCC const&) const = default;

    char cc[4];
};

// https://developers.google.com/speed/webp/docs/riff_container#webp_file_header
struct WebPFileHeader {
    FourCC riff;
    LittleEndian<u32> file_size;
    FourCC webp;
};
static_assert(AssertSize<WebPFileHeader, 12>());

struct ChunkHeader {
    FourCC chunk_type;
    LittleEndian<u32> chunk_size;
};
static_assert(AssertSize<ChunkHeader, 8>());

struct Chunk {
    FourCC type;
    ReadonlyBytes data;
};

struct VP8Header {
    u8 version;
    bool show_frame;
    u32 size_of_first_partition;
    u32 width;
    u8 horizontal_scale;
    u32 height;
    u8 vertical_scale;
};

struct VP8LHeader {
    u16 width;
    u16 height;
    bool is_alpha_used;
};

struct VP8XHeader {
    bool has_icc;
    bool has_alpha;
    bool has_exif;
    bool has_xmp;
    bool has_animation;
    u32 width;
    u32 height;
};

struct ANIMChunk {
    u32 background_color;
    u16 loop_count;
};

}

struct WebPLoadingContext {
    enum State {
        NotDecoded = 0,
        Error,
        HeaderDecoded,
        FirstChunkRead,
        FirstChunkDecoded,
        ChunksDecoded,
        BitmapDecoded,
    };
    State state { State::NotDecoded };
    ReadonlyBytes data;

    ReadonlyBytes chunks_cursor;

    Optional<IntSize> size;

    RefPtr<Gfx::Bitmap> bitmap;

    // Either 'VP8 ' (simple lossy file), 'VP8L' (simple lossless file), or 'VP8X' (extended file).
    Optional<Chunk> first_chunk;
    union {
        VP8Header vp8_header;
        VP8LHeader vp8l_header;
        VP8XHeader vp8x_header;
    };

    // If first_chunk is not a VP8X chunk, then only image_data_chunk is set and all the other Chunks are not set.

    // "For a still image, the image data consists of a single frame, which is made up of:
    //     An optional alpha subchunk.
    //     A bitstream subchunk."
    Optional<Chunk> alpha_chunk;      // 'ALPH'
    Optional<Chunk> image_data_chunk; // Either 'VP8 ' or 'VP8L'.

    Optional<Chunk> animation_header_chunk; // 'ANIM'
    Vector<Chunk> animation_frame_chunks;   // 'ANMF'

    Optional<Chunk> iccp_chunk; // 'ICCP'
    Optional<Chunk> exif_chunk; // 'EXIF'
    Optional<Chunk> xmp_chunk;  // 'XMP '

    template<size_t N>
    [[nodiscard]] class Error error(char const (&string_literal)[N])
    {
        state = WebPLoadingContext::State::Error;
        return Error::from_string_literal(string_literal);
    }
};

// https://developers.google.com/speed/webp/docs/riff_container#webp_file_header
static ErrorOr<void> decode_webp_header(WebPLoadingContext& context)
{
    if (context.state >= WebPLoadingContext::HeaderDecoded)
        return {};

    if (context.data.size() < sizeof(WebPFileHeader))
        return context.error("Missing WebP header");

    auto& header = *bit_cast<WebPFileHeader const*>(context.data.data());
    if (header.riff != FourCC("RIFF") || header.webp != FourCC("WEBP"))
        return context.error("Invalid WebP header");

    // "File Size: [...] The size of the file in bytes starting at offset 8. The maximum value of this field is 2^32 minus 10 bytes."
    u32 const maximum_webp_file_size = 0xffff'ffff - 9;
    if (header.file_size > maximum_webp_file_size)
        return context.error("WebP header file size over maximum");

    // "The file size in the header is the total size of the chunks that follow plus 4 bytes for the 'WEBP' FourCC.
    //  The file SHOULD NOT contain any data after the data specified by File Size.
    //  Readers MAY parse such files, ignoring the trailing data."
    if (context.data.size() - 8 < header.file_size)
        return context.error("WebP data too small for size in header");
    if (context.data.size() - 8 > header.file_size) {
        dbgln_if(WEBP_DEBUG, "WebP has {} bytes of data, but header needs only {}. Trimming.", context.data.size(), header.file_size + 8);
        context.data = context.data.trim(header.file_size + 8);
    }

    context.state = WebPLoadingContext::HeaderDecoded;
    return {};
}

// https://developers.google.com/speed/webp/docs/riff_container#riff_file_format
static ErrorOr<Chunk> decode_webp_chunk_header(WebPLoadingContext& context, ReadonlyBytes chunks)
{
    if (chunks.size() < sizeof(ChunkHeader))
        return context.error("Not enough data for WebP chunk header");

    auto const& header = *bit_cast<ChunkHeader const*>(chunks.data());
    dbgln_if(WEBP_DEBUG, "chunk {} size {}", header.chunk_type, header.chunk_size);

    if (chunks.size() < sizeof(ChunkHeader) + header.chunk_size)
        return context.error("Not enough data for WebP chunk");

    return Chunk { header.chunk_type, { chunks.data() + sizeof(ChunkHeader), header.chunk_size } };
}

// https://developers.google.com/speed/webp/docs/riff_container#riff_file_format
static ErrorOr<Chunk> decode_webp_advance_chunk(WebPLoadingContext& context, ReadonlyBytes& chunks)
{
    auto chunk = TRY(decode_webp_chunk_header(context, chunks));

    // "Chunk Size: 32 bits (uint32)
    //      The size of the chunk in bytes, not including this field, the chunk identifier or padding.
    //  Chunk Payload: Chunk Size bytes
    //      The data payload. If Chunk Size is odd, a single padding byte -- that MUST be 0 to conform with RIFF -- is added."
    chunks = chunks.slice(sizeof(ChunkHeader) + chunk.data.size());

    if (chunk.data.size() % 2 != 0) {
        if (chunks.is_empty())
            return context.error("Missing data for padding byte");
        if (*chunks.data() != 0)
            return context.error("Padding byte is not 0");
        chunks = chunks.slice(1);
    }

    return chunk;
}

// https://developers.google.com/speed/webp/docs/riff_container#simple_file_format_lossy
// https://datatracker.ietf.org/doc/html/rfc6386#section-19 "Annex A: Bitstream Syntax"
static ErrorOr<VP8Header> decode_webp_chunk_VP8_header(WebPLoadingContext& context, Chunk const& vp8_chunk)
{
    VERIFY(vp8_chunk.type == FourCC("VP8 "));

    if (vp8_chunk.data.size() < 10)
        return context.error("WebPImageDecoderPlugin: 'VP8 ' chunk too small");

    // FIXME: Eventually, this should probably call into LibVideo/VP8,
    // and image decoders should move into LibImageDecoders which depends on both LibGfx and LibVideo.
    // (LibVideo depends on LibGfx, so LibGfx can't depend on LibVideo itself.)

    // https://datatracker.ietf.org/doc/html/rfc6386#section-4 "Overview of Compressed Data Format"
    // "The decoder is simply presented with a sequence of compressed frames [...]
    //  The first frame presented to the decompressor is [...] a key frame.  [...]
    //  [E]very compressed frame has three or more pieces. It begins with an uncompressed data chunk comprising 10 bytes in the case of key frames

    u8 const* data = vp8_chunk.data.data();

    // https://datatracker.ietf.org/doc/html/rfc6386#section-9.1 "Uncompressed Data Chunk"
    u32 frame_tag = data[0] | (data[1] << 8) | (data[2] << 16);
    bool is_key_frame = (frame_tag & 1) == 0; // https://www.rfc-editor.org/errata/eid5534
    u8 version = (frame_tag & 0xe) >> 1;
    bool show_frame = (frame_tag & 0x10) != 0;
    u32 size_of_first_partition = frame_tag >> 5;

    if (!is_key_frame)
        return context.error("WebPImageDecoderPlugin: 'VP8 ' chunk not a key frame");

    // FIXME: !show_frame does not make sense in a webp file either, probably?

    u32 start_code = data[3] | (data[4] << 8) | (data[5] << 16);
    if (start_code != 0x2a019d) // https://www.rfc-editor.org/errata/eid7370
        return context.error("WebPImageDecoderPlugin: 'VP8 ' chunk invalid start_code");

    // "The scaling specifications for each dimension are encoded as follows.
    //   0     | No upscaling (the most common case).
    //   1     | Upscale by 5/4.
    //   2     | Upscale by 5/3.
    //   3     | Upscale by 2."
    // This is a display-time operation and doesn't affect decoding.
    u16 width_and_horizontal_scale = data[6] | (data[7] << 8);
    u16 width = width_and_horizontal_scale & 0x3fff;
    u8 horizontal_scale = width_and_horizontal_scale >> 14;

    u16 heigth_and_vertical_scale = data[8] | (data[9] << 8);
    u16 height = heigth_and_vertical_scale & 0x3fff;
    u8 vertical_scale = heigth_and_vertical_scale >> 14;

    dbgln_if(WEBP_DEBUG, "version {}, show_frame {}, size_of_first_partition {}, width {}, horizontal_scale {}, height {}, vertical_scale {}",
        version, show_frame, size_of_first_partition, width, horizontal_scale, height, vertical_scale);

    return VP8Header { version, show_frame, size_of_first_partition, width, horizontal_scale, height, vertical_scale };
}

// https://developers.google.com/speed/webp/docs/riff_container#simple_file_format_lossless
// https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#7_overall_structure_of_the_format
static ErrorOr<VP8LHeader> decode_webp_chunk_VP8L_header(WebPLoadingContext& context, Chunk const& vp8l_chunk)
{
    VERIFY(vp8l_chunk.type == FourCC("VP8L"));

    // https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#3_riff_header
    if (vp8l_chunk.data.size() < 5)
        return context.error("WebPImageDecoderPlugin: VP8L chunk too small");

    FixedMemoryStream memory_stream { vp8l_chunk.data.trim(5) };
    LittleEndianInputBitStream bit_stream { MaybeOwned<Stream>(memory_stream) };

    u8 signature = TRY(bit_stream.read_bits(8));
    if (signature != 0x2f)
        return context.error("WebPImageDecoderPlugin: VP8L chunk invalid signature");

    // 14 bits width-1, 14 bits height-1, 1 bit alpha hint, 3 bit version_number.
    u16 width = TRY(bit_stream.read_bits(14)) + 1;
    u16 height = TRY(bit_stream.read_bits(14)) + 1;
    bool is_alpha_used = TRY(bit_stream.read_bits(1)) != 0;
    u8 version_number = TRY(bit_stream.read_bits(3));
    VERIFY(bit_stream.is_eof());

    dbgln_if(WEBP_DEBUG, "width {}, height {}, is_alpha_used {}, version_number {}",
        width, height, is_alpha_used, version_number);

    // "The version_number is a 3 bit code that must be set to 0. Any other value should be treated as an error. [AMENDED]"
    if (version_number != 0)
        return context.error("WebPImageDecoderPlugin: VP8L chunk invalid version_number");

    return VP8LHeader { width, height, is_alpha_used };
}

// https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#61_overview
// "From here on, we refer to this set as a prefix code group."
class PrefixCodeGroup {
public:
    Compress::CanonicalCode& operator[](int i) { return m_codes[i]; }
    Compress::CanonicalCode const& operator[](int i) const { return m_codes[i]; }

private:
    Array<Compress::CanonicalCode, 5> m_codes;
};

// https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#621_decoding_and_building_the_prefix_codes
static ErrorOr<Compress::CanonicalCode> decode_webp_chunk_VP8L_prefix_code(WebPLoadingContext& context, LittleEndianInputBitStream& bit_stream, size_t alphabet_size)
{
    // prefix-code           =  simple-prefix-code / normal-prefix-code
    bool is_simple_code_length_code = TRY(bit_stream.read_bits(1));
    dbgln_if(WEBP_DEBUG, "is_simple_code_length_code {}", is_simple_code_length_code);

    Vector<u8, 286> code_lengths;

    if (is_simple_code_length_code) {
        TRY(code_lengths.try_resize(alphabet_size));

        int num_symbols = TRY(bit_stream.read_bits(1)) + 1;
        int is_first_8bits = TRY(bit_stream.read_bits(1));
        u8 symbol0 = TRY(bit_stream.read_bits(1 + 7 * is_first_8bits));
        dbgln_if(WEBP_DEBUG, "  symbol0 {}", symbol0);

        if (symbol0 >= code_lengths.size())
            return Error::from_string_literal("symbol0 out of bounds");
        code_lengths[symbol0] = 1;
        if (num_symbols == 2) {
            u8 symbol1 = TRY(bit_stream.read_bits(8));
            dbgln_if(WEBP_DEBUG, "  symbol1 {}", symbol1);

            if (symbol1 >= code_lengths.size())
                return Error::from_string_literal("symbol1 out of bounds");
            code_lengths[symbol1] = 1;
        }

        return Compress::CanonicalCode::from_bytes(code_lengths);
    }

    // This has plenty in common with deflate (cf DeflateDecompressor::decode_codes() in Deflate.cpp in LibCompress)
    // Symbol 16 has different semantics, and kCodeLengthCodeOrder is different. Other than that, this is virtually deflate.
    // (...but webp uses 5 different prefix codes, while deflate doesn't.)
    int num_code_lengths = 4 + TRY(bit_stream.read_bits(4));
    dbgln_if(WEBP_DEBUG, "  num_code_lengths {}", num_code_lengths);

    // "If num_code_lengths is > 19, the bit_stream is invalid. [AMENDED3]"
    if (num_code_lengths > 19)
        return context.error("WebPImageDecoderPlugin: invalid num_code_lengths");

    constexpr int kCodeLengthCodes = 19;
    int kCodeLengthCodeOrder[kCodeLengthCodes] = { 17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
    u8 code_length_code_lengths[kCodeLengthCodes] = { 0 }; // "All zeros" [sic]
    for (int i = 0; i < num_code_lengths; ++i) {
        code_length_code_lengths[kCodeLengthCodeOrder[i]] = TRY(bit_stream.read_bits(3));
        dbgln_if(WEBP_DEBUG, "  code_length_code_lengths[{}] = {}", kCodeLengthCodeOrder[i], code_length_code_lengths[kCodeLengthCodeOrder[i]]);
    }

    // The spec is at best misleading here, suggesting that max_symbol should be set to "num_code_lengths" if it's not explicitly stored.
    // But num_code_lengths doesn't mean the num_code_lengths mentioned a few lines further up in the spec (and in scope here),
    // but alphabet_size!
    //
    // Since the spec doesn't mention it, see libwebp vp8l_dec.c, ReadHuffmanCode()
    // which passes alphabet_size to ReadHuffmanCodeLengths() as num_symbols,
    // and ReadHuffmanCodeLengths() then sets max_symbol to that.)
    unsigned max_symbol = alphabet_size;
    if (TRY(bit_stream.read_bits(1))) {
        int length_nbits = 2 + 2 * TRY(bit_stream.read_bits(3));
        max_symbol = 2 + TRY(bit_stream.read_bits(length_nbits));
        dbgln_if(WEBP_DEBUG, "  extended, length_nbits {} max_symbol {}", length_nbits, max_symbol);
        if (max_symbol > alphabet_size)
            return context.error("WebPImageDecoderPlugin: invalid max_symbol");
    }

    auto const code_length_code = TRY(Compress::CanonicalCode::from_bytes({ code_length_code_lengths, sizeof(code_length_code_lengths) }));

    // Next we extract the code lengths of the code that was used to encode the block.

    u8 last_non_zero = 8; // "If code 16 is used before a non-zero value has been emitted, a value of 8 is repeated."

    // "A prefix table is then built from code_length_code_lengths and used to read up to max_symbol code lengths."
    dbgln_if(WEBP_DEBUG, "  reading {} symbols", max_symbol);
    while (code_lengths.size() < max_symbol) {
        auto symbol = TRY(code_length_code.read_symbol(bit_stream));

        if (symbol < 16) {
            // "Code [0..15] indicates literal code lengths."
            dbgln_if(WEBP_DEBUG, "  append {}", symbol);
            code_lengths.append(static_cast<u8>(symbol));
            if (symbol != 0)
                last_non_zero = symbol;
        } else if (symbol == 16) {
            // "Code 16 repeats the previous non-zero value [3..6] times, i.e., 3 + ReadBits(2) times."
            auto nrepeat = 3 + TRY(bit_stream.read_bits(2));
            dbgln_if(WEBP_DEBUG, "  repeat {} {}s", nrepeat, last_non_zero);

            // This is different from deflate.
            for (size_t j = 0; j < nrepeat; ++j)
                code_lengths.append(last_non_zero);
        } else if (symbol == 17) {
            // "Code 17 emits a streak of zeros [3..10], i.e., 3 + ReadBits(3) times."
            auto nrepeat = 3 + TRY(bit_stream.read_bits(3));
            dbgln_if(WEBP_DEBUG, "  repeat {} zeroes", nrepeat);
            for (size_t j = 0; j < nrepeat; ++j)
                code_lengths.append(0);
        } else {
            VERIFY(symbol == 18);
            // "Code 18 emits a streak of zeros of length [11..138], i.e., 11 + ReadBits(7) times."
            auto nrepeat = 11 + TRY(bit_stream.read_bits(7));
            dbgln_if(WEBP_DEBUG, "  Repeat {} zeroes", nrepeat);
            for (size_t j = 0; j < nrepeat; ++j)
                code_lengths.append(0);
        }
    }

    if (code_lengths.size() != alphabet_size)
        return Error::from_string_literal("Number of code lengths does not match the sum of codes");

    dbgln_if(WEBP_DEBUG, "  done reading symbols");
    return Compress::CanonicalCode::from_bytes(code_lengths);
}

// https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#622_decoding_of_meta_prefix_codes
// The description of prefix code groups is in "Decoding of Meta Prefix Codes", even though prefix code groups are used
// in regular images without meta prefix code as well ¯\_(ツ)_/¯.
static ErrorOr<PrefixCodeGroup> decode_webp_chunk_VP8L_prefix_code_group(WebPLoadingContext& context, u16 color_cache_size, LittleEndianInputBitStream& bit_stream)
{
    // prefix-code-group     =
    //     5prefix-code ; See "Interpretation of Meta Prefix Codes" to
    //                  ; understand what each of these five prefix
    //                  ; codes are for.

    // "Once code lengths are read, a prefix code for each symbol type (A, R, G, B, distance) is formed using their respective alphabet sizes:
    //  * G channel: 256 + 24 + color_cache_size
    //  * other literals (A,R,B): 256
    //  * distance code: 40"
    Array<size_t, 5> const alphabet_sizes { 256 + 24 + static_cast<size_t>(color_cache_size), 256, 256, 256, 40 };

    PrefixCodeGroup group;
    for (size_t i = 0; i < alphabet_sizes.size(); ++i)
        group[i] = TRY(decode_webp_chunk_VP8L_prefix_code(context, bit_stream, alphabet_sizes[i]));
    return group;
}

enum class ImageKind {
    SpatiallyCoded,
    EntropyCoded,
};

static ErrorOr<NonnullRefPtr<Bitmap>> decode_webp_chunk_VP8L_image(WebPLoadingContext& context, ImageKind image_kind, BitmapFormat format, IntSize const& size, LittleEndianInputBitStream& bit_stream)
{
    // https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#623_decoding_entropy-coded_image_data
    // https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#523_color_cache_coding
    // spatially-coded-image =  color-cache-info meta-prefix data
    // entropy-coded-image   =  color-cache-info data

    // color-cache-info      =  %b0
    // color-cache-info      =/ (%b1 4BIT) ; 1 followed by color cache size
    bool has_color_cache_info = TRY(bit_stream.read_bits(1));
    u16 color_cache_size = 0;
    u8 color_cache_code_bits;
    dbgln_if(WEBP_DEBUG, "has_color_cache_info {}", has_color_cache_info);
    Vector<ARGB32, 32> color_cache;
    if (has_color_cache_info) {
        color_cache_code_bits = TRY(bit_stream.read_bits(4));

        // "The range of allowed values for color_cache_code_bits is [1..11]. Compliant decoders must indicate a corrupted bitstream for other values."
        if (color_cache_code_bits < 1 || color_cache_code_bits > 11)
            return context.error("WebPImageDecoderPlugin: VP8L invalid color_cache_code_bits");

        color_cache_size = 1 << color_cache_code_bits;
        dbgln_if(WEBP_DEBUG, "color_cache_size {}", color_cache_size);

        TRY(color_cache.try_resize(color_cache_size));
    }

    int num_prefix_groups = 1;
    RefPtr<Gfx::Bitmap> entropy_image;
    int prefix_bits = 0;
    if (image_kind == ImageKind::SpatiallyCoded) {
        // https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#622_decoding_of_meta_prefix_codes
        // In particular, the "Entropy image" subsection.
        // "Meta prefix codes may be used only when the image is being used in the role of an ARGB image."
        // meta-prefix           =  %b0 / (%b1 entropy-image)
        bool has_meta_prefix = TRY(bit_stream.read_bits(1));
        dbgln_if(WEBP_DEBUG, "has_meta_prefix {}", has_meta_prefix);
        if (has_meta_prefix) {
            prefix_bits = TRY(bit_stream.read_bits(3)) + 2;
            dbgln_if(WEBP_DEBUG, "prefix_bits {}", prefix_bits);
            int block_size = 1 << prefix_bits;
            IntSize prefix_size { ceil_div(size.width(), block_size), ceil_div(size.height(), block_size) };

            entropy_image = TRY(decode_webp_chunk_VP8L_image(context, ImageKind::EntropyCoded, BitmapFormat::BGRx8888, prefix_size, bit_stream));
            MUST(save_bitmap(*entropy_image, "webp-entropy.png"sv));

            // "The red and green components of a pixel define the meta prefix code used in a particular block of the ARGB image."
            // ...
            // "The number of prefix code groups in the ARGB image can be obtained by finding the largest meta prefix code from the entropy image"
            u16 largest_meta_prefix_code = 0;
            for (ARGB32& pixel : *entropy_image) {
                // FIXME: Should this check if alpha and blue are 0?
                u16 meta_prefix_code = (pixel >> 8) & 0xffff;
                if (meta_prefix_code > largest_meta_prefix_code)
                    largest_meta_prefix_code = meta_prefix_code;
            }
            dbgln_if(WEBP_DEBUG, "largest meta prefix code {}", largest_meta_prefix_code);

            num_prefix_groups = largest_meta_prefix_code + 1;
        }
    }

    // https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#52_encoding_of_image_data
    // "The encoded image data consists of several parts:
    //    1. Decoding and building the prefix codes [AMENDED2]
    //    2. Meta prefix codes
    //    3. Entropy-coded image data"
    // data                  =  prefix-codes lz77-coded-image
    // prefix-codes          =  prefix-code-group *prefix-codes

    Vector<PrefixCodeGroup, 1> groups;
    for (int i = 0; i < num_prefix_groups; ++i)
        TRY(groups.try_append(TRY(decode_webp_chunk_VP8L_prefix_code_group(context, color_cache_size, bit_stream))));

    auto bitmap = TRY(Bitmap::create(format, size));

    // https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#522_lz77_backward_reference
    struct Offset {
        i8 x, y;
    };
    // clang-format off
    Array<Offset, 120> distance_map { {
        {0, 1}, {1, 0},
        {1, 1}, {-1, 1}, {0, 2}, { 2, 0},
        {1, 2}, {-1, 2}, {2, 1}, {-2, 1},
        {2, 2}, {-2, 2}, {0, 3}, { 3, 0}, { 1, 3}, {-1, 3}, { 3, 1}, {-3, 1},
        {2, 3}, {-2, 3}, {3, 2}, {-3, 2}, { 0, 4}, { 4, 0}, { 1, 4}, {-1, 4}, { 4, 1}, {-4, 1},
        {3, 3}, {-3, 3}, {2, 4}, {-2, 4}, { 4, 2}, {-4, 2}, { 0, 5},
        {3, 4}, {-3, 4}, {4, 3}, {-4, 3}, { 5, 0}, { 1, 5}, {-1, 5}, { 5, 1}, {-5, 1}, { 2, 5}, {-2, 5}, { 5, 2}, {-5, 2},
        {4, 4}, {-4, 4}, {3, 5}, {-3, 5}, { 5, 3}, {-5, 3}, { 0, 6}, { 6, 0}, { 1, 6}, {-1, 6}, { 6, 1}, {-6, 1}, { 2, 6}, {-2, 6}, {6, 2}, {-6, 2},
        {4, 5}, {-4, 5}, {5, 4}, {-5, 4}, { 3, 6}, {-3, 6}, { 6, 3}, {-6, 3}, { 0, 7}, { 7, 0}, { 1, 7}, {-1, 7},
        {5, 5}, {-5, 5}, {7, 1}, {-7, 1}, { 4, 6}, {-4, 6}, { 6, 4}, {-6, 4}, { 2, 7}, {-2, 7}, { 7, 2}, {-7, 2}, { 3, 7}, {-3, 7}, {7, 3}, {-7, 3},
        {5, 6}, {-5, 6}, {6, 5}, {-6, 5}, { 8, 0}, { 4, 7}, {-4, 7}, { 7, 4}, {-7, 4}, { 8, 1}, { 8, 2},
        {6, 6}, {-6, 6}, {8, 3}, { 5, 7}, {-5, 7}, { 7, 5}, {-7, 5}, { 8, 4},
        {6, 7}, {-6, 7}, {7, 6}, {-7, 6}, { 8, 5},
        {7, 7}, {-7, 7}, {8, 6},
        {8, 7},
    } };
    // clang-format on

    // lz77-coded-image      =
    //     *((argb-pixel / lz77-copy / color-cache-code) lz77-coded-image)
    // https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#623_decoding_entropy-coded_image_data
    ARGB32* begin = bitmap->begin();
    ARGB32* end = bitmap->end();
    ARGB32* pixel = begin;

    auto prefix_group = [prefix_bits, begin, &groups, size, &entropy_image](ARGB32* pixel) {
        if (!prefix_bits)
            return groups[0];

        size_t offset = pixel - begin;
        int x = offset % size.width();
        int y = offset / size.width();

        int meta_prefix_code = (entropy_image->scanline(y >> prefix_bits)[x >> prefix_bits] >> 8) & 0xffff;
        return groups[meta_prefix_code];
    };

    auto emit_pixel = [&pixel, &color_cache, color_cache_size, color_cache_code_bits](ARGB32 color) {
        // https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#523_color_cache_coding
        // "The state of the color cache is maintained by inserting every pixel, be it produced by backward referencing or as literals, into the cache in the order they appear in the stream."
        *pixel++ = color;
        if (color_cache_size)
            color_cache[(0x1e35a7bd * color) >> (32 - color_cache_code_bits)] = color;
    };

    while (pixel < end) {
        auto const& group = prefix_group(pixel);

        auto symbol = TRY(group[0].read_symbol(bit_stream));
        if (symbol >= 256u + 24u + color_cache_size)
            return context.error("WebPImageDecoderPlugin: Symbol out of bounds");

        // "1. if S < 256"
        if (symbol < 256u) {
            // "a. Use S as the green component."
            u8 g = symbol;

            // "b. Read red from the bitstream using prefix code #2."
            u8 r = TRY(group[1].read_symbol(bit_stream));

            // "c. Read blue from the bitstream using prefix code #3."
            u8 b = TRY(group[2].read_symbol(bit_stream));

            // "d. Read alpha from the bitstream using prefix code #4."
            u8 a = TRY(group[3].read_symbol(bit_stream));

            emit_pixel(Color(r, g, b, a).value());
        }
        // "2. if S >= 256 && S < 256 + 24"
        else if (symbol < 256u + 24u) {
            auto prefix_value = [&bit_stream](u8 prefix_code) -> ErrorOr<u32> {
                // https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#522_lz77_backward_reference
                if (prefix_code < 4)
                    return prefix_code + 1;
                int extra_bits = (prefix_code - 2) >> 1;
                int offset = (2 + (prefix_code & 1)) << extra_bits;
                return offset + TRY(bit_stream.read_bits(extra_bits)) + 1;
            };

            // "a. Use S - 256 as a length prefix code."
            u8 length_prefix_code = symbol - 256;

            // "b. Read extra bits for length from the bitstream."
            // "c. Determine backward-reference length L from length prefix code and the extra bits read."
            u32 length = TRY(prefix_value(length_prefix_code));

            // "d. Read distance prefix code from the bitstream using prefix code #5."
            u8 distance_prefix_code = TRY(group[4].read_symbol(bit_stream));

            // "e. Read extra bits for distance from the bitstream."
            // "f. Determine backward-reference distance D from distance prefix code and the extra bits read."
            i32 distance = TRY(prefix_value(distance_prefix_code));

            // "g. Copy the L pixels (in scan-line order) from the sequence of pixels prior to them by D pixels."

            // https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#522_lz77_backward_reference
            // "Distance codes larger than 120 denote the pixel-distance in scan-line order, offset by 120."
            // "The smallest distance codes [1..120] are special, and are reserved for a close neighborhood of the current pixel."
            if (distance <= 120) {
                auto offset = distance_map[distance - 1];
                distance = offset.x + offset.y * bitmap->physical_width();
                if (distance < 1)
                    distance = 1;
            } else {
                distance = distance - 120;
            }

            if (pixel - begin < distance) {
                dbgln_if(WEBP_DEBUG, "invalid backref, {} < {}", pixel - begin, distance);
                return context.error("WebPImageDecoderPlugin: Backward reference distance out of bounds");
            }

            if (end - pixel < length) {
                dbgln_if(WEBP_DEBUG, "invalid length, {} < {}", end - pixel, length);
                return context.error("WebPImageDecoderPlugin: Backward reference length out of bounds");
            }

            ARGB32* src = pixel - distance;
            for (u32 i = 0; i < length; ++i)
                emit_pixel(src[i]);
        }
        // "3. if S >= 256 + 24"
        else {
            // "a. Use S - (256 + 24) as the index into the color cache."
            unsigned index = symbol - (256 + 24);

            // "b. Get ARGB color from the color cache at that index."
            if (index >= color_cache_size)
                return context.error("WebPImageDecoderPlugin: Color cache index out of bounds");
            *pixel++ = color_cache[index];
        }
    }

    return bitmap;
}

namespace {
class Transform {
public:
    virtual ~Transform();
    virtual ErrorOr<void> transform(Bitmap&) = 0;
};

Transform::~Transform() = default;

// https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#41_predictor_transform
class PredictorTransform : public Transform {
public:
    static ErrorOr<NonnullOwnPtr<PredictorTransform>> read(WebPLoadingContext&, LittleEndianInputBitStream&, IntSize const& image_size);
    virtual ErrorOr<void> transform(Bitmap&) override;

private:
    PredictorTransform(int size_bits, NonnullRefPtr<Bitmap> predictor_bitmap)
        : m_size_bits(size_bits)
        , m_predictor_bitmap(predictor_bitmap)
    {
    }

    static u8 Average2(u8 a, u8 b)
    {
        return (a + b) / 2;
    }

    static u32 Select(u32 L, u32 T, u32 TL) {
      // "L = left pixel, T = top pixel, TL = top left pixel."

      // XXX maybe don't need the shift and could just mask?
#define ALPHA(x) ((x >> 24) & 0xff)
#define RED(x) ((x >> 16) & 0xff)
#define GREEN(x) ((x >> 8) & 0xff)
#define BLUE(x) (x & 0xff)

      // "ARGB component estimates for prediction."
      int pAlpha = ALPHA(L) + ALPHA(T) - ALPHA(TL);
      int pRed = RED(L) + RED(T) - RED(TL);
      int pGreen = GREEN(L) + GREEN(T) - GREEN(TL);
      int pBlue = BLUE(L) + BLUE(T) - BLUE(TL);

      // "Manhattan distances to estimates for left and top pixels."
      int pL = abs(pAlpha - (int)ALPHA(L)) + abs(pRed - (int)RED(L)) +
               abs(pGreen - (int)GREEN(L)) + abs(pBlue - (int)BLUE(L));
      int pT = abs(pAlpha - (int)ALPHA(T)) + abs(pRed - (int)RED(T)) +
               abs(pGreen - (int)GREEN(T)) + abs(pBlue - (int)BLUE(T));

      // "Return either left or top, the one closer to the prediction."
      if (pL < pT) {     // "\[AMENDED\]"
        return L;
      } else {
        return T;
      }

#undef BLUE
#undef GREEN
#undef RED
#undef ALPHA
    }

    // Clamp the input value between 0 and 255.
    static int Clamp(int a)
    {
      return (a < 0) ? 0 : (a > 255) ?  255 : a;
    }

    static int ClampAddSubtractFull(int a, int b, int c)
    {
      return Clamp(a + b - c);
    }

    static int ClampAddSubtractHalf(int a, int b)
    {
      return Clamp(a + (a - b) / 2);
    }

    static ErrorOr<ARGB32> predict(u8 predictor, ARGB32 TL, ARGB32 T, ARGB32 TR, ARGB32 L);
    static ARGB32 inverse_transform(ARGB32 pixel, ARGB32 prediction);

    int m_size_bits;
    NonnullRefPtr<Bitmap> m_predictor_bitmap;
};

ErrorOr<NonnullOwnPtr<PredictorTransform>> PredictorTransform::read(WebPLoadingContext& context, LittleEndianInputBitStream& bit_stream, IntSize const& image_size)
{
    // predictor-image      =  3BIT ; sub-pixel code
    //                         entropy-coded-image
    int size_bits = TRY(bit_stream.read_bits(3)) + 2;
    int block_size = 1 << size_bits;
    IntSize predictor_image_size { ceil_div(image_size.width(), block_size), ceil_div(image_size.height(), block_size) };

    auto predictor_bitmap = TRY(decode_webp_chunk_VP8L_image(context, ImageKind::EntropyCoded, BitmapFormat::BGRx8888, predictor_image_size, bit_stream));
    MUST(save_bitmap(*predictor_bitmap, "webp-predictor.png"sv));

    return adopt_nonnull_own_or_enomem(new (nothrow) PredictorTransform(size_bits, move(predictor_bitmap)));
}

ErrorOr<void> PredictorTransform::transform(Bitmap& bitmap)
{
    // "There are special handling rules for some border pixels.
    //  If there is a prediction transform, regardless of the mode [0..13] for these pixels,
    //  the predicted value for the left-topmost pixel of the image is 0xff000000,
    bitmap.scanline(0)[0] = inverse_transform(bitmap.scanline(0)[0], 0xff'00'00'00);

    //  L-pixel for all pixels on the top row,
    for (int x = 1; x < bitmap.width(); ++x)
        bitmap.scanline(0)[x] = inverse_transform(bitmap.scanline(0)[x], bitmap.scanline(0)[x - 1]);

    //  and T-pixel for all pixels on the leftmost column."
    for (int y = 1; y < bitmap.height(); ++y)
        bitmap.scanline(y)[0] = inverse_transform(bitmap.scanline(y)[0], bitmap.scanline(y - 1)[0]);

    ARGB32* bitmap_previous_scanline = bitmap.scanline(0);
    for (int y = 1; y < bitmap.height(); ++y) {
        ARGB32* bitmap_scanline = bitmap.scanline(y);

        ARGB32 TL = bitmap_previous_scanline[0];
        ARGB32 T = bitmap_previous_scanline[1];
        ARGB32 TR = 2 < bitmap.width() ? bitmap_previous_scanline[2] : bitmap_previous_scanline[0];

        ARGB32 L = bitmap_scanline[0];

        int predictor_y = y >> m_size_bits;
        ARGB32* predictor_scanline = m_predictor_bitmap->scanline(predictor_y);

        for (int x = 1; x < bitmap.width(); ++x) {
            int predictor_x = x >> m_size_bits;

            // https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#51_roles_of_image_data
            // "The green component of a pixel defines which of the 14 predictors is used within a particular block of the ARGB image."
            u8 predictor = Color::from_argb(predictor_scanline[predictor_x]).green();

            ARGB32 predicted = TRY(predict(predictor, TL, T, TR, L));
            bitmap_scanline[x] = inverse_transform(bitmap_scanline[x], predicted);

            TL = T;
            T = TR;

            // "Addressing the TR-pixel for pixels on the rightmost column is exceptional.
            //  The pixels on the rightmost column are predicted by using the modes [0..13] just like pixels not on the border,
            //  but the leftmost pixel on the same row as the current pixel is instead used as the TR-pixel."
            TR = x + 2 < bitmap.width() ? bitmap_previous_scanline[x + 2] : bitmap_previous_scanline[0];

            L = bitmap_scanline[x];
        }

        bitmap_previous_scanline = bitmap_scanline;
    }
    return {};
}

ErrorOr<ARGB32> PredictorTransform::predict(u8 predictor, ARGB32 TL, ARGB32 T, ARGB32 TR, ARGB32 L)
{
    switch (predictor) {
    case 0:
        // "0xff000000 (represents solid black color in ARGB)"
        return 0xff'00'00'00;
    case 1:
        // "L"
        return L;
    case 2:
        // "T"
        return T;
    case 3:
        // "TR"
        return TR;
    case 4:
        // "TL"
        return TL;
    case 5: {
        // "Average2(Average2(L, TR), T) for each ARGB component"
        Color color_L = Color::from_argb(L);
        Color color_TR = Color::from_argb(TR);
        Color color_T = Color::from_argb(T);
        return Color(Average2(Average2(color_L.red(), color_TR.red()), color_T.red()),
                     Average2(Average2(color_L.green(), color_TR.green()), color_T.green()),
                     Average2(Average2(color_L.blue(), color_TR.blue()), color_T.blue()),
                     Average2(Average2(color_L.alpha(), color_TR.alpha()), color_T.alpha())).value();
    }
    case 6: {
        // "Average2(L, TL)"
        Color color_L = Color::from_argb(L);
        Color color_TL = Color::from_argb(TL);
        return Color(Average2(color_L.red(), color_TL.red()),
                     Average2(color_L.green(), color_TL.green()),
                     Average2(color_L.blue(), color_TL.blue()),
                     Average2(color_L.alpha(), color_TL.alpha())).value();
    }
    case 7: {
        // "Average2(L, T) for each ARGB component"
        Color color_L = Color::from_argb(L);
        Color color_T = Color::from_argb(T);
        return Color(Average2(color_L.red(), color_T.red()),
                     Average2(color_L.green(), color_T.green()),
                     Average2(color_L.blue(), color_T.blue()),
                     Average2(color_L.alpha(), color_T.alpha())).value();
    }
    case 8: {
        // "Average2(TL, T) for each ARGB component"
        Color color_TL = Color::from_argb(TL);
        Color color_T = Color::from_argb(T);
        return Color(Average2(color_TL.red(), color_T.red()),
                     Average2(color_TL.green(), color_T.green()),
                     Average2(color_TL.blue(), color_T.blue()),
                     Average2(color_TL.alpha(), color_T.alpha())).value();
    }
    case 9: {
        // "Average2(T, TR) for each ARGB component"
        Color color_T = Color::from_argb(T);
        Color color_TR = Color::from_argb(TR);
        return Color(Average2(color_T.red(), color_TR.red()),
                     Average2(color_T.green(), color_TR.green()),
                     Average2(color_T.blue(), color_TR.blue()),
                     Average2(color_T.alpha(), color_TR.alpha())).value();
    }
    case 10: {
        // "Average2(Average2(L, TL), Average2(T, TR)) for each ARGB component"
        Color color_L = Color::from_argb(L);
        Color color_TL = Color::from_argb(TL);
        Color color_T = Color::from_argb(T);
        Color color_TR = Color::from_argb(TR);
        return Color(Average2(Average2(color_L.red(), color_TL.red()), Average2(color_T.red(), color_TR.red())),
                     Average2(Average2(color_L.green(), color_TL.green()), Average2(color_T.green(), color_TR.green())),
                     Average2(Average2(color_L.blue(), color_TL.blue()), Average2(color_T.blue(), color_TR.blue())),
                     Average2(Average2(color_L.alpha(), color_TL.alpha()), Average2(color_T.alpha(), color_TR.alpha()))).value();
    }
    case 11:
        // "Select(L, T, TL)"
        return Select(L, T, TL);
    case 12: {
        // "ClampAddSubtractFull(L, T, TL) for each ARGB component" 
        Color color_L = Color::from_argb(L);
        Color color_T = Color::from_argb(T);
        Color color_TL = Color::from_argb(TL);
        return Color(ClampAddSubtractFull(color_L.red(), color_T.red(), color_TL.red()),
                     ClampAddSubtractFull(color_L.green(), color_T.green(), color_TL.green()),
                     ClampAddSubtractFull(color_L.blue(), color_T.blue(), color_TL.blue()),
                     ClampAddSubtractFull(color_L.alpha(), color_T.alpha(), color_TL.alpha())).value();
    }
    case 13: {
        // "ClampAddSubtractHalf(Average2(L, T), TL) for each ARGB component"
        Color color_L = Color::from_argb(L);
        Color color_T = Color::from_argb(T);
        Color color_TL = Color::from_argb(TL);
        return Color(ClampAddSubtractHalf(Average2(color_L.red(), color_T.red()), color_TL.red()),
                     ClampAddSubtractHalf(Average2(color_L.green(), color_T.green()), color_TL.green()),
                     ClampAddSubtractHalf(Average2(color_L.blue(), color_T.blue()), color_TL.blue()),
                     ClampAddSubtractHalf(Average2(color_L.alpha(), color_T.alpha()), color_TL.alpha())).value();
    }
    }
    return Error::from_string_literal("WebPImageDecoderPlugin: invalid preditor");
}

ARGB32 PredictorTransform::inverse_transform(ARGB32 pixel, ARGB32 prediction)
{
    Color pixel_color = Color::from_argb(pixel);
    Color prediction_color = Color::from_argb(prediction);
    return Color(pixel_color.red() + prediction_color.red(),
                 pixel_color.green() + prediction_color.green(),
                 pixel_color.blue() + prediction_color.blue(),
                 pixel_color.alpha() + prediction_color.alpha()).value();
}

// https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#42_color_transform
class ColorTransform : public Transform {
public:
    static ErrorOr<NonnullOwnPtr<ColorTransform>> read(WebPLoadingContext&, LittleEndianInputBitStream&, IntSize const& image_size);
    virtual ErrorOr<void> transform(Bitmap&) override;

private:
    ColorTransform(int size_bits, NonnullRefPtr<Bitmap> color_bitmap)
        : m_size_bits(size_bits)
        , m_color_bitmap(color_bitmap)
    {
    }

    static i8 ColorTransformDelta(i8 transform, i8 color)
    {
        return (transform * color) >> 5;
    }

    static ARGB32 inverse_transform(ARGB32 pixel, ARGB32 transform);

    int m_size_bits;
    NonnullRefPtr<Bitmap> m_color_bitmap;
};

ErrorOr<NonnullOwnPtr<ColorTransform>> ColorTransform::read(WebPLoadingContext& context, LittleEndianInputBitStream& bit_stream, IntSize const& image_size)
{
    // color-image          =  3BIT ; sub-pixel code
    //                         entropy-coded-image
    int size_bits = TRY(bit_stream.read_bits(3)) + 2;
    int block_size = 1 << size_bits;
    IntSize color_image_size { ceil_div(image_size.width(), block_size), ceil_div(image_size.height(), block_size) };

    auto color_bitmap = TRY(decode_webp_chunk_VP8L_image(context, ImageKind::EntropyCoded, BitmapFormat::BGRx8888, color_image_size, bit_stream));
    MUST(save_bitmap(*color_bitmap, "webp-color.png"sv));

    return adopt_nonnull_own_or_enomem(new (nothrow) ColorTransform(size_bits, move(color_bitmap)));
}

ErrorOr<void> ColorTransform::transform(Bitmap& bitmap)
{
    for (int y = 0; y < bitmap.height(); ++y) {
        ARGB32* bitmap_scanline = bitmap.scanline(y);

        int color_y = y >> m_size_bits;
        ARGB32* color_scanline = m_color_bitmap->scanline(color_y);

        for (int x = 0; x < bitmap.width(); ++x) {
            int color_x = x >> m_size_bits;
            bitmap_scanline[x] = inverse_transform(bitmap_scanline[x], color_scanline[color_x]);
        }
    }
    return {};
}

ARGB32 ColorTransform::inverse_transform(ARGB32 pixel, ARGB32 transform)
{
    // https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#51_roles_of_image_data
    // "Each ColorTransformElement 'cte' is treated as a pixel whose alpha component is 255,
    // red component is cte.red_to_blue, green component is cte.green_to_blue
    // and blue component is cte.green_to_red."
    Color transform_color = Color::from_argb(transform);
    i8 red_to_blue = static_cast<i8>(transform_color.red());
    i8 green_to_blue = static_cast<i8>(transform_color.green());
    i8 green_to_red = static_cast<i8>(transform_color.blue());

    Color pixel_color = Color::from_argb(pixel);

    // "Transformed values of red and blue components"
    int tmp_red = pixel_color.red();
    int green = pixel_color.green();
    int tmp_blue = pixel_color.blue();

    // "Applying the inverse transform is just adding the color transform deltas"
    tmp_red += ColorTransformDelta(green_to_red, green);
    tmp_blue += ColorTransformDelta(green_to_blue, green);
    tmp_blue += ColorTransformDelta(red_to_blue, tmp_red & 0xff);

    return Color(tmp_red & 0xff, green, tmp_blue & 0xff, pixel_color.alpha()).value();
}

// https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#43_subtract_green_transform
class SubtractGreenTransform : public Transform {
public:
    virtual ErrorOr<void> transform(Bitmap&) override;
};

ErrorOr<void> SubtractGreenTransform::transform(Bitmap& bitmap)
{
    for (ARGB32& pixel : bitmap) {
        Color color = Color::from_argb(pixel);
        u8 red = (color.red() + color.green()) & 0xff;
        u8 blue = (color.blue() + color.green()) & 0xff;
        pixel = Color(red, color.green(), blue, color.alpha()).value();
    }
    return {};
}

}

// https://developers.google.com/speed/webp/docs/riff_container#simple_file_format_lossless
// https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#7_overall_structure_of_the_format
static ErrorOr<void> decode_webp_chunk_VP8L(WebPLoadingContext& context, Chunk const& vp8l_chunk)
{
    VERIFY(context.first_chunk->type == FourCC("VP8L") || context.first_chunk->type == FourCC("VP8X"));
    VERIFY(vp8l_chunk.type == FourCC("VP8L"));

    auto vp8l_header = TRY(decode_webp_chunk_VP8L_header(context, vp8l_chunk));

    // Check that size in VP8X chunk matches dimensions in VP8L chunk if both are present.
    if (context.first_chunk->type == FourCC("VP8X")) {
        if (vp8l_header.width != context.vp8x_header.width)
            return context.error("WebPImageDecoderPlugin: VP8X and VP8L chunks store different widths");
        if (vp8l_header.height != context.vp8x_header.height)
            return context.error("WebPImageDecoderPlugin: VP8X and VP8L chunks store different heights");
        if (vp8l_header.is_alpha_used != context.vp8x_header.has_alpha)
            return context.error("WebPImageDecoderPlugin: VP8X and VP8L chunks store different alpha");
    }

    FixedMemoryStream memory_stream { vp8l_chunk.data.slice(5) };
    LittleEndianInputBitStream bit_stream { MaybeOwned<Stream>(memory_stream) };

    // image-stream = optional-transform spatially-coded-image

    // https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#4_transformations
    // https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#72_structure_of_transforms

    // optional-transform   =  (%b1 transform optional-transform) / %b0
    // "Each transform is allowed to be used only once."
    u8 seen_transforms = 0;
    Vector<NonnullOwnPtr<Transform>> transforms;
    while (TRY(bit_stream.read_bits(1))) {
        // transform            =  predictor-tx / color-tx / subtract-green-tx
        // transform            =/ color-indexing-tx

        enum TransformType {
            // predictor-tx         =  %b00 predictor-image
            PREDICTOR_TRANSFORM = 0,

            // color-tx             =  %b01 color-image
            COLOR_TRANSFORM = 1,

            // subtract-green-tx    =  %b10
            SUBTRACT_GREEN_TRANSFORM = 2,

            // color-indexing-tx    =  %b11 color-indexing-image
            COLOR_INDEXING_TRANSFORM = 3,
        };

        TransformType transform_type = static_cast<TransformType>(TRY(bit_stream.read_bits(2)));
        dbgln_if(WEBP_DEBUG, "transform type {}", (int)transform_type);

        // Check that every transfom is used at most once.
        u8 mask = 1 << (int)transform_type;
        if (seen_transforms & mask)
            return context.error("WebPImageDecoderPlugin: transform type used multiple times");
        seen_transforms |= mask;

        switch (transform_type) {
        case PREDICTOR_TRANSFORM:
            TRY(transforms.try_append(TRY(PredictorTransform::read(context, bit_stream, context.size.value()))));
            break;
        case COLOR_TRANSFORM:
            TRY(transforms.try_append(TRY(ColorTransform::read(context, bit_stream, context.size.value()))));
            break;
        case SUBTRACT_GREEN_TRANSFORM:
            TRY(transforms.try_append(TRY(try_make<SubtractGreenTransform>())));
            break;
        case COLOR_INDEXING_TRANSFORM:
            return context.error("WebPImageDecoderPlugin: VP8L COLOR_INDEXING_TRANSFORM handling not yet implemented");
        }
    }

    auto format = vp8l_header.is_alpha_used ? BitmapFormat::BGRA8888 : BitmapFormat::BGRx8888;
    context.bitmap = TRY(decode_webp_chunk_VP8L_image(context, ImageKind::SpatiallyCoded, format, context.size.value(), bit_stream));

    MUST(save_bitmap(*context.bitmap, "webp-untransformed.png"sv));

    // Transforms have to be applied in the reverse order they appear in in the file.
int i = 0;
    for (auto const& transform : transforms.in_reverse()) {
    //for (auto const& transform : transforms) {
        TRY(transform->transform(*context.bitmap));
        i++;
        MUST(save_bitmap(*context.bitmap, MUST(String::formatted("webp-untransformed-{}.png"sv, i))));
    }

    return {};
}

static ErrorOr<VP8XHeader> decode_webp_chunk_VP8X(WebPLoadingContext& context, Chunk const& vp8x_chunk)
{
    VERIFY(vp8x_chunk.type == FourCC("VP8X"));

    // The VP8X chunk is documented at "Extended WebP file header:" at the end of
    // https://developers.google.com/speed/webp/docs/riff_container#extended_file_format
    if (vp8x_chunk.data.size() < 10)
        return context.error("WebPImageDecoderPlugin: VP8X chunk too small");

    u8 const* data = vp8x_chunk.data.data();

    // 1 byte flags
    // "Reserved (Rsv): 2 bits   MUST be 0. Readers MUST ignore this field.
    //  ICC profile (I): 1 bit   Set if the file contains an ICC profile.
    //  Alpha (L): 1 bit         Set if any of the frames of the image contain transparency information ("alpha").
    //  Exif metadata (E): 1 bit Set if the file contains Exif metadata.
    //  XMP metadata (X): 1 bit  Set if the file contains XMP metadata.
    //  Animation (A): 1 bit     Set if this is an animated image. Data in 'ANIM' and 'ANMF' chunks should be used to control the animation.
    //  Reserved (R): 1 bit      MUST be 0. Readers MUST ignore this field."
    u8 flags = data[0];
    bool has_icc = flags & 0x20;
    bool has_alpha = flags & 0x10;
    bool has_exif = flags & 0x8;
    bool has_xmp = flags & 0x4;
    bool has_animation = flags & 0x2;

    // 3 bytes reserved
    // 3 bytes width minus one
    u32 width = (data[4] | (data[5] << 8) | (data[6] << 16)) + 1;

    // 3 bytes height minus one
    u32 height = (data[7] | (data[8] << 8) | (data[9] << 16)) + 1;

    dbgln_if(WEBP_DEBUG, "flags 0x{:x} --{}{}{}{}{}{}, width {}, height {}",
        flags,
        has_icc ? " icc" : "",
        has_alpha ? " alpha" : "",
        has_exif ? " exif" : "",
        has_xmp ? " xmp" : "",
        has_animation ? " anim" : "",
        (flags & 0x3e) == 0 ? " none" : "",
        width, height);

    return VP8XHeader { has_icc, has_alpha, has_exif, has_xmp, has_animation, width, height };
}

// https://developers.google.com/speed/webp/docs/riff_container#animation
static ErrorOr<ANIMChunk> decode_webp_chunk_ANIM(WebPLoadingContext& context, Chunk const& anim_chunk)
{
    VERIFY(anim_chunk.type == FourCC("ANIM"));
    if (anim_chunk.data.size() < 6)
        return context.error("WebPImageDecoderPlugin: ANIM chunk too small");

    u8 const* data = anim_chunk.data.data();
    u32 background_color = (u32)data[0] | ((u32)data[1] << 8) | ((u32)data[2] << 16) | ((u32)data[3] << 24);
    u16 loop_count = data[4] | (data[5] << 8);

    return ANIMChunk { background_color, loop_count };
}

// https://developers.google.com/speed/webp/docs/riff_container#extended_file_format
static ErrorOr<void> decode_webp_extended(WebPLoadingContext& context, ReadonlyBytes chunks)
{
    VERIFY(context.first_chunk->type == FourCC("VP8X"));

    // FIXME: This isn't quite to spec, which says
    // "All chunks SHOULD be placed in the same order as listed above.
    //  If a chunk appears in the wrong place, the file is invalid, but readers MAY parse the file, ignoring the chunks that are out of order."
    auto store = [](auto& field, Chunk const& chunk) {
        if (!field.has_value())
            field = chunk;
    };
    while (!chunks.is_empty()) {
        auto chunk = TRY(decode_webp_advance_chunk(context, chunks));

        if (chunk.type == FourCC("ICCP"))
            store(context.iccp_chunk, chunk);
        else if (chunk.type == FourCC("ALPH"))
            store(context.alpha_chunk, chunk);
        else if (chunk.type == FourCC("ANIM"))
            store(context.animation_header_chunk, chunk);
        else if (chunk.type == FourCC("ANMF"))
            TRY(context.animation_frame_chunks.try_append(chunk));
        else if (chunk.type == FourCC("EXIF"))
            store(context.exif_chunk, chunk);
        else if (chunk.type == FourCC("XMP "))
            store(context.xmp_chunk, chunk);
        else if (chunk.type == FourCC("VP8 ") || chunk.type == FourCC("VP8L"))
            store(context.image_data_chunk, chunk);
    }

    // Validate chunks.

    // https://developers.google.com/speed/webp/docs/riff_container#animation
    // "ANIM Chunk: [...] This chunk MUST appear if the Animation flag in the VP8X chunk is set. If the Animation flag is not set and this chunk is present, it MUST be ignored."
    if (context.vp8x_header.has_animation && !context.animation_header_chunk.has_value())
        return context.error("WebPImageDecoderPlugin: Header claims animation, but no ANIM chunk");
    if (!context.vp8x_header.has_animation && context.animation_header_chunk.has_value()) {
        dbgln_if(WEBP_DEBUG, "WebPImageDecoderPlugin: Header claims no animation, but ANIM chunk present. Ignoring ANIM chunk.");
        context.animation_header_chunk.clear();
    }

    // "ANMF Chunk: [...] If the Animation flag is not set, then this chunk SHOULD NOT be present."
    if (!context.vp8x_header.has_animation && context.animation_header_chunk.has_value()) {
        dbgln_if(WEBP_DEBUG, "WebPImageDecoderPlugin: Header claims no animation, but ANMF chunks present. Ignoring ANMF chunks.");
        context.animation_frame_chunks.clear();
    }

    // https://developers.google.com/speed/webp/docs/riff_container#alpha
    // "A frame containing a 'VP8L' chunk SHOULD NOT contain this chunk."
    // FIXME: Also check in ANMF chunks.
    if (context.alpha_chunk.has_value() && context.image_data_chunk.has_value() && context.image_data_chunk->type == FourCC("VP8L")) {
        dbgln_if(WEBP_DEBUG, "WebPImageDecoderPlugin: VP8L frames should not have ALPH chunks. Ignoring ALPH chunk.");
        context.alpha_chunk.clear();
    }

    // https://developers.google.com/speed/webp/docs/riff_container#color_profile
    // "This chunk MUST appear before the image data."
    // FIXME: Doesn't check animated files.
    if (context.iccp_chunk.has_value() && context.image_data_chunk.has_value() && context.iccp_chunk->data.data() > context.image_data_chunk->data.data())
        return context.error("WebPImageDecoderPlugin: ICCP chunk is after image data");

    context.state = WebPLoadingContext::State::ChunksDecoded;
    return {};
}

static ErrorOr<void> read_webp_first_chunk(WebPLoadingContext& context)
{
    if (context.state >= WebPLoadingContext::State::FirstChunkRead)
        return {};

    if (context.state < WebPLoadingContext::HeaderDecoded)
        TRY(decode_webp_header(context));

    context.chunks_cursor = context.data.slice(sizeof(WebPFileHeader));
    auto first_chunk = TRY(decode_webp_advance_chunk(context, context.chunks_cursor));

    if (first_chunk.type != FourCC("VP8 ") && first_chunk.type != FourCC("VP8L") && first_chunk.type != FourCC("VP8X"))
        return context.error("WebPImageDecoderPlugin: Invalid first chunk type");

    context.first_chunk = first_chunk;
    context.state = WebPLoadingContext::State::FirstChunkRead;

    if (first_chunk.type == FourCC("VP8 ") || first_chunk.type == FourCC("VP8L"))
        context.image_data_chunk = first_chunk;

    return {};
}

static ErrorOr<void> decode_webp_first_chunk(WebPLoadingContext& context)
{
    if (context.state >= WebPLoadingContext::State::FirstChunkDecoded)
        return {};

    if (context.state < WebPLoadingContext::FirstChunkRead)
        TRY(read_webp_first_chunk(context));

    if (context.first_chunk->type == FourCC("VP8 ")) {
        context.vp8_header = TRY(decode_webp_chunk_VP8_header(context, context.first_chunk.value()));
        context.size = IntSize { context.vp8_header.width, context.vp8_header.height };
        context.state = WebPLoadingContext::State::FirstChunkDecoded;
        return {};
    }
    if (context.first_chunk->type == FourCC("VP8L")) {
        context.vp8l_header = TRY(decode_webp_chunk_VP8L_header(context, context.first_chunk.value()));
        context.size = IntSize { context.vp8l_header.width, context.vp8l_header.height };
        context.state = WebPLoadingContext::State::FirstChunkDecoded;
        return {};
    }
    VERIFY(context.first_chunk->type == FourCC("VP8X"));
    context.vp8x_header = TRY(decode_webp_chunk_VP8X(context, context.first_chunk.value()));
    context.size = IntSize { context.vp8x_header.width, context.vp8x_header.height };
    context.state = WebPLoadingContext::State::FirstChunkDecoded;
    return {};
}

static ErrorOr<void> decode_webp_chunks(WebPLoadingContext& context)
{
    if (context.state >= WebPLoadingContext::State::ChunksDecoded)
        return {};

    if (context.state < WebPLoadingContext::FirstChunkDecoded)
        TRY(decode_webp_first_chunk(context));

    if (context.first_chunk->type == FourCC("VP8X"))
        return decode_webp_extended(context, context.chunks_cursor);

    context.state = WebPLoadingContext::State::ChunksDecoded;
    return {};
}

WebPImageDecoderPlugin::WebPImageDecoderPlugin(ReadonlyBytes data, OwnPtr<WebPLoadingContext> context)
    : m_context(move(context))
{
    m_context->data = data;
}

WebPImageDecoderPlugin::~WebPImageDecoderPlugin() = default;

IntSize WebPImageDecoderPlugin::size()
{
    if (m_context->state == WebPLoadingContext::State::Error)
        return {};

    if (m_context->state < WebPLoadingContext::State::FirstChunkDecoded) {
        if (decode_webp_first_chunk(*m_context).is_error())
            return {};
    }

    return m_context->size.value();
}

void WebPImageDecoderPlugin::set_volatile()
{
    if (m_context->bitmap)
        m_context->bitmap->set_volatile();
}

bool WebPImageDecoderPlugin::set_nonvolatile(bool& was_purged)
{
    if (!m_context->bitmap)
        return false;
    return m_context->bitmap->set_nonvolatile(was_purged);
}

bool WebPImageDecoderPlugin::initialize()
{
    return !decode_webp_header(*m_context).is_error();
}

bool WebPImageDecoderPlugin::sniff(ReadonlyBytes data)
{
    WebPLoadingContext context;
    context.data = data;
    return !decode_webp_header(context).is_error();
}

ErrorOr<NonnullOwnPtr<ImageDecoderPlugin>> WebPImageDecoderPlugin::create(ReadonlyBytes data)
{
    auto context = TRY(try_make<WebPLoadingContext>());
    return adopt_nonnull_own_or_enomem(new (nothrow) WebPImageDecoderPlugin(data, move(context)));
}

bool WebPImageDecoderPlugin::is_animated()
{
    if (m_context->state == WebPLoadingContext::State::Error)
        return false;

    if (m_context->state < WebPLoadingContext::State::FirstChunkDecoded) {
        if (decode_webp_first_chunk(*m_context).is_error())
            return false;
    }

    return m_context->first_chunk->type == FourCC("VP8X") && m_context->vp8x_header.has_animation;
}

size_t WebPImageDecoderPlugin::loop_count()
{
    if (!is_animated())
        return 0;

    if (m_context->state < WebPLoadingContext::State::ChunksDecoded) {
        if (decode_webp_chunks(*m_context).is_error())
            return 0;
    }

    auto anim_or_error = decode_webp_chunk_ANIM(*m_context, m_context->animation_header_chunk.value());
    if (decode_webp_chunks(*m_context).is_error())
        return 0;

    return anim_or_error.value().loop_count;
}

size_t WebPImageDecoderPlugin::frame_count()
{
    if (!is_animated())
        return 1;

    if (m_context->state < WebPLoadingContext::State::ChunksDecoded) {
        if (decode_webp_chunks(*m_context).is_error())
            return 1;
    }

    return m_context->animation_frame_chunks.size();
}

ErrorOr<ImageFrameDescriptor> WebPImageDecoderPlugin::frame(size_t index)
{
    if (index >= frame_count())
        return Error::from_string_literal("WebPImageDecoderPlugin: Invalid frame index");

    if (m_context->state == WebPLoadingContext::State::Error)
        return Error::from_string_literal("WebPImageDecoderPlugin: Decoding failed");

    if (m_context->state < WebPLoadingContext::State::ChunksDecoded)
        TRY(decode_webp_chunks(*m_context));

    if (is_animated())
        return Error::from_string_literal("WebPImageDecoderPlugin: decoding of animated files not yet implemented");

    if (m_context->image_data_chunk.has_value() && m_context->image_data_chunk->type == FourCC("VP8L")) {
        if (m_context->state < WebPLoadingContext::State::BitmapDecoded) {
            TRY(decode_webp_chunk_VP8L(*m_context, m_context->image_data_chunk.value()));
            m_context->state = WebPLoadingContext::State::BitmapDecoded;
        }

        VERIFY(m_context->bitmap);
        return ImageFrameDescriptor { m_context->bitmap, 0 };
    }

    return Error::from_string_literal("WebPImageDecoderPlugin: decoding not yet implemented");
}

ErrorOr<Optional<ReadonlyBytes>> WebPImageDecoderPlugin::icc_data()
{
    TRY(decode_webp_chunks(*m_context));

    // FIXME: "If this chunk is not present, sRGB SHOULD be assumed."

    return m_context->iccp_chunk.map([](auto iccp_chunk) { return iccp_chunk.data; });
}

}

template<>
struct AK::Formatter<Gfx::FourCC> : StandardFormatter {
    ErrorOr<void> format(FormatBuilder& builder, Gfx::FourCC const& four_cc)
    {
        TRY(builder.put_padding('\'', 1));
        TRY(builder.put_padding(four_cc.cc[0], 1));
        TRY(builder.put_padding(four_cc.cc[1], 1));
        TRY(builder.put_padding(four_cc.cc[2], 1));
        TRY(builder.put_padding(four_cc.cc[3], 1));
        TRY(builder.put_padding('\'', 1));
        return {};
    }
};
