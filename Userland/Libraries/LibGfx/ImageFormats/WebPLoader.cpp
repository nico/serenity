/*
 * Copyright (c) 2023, Nico Weber <thakis@chromium.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#define WEBP_DEBUG 1

#include <AK/BinarySearch.h>
#include <AK/BitStream.h>
#include <AK/Debug.h>
#include <AK/Endian.h>
#include <AK/Format.h>
#include <AK/MemoryStream.h>
#include <AK/Vector.h>
#include <LibGfx/ImageFormats/WebPLoader.h>

// Overview: https://developers.google.com/speed/webp/docs/compression
// Container: https://developers.google.com/speed/webp/docs/riff_container
// Lossless format: https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification
// Lossy format: https://datatracker.ietf.org/doc/html/rfc6386

namespace Gfx {

namespace {

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

class CanonicalCode {
public:
    CanonicalCode() = default;
    ErrorOr<u32> read_symbol(LittleEndianInputBitStream&) const;
    ErrorOr<void> write_symbol(LittleEndianOutputBitStream&, u32) const;

    static Optional<CanonicalCode> from_bytes(ReadonlyBytes);

private:
    static constexpr size_t max_allowed_prefixed_code_length = 8;

    struct PrefixTableEntry {
        u16 symbol_value { 0 };
        u16 code_length { 0 };
    };

    // Decompression - indexed by code
    Vector<u16, 286> m_symbol_codes;
    Vector<u16, 286> m_symbol_values;

    Array<PrefixTableEntry, 1 << max_allowed_prefixed_code_length> m_prefix_table {};
    size_t m_max_prefixed_code_length { 0 };
};

static consteval u8 reverse8(u8 value)
{
    u8 result = 0;
    for (size_t i = 0; i < 8; i++) {
        if (value & (1 << i))
            result |= 1 << (7 - i);
    }
    return result;
}
static consteval Array<u8, UINT8_MAX + 1> generate_reverse8_lookup_table()
{
    Array<u8, UINT8_MAX + 1> array;
    for (size_t i = 0; i <= UINT8_MAX; i++) {
        array[i] = reverse8(i);
    }
    return array;
}
static constexpr auto reverse8_lookup_table = generate_reverse8_lookup_table();

// Lookup-table based bit swap
ALWAYS_INLINE static u16 fast_reverse16(u16 value, size_t bits)
{
    VERIFY(bits <= 16);

    u16 lo = value & 0xff;
    u16 hi = value >> 8;

    u16 reversed = (u16)((reverse8_lookup_table[lo] << 8) | reverse8_lookup_table[hi]);

    return reversed >> (16 - bits);
}

//static constexpr u8 deflate_special_code_length_copy = 16;
//static constexpr u8 deflate_special_code_length_zeros = 17;
//static constexpr u8 deflate_special_code_length_long_zeros = 18;

Optional<CanonicalCode> CanonicalCode::from_bytes(ReadonlyBytes bytes)
{
    // FIXME: I can't quite follow the algorithm here, but it seems to work.

    CanonicalCode code;

    auto non_zero_symbols = 0;
    auto last_non_zero = -1;
    for (size_t i = 0; i < bytes.size(); i++) {
        if (bytes[i] != 0) {
            non_zero_symbols++;
            last_non_zero = i;
        }
    }

    if (non_zero_symbols == 1) { // special case - only 1 symbol
        code.m_prefix_table[0] = PrefixTableEntry { static_cast<u16>(last_non_zero), 1u };
        code.m_prefix_table[1] = code.m_prefix_table[0];
        code.m_max_prefixed_code_length = 1;
        return code;
    }

    struct PrefixCode {
        u16 symbol_code { 0 };
        u16 symbol_value { 0 };
        u16 code_length { 0 };
    };
    Array<PrefixCode, 1 << CanonicalCode::max_allowed_prefixed_code_length> prefix_codes;
    size_t number_of_prefix_codes = 0;

    auto next_code = 0;
    for (size_t code_length = 1; code_length <= 15; ++code_length) {
        next_code <<= 1;
        auto start_bit = 1 << code_length;

        for (size_t symbol = 0; symbol < bytes.size(); ++symbol) {
            if (bytes[symbol] != code_length)
                continue;

            if (next_code > start_bit)
                return {};

            if (code_length <= CanonicalCode::max_allowed_prefixed_code_length) {
                auto& prefix_code = prefix_codes[number_of_prefix_codes++];
                prefix_code.symbol_code = next_code;
                prefix_code.symbol_value = symbol;
                prefix_code.code_length = code_length;

                code.m_max_prefixed_code_length = code_length;
            } else {
                code.m_symbol_codes.append(start_bit | next_code);
                code.m_symbol_values.append(symbol);
            }

            next_code++;
        }
    }

    if (next_code != (1 << 15)) {
        return {};
    }

    for (auto [symbol_code, symbol_value, code_length] : prefix_codes) {
        if (code_length == 0 || code_length > CanonicalCode::max_allowed_prefixed_code_length)
            break;

        auto shift = code.m_max_prefixed_code_length - code_length;
        symbol_code <<= shift;

        for (size_t j = 0; j < (1u << shift); ++j) {
            auto index = fast_reverse16(symbol_code + j, code.m_max_prefixed_code_length);
            code.m_prefix_table[index] = PrefixTableEntry { symbol_value, code_length };
        }
    }

    return code;
}

ErrorOr<u32> CanonicalCode::read_symbol(LittleEndianInputBitStream& stream) const
{
    auto prefix = TRY(stream.peek_bits<size_t>(m_max_prefixed_code_length));

    if (auto [symbol_value, code_length] = m_prefix_table[prefix]; code_length != 0) {
        stream.discard_previously_peeked_bits(code_length);
        return symbol_value;
    }

    auto code_bits = TRY(stream.read_bits<u16>(m_max_prefixed_code_length));
    code_bits = fast_reverse16(code_bits, m_max_prefixed_code_length);
    code_bits |= 1 << m_max_prefixed_code_length;

    for (size_t i = m_max_prefixed_code_length; i < 16; ++i) {
        size_t index;
        if (binary_search(m_symbol_codes.span(), code_bits, &index))
            return m_symbol_values[index];

        code_bits = code_bits << 1 | TRY(stream.read_bit());
    }

    return Error::from_string_literal("Symbol exceeds maximum symbol number");
}


// https://developers.google.com/speed/webp/docs/riff_container#simple_file_format_lossless
// https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#7_overall_structure_of_the_format
static ErrorOr<void> decode_webp_chunk_VP8L(WebPLoadingContext& context, Chunk const& vp8l_chunk)
{
    VERIFY(context.first_chunk->type == FourCC("VP8L") || context.first_chunk->type == FourCC("VP8X"));
    VERIFY(vp8l_chunk.type == FourCC("VP8L"));

    // FIXME: could compare width in header with width in VP8X header for VP8X images...
    //TRY(decode_webp_chunk_VP8L_header(context, vp8l_chunk));

    FixedMemoryStream memory_stream { vp8l_chunk.data.slice(5) };
    LittleEndianInputBitStream bit_stream { MaybeOwned<Stream>(memory_stream) };

    // image-stream = optional-transform spatially-coded-image

    // https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#4_transformations
    // https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#72_structure_of_transforms

    // optional-transform   =  (%b1 transform optional-transform) / %b0
    if (TRY(bit_stream.read_bits(1)))
        return context.error("WebPImageDecoderPlugin: VP8L transform handling not yet implemented");

    // https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#623_decoding_entropy-coded_image_data
    // https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#523_color_cache_coding
    // spatially-coded-image =  color-cache-info meta-prefix data

    // color-cache-info      =  %b0
    // color-cache-info      =/ (%b1 4BIT) ; 1 followed by color cache size
    bool has_color_cache_info = TRY(bit_stream.read_bits(1)); // XXX Optional
    int color_cache_size = 0;
    dbgln_if(WEBP_DEBUG, "has_color_cache_info {}", has_color_cache_info);
    if (has_color_cache_info) {
        int color_cache_code_bits = TRY(bit_stream.read_bits(4));

        // "The range of allowed values for color_cache_code_bits is [1..11]. Compliant decoders must indicate a corrupted bitstream for other values."
        if (color_cache_code_bits < 1 || color_cache_code_bits > 11)
            return context.error("WebPImageDecoderPlugin: VP8L invalid color_cache_code_bits");

        color_cache_size = 1 << color_cache_code_bits;
        dbgln_if(WEBP_DEBUG, "color_cache_size {}", color_cache_size);
    }

    // https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#622_decoding_of_meta_prefix_codes
    // "Meta prefix codes may be used only when the image is being used in the role of an ARGB image."
    // meta-prefix           =  %b0 / (%b1 entropy-image)
    bool has_meta_prefix = TRY(bit_stream.read_bits(1));
    dbgln_if(WEBP_DEBUG, "has_meta_prefix {}", has_meta_prefix);
    if (has_meta_prefix) {
        return context.error("WebPImageDecoderPlugin: VP8L meta_prefix not yet implemented");
    }

    // https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#52_encoding_of_image_data
    // "The encoded image data consists of several parts:
    //    1. Decoding and building the prefix codes [AMENDED2]
    //    2. Meta prefix codes
    //    3. Entropy-coded image data"
    // data                  =  prefix-codes lz77-coded-image
    // prefix-codes          =  prefix-code-group *prefix-codes
    // prefix-code-group     =
    //     5prefix-code ; See "Interpretation of Meta Prefix Codes" to
    //                  ; understand what each of these five prefix
    //                  ; codes are for.
    //
    // prefix-code           =  simple-prefix-code / normal-prefix-code
    // https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#621_decoding_and_building_the_prefix_codes
    // g r b a d
for (int k = 0; k < 5; ++k) {
    bool is_simple_code_length_code = TRY(bit_stream.read_bits(1));
    dbgln_if(WEBP_DEBUG, "is_simple_code_length_code {}", is_simple_code_length_code);

    // Some of this has plenty in common with deflate (cf DeflateDecompressor::decode_codes() in Deflate.cpp in LibCompress)
    if (is_simple_code_length_code) {
        int num_symbols = TRY(bit_stream.read_bits(1)) + 1;
        int is_first_8bits = TRY(bit_stream.read_bits(1));
        int symbol0 = TRY(bit_stream.read_bits(1 + 7 * is_first_8bits));
        dbgln_if(WEBP_DEBUG, "  symbol0 {}", symbol0);
        if (num_symbols == 2) {
            int symbol1 = TRY(bit_stream.read_bits(8));
            dbgln_if(WEBP_DEBUG, "  symbol1 {}", symbol1);
        }
    } else {
        int num_code_lengths = 4 + TRY(bit_stream.read_bits(4));
        dbgln_if(WEBP_DEBUG, "  num_code_lengths {}", num_code_lengths);

        // "If num_code_lengths is > 19, the bit_stream is invalid. [AMENDED3]"
        if (num_code_lengths > 19)
            return context.error("WebPImageDecoderPlugin: invalid num_code_lengths");

        constexpr int kCodeLengthCodes = 19;
        int kCodeLengthCodeOrder[kCodeLengthCodes] = { 17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
        u8 code_length_code_lengths[kCodeLengthCodes] = { 0 };  // All zeros
        for (int i = 0; i < num_code_lengths; ++i) {
            code_length_code_lengths[kCodeLengthCodeOrder[i]] = TRY(bit_stream.read_bits(3));
            dbgln_if(WEBP_DEBUG, "  code_length_code_lengths[{}] = {}", kCodeLengthCodeOrder[i], code_length_code_lengths[kCodeLengthCodeOrder[i]]);
        }

        int max_symbol = num_code_lengths;
        if (TRY(bit_stream.read_bits(1))) {
            int length_nbits = 2 + 2 * TRY(bit_stream.read_bits(3));
            max_symbol = 2 + TRY(bit_stream.read_bits(length_nbits));
            dbgln_if(WEBP_DEBUG, "  extended, length_nbits {} max_symbol {}", length_nbits, max_symbol);
        }

        auto code_length_code_result = CanonicalCode::from_bytes({ code_length_code_lengths, sizeof(code_length_code_lengths) });
        if (!code_length_code_result.has_value())
            return Error::from_string_literal("Failed to decode code length code");
        auto const code_length_code = code_length_code_result.value();

        // Next we extract the code lengths of the code that was used to encode the block.

        // "Once code lengths are read, a prefix code for each symbol type (A, R, G, B, distance) is formed using their respective alphabet sizes:
        //  * G channel: 256 + 24 + color_cache_size
        //  * other literals (A,R,B): 256
        //  * distance code: 40"
        size_t alphabet_size = 256;
        if (k == 0)
            alphabet_size += 24 + color_cache_size;
        else if (k == 3)
            alphabet_size = 40;

        u8 last_non_zero = 8; // "If code 16 is used before a non-zero value has been emitted, a value of 8 is repeated."
        Vector<u8, 286> code_lengths;

        // FIXME: what's max_symbol good for? (seems to work with alphabet_size)
        //while (code_lengths.size() < (size_t)max_symbol) {
        while (code_lengths.size() < alphabet_size) {
            auto symbol = TRY(code_length_code.read_symbol(bit_stream));

            if (symbol < 16 /*deflate_special_code_length_copy*/) {
                dbgln_if(WEBP_DEBUG, "  append {}", symbol);
                code_lengths.append(static_cast<u8>(symbol));
                if (symbol != 0)
                    last_non_zero = symbol;
                continue;
            } else if (symbol == 17 /*deflate_special_code_length_zeros*/) {
                auto nrepeat = 3 + TRY(bit_stream.read_bits(3));
                dbgln_if(WEBP_DEBUG, "  repeat {} zeroes", nrepeat);
                for (size_t j = 0; j < nrepeat; ++j)
                    code_lengths.append(0);
                continue;
            } else if (symbol == 18 /*deflate_special_code_length_long_zeros*/) {
                auto nrepeat = 11 + TRY(bit_stream.read_bits(7));
                dbgln_if(WEBP_DEBUG, "  Repeat {} zeroes", nrepeat);
                for (size_t j = 0; j < nrepeat; ++j)
                    code_lengths.append(0);
                continue;
            } else {
                VERIFY(symbol == 16 /*deflate_special_code_length_copy*/);

                auto nrepeat = 3 + TRY(bit_stream.read_bits(2));
                dbgln_if(WEBP_DEBUG, "  repeat {} {}s", nrepeat, last_non_zero);

                // XXX this is slightly different from deflate
                for (size_t j = 0; j < nrepeat; ++j)
                    code_lengths.append(last_non_zero);
            }
        }

        //if (code_lengths.size() != (size_t)max_symbol)
        if (code_lengths.size() != alphabet_size)
            return Error::from_string_literal("Number of code lengths does not match the sum of codes");

        (void)alphabet_size;

        int i = 0;
        while (i < max_symbol) {
            // FIXME: huff pretree dec
        }

        return context.error("WebPImageDecoderPlugin: extended normal_prefix not yet implemented");
    }
}

    // https://developers.google.com/speed/webp/docs/webp_lossless_bitstream_specification#621_decoding_and_building_the_prefix_codes
    return context.error("WebPImageDecoderPlugin: VP8L pixel decoding not yet implemented");
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
        return Error::from_string_literal("PNGImageDecoderPlugin: Decoding failed");

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
