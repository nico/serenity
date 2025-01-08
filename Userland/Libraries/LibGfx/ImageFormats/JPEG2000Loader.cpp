/*
 * Copyright (c) 2024, Nico Weber <thakis@chromium.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#define JPEG2000_DEBUG 1

#include <AK/BitStream.h>
#include <AK/Debug.h>
#include <AK/Enumerate.h>
#include <AK/MemoryStream.h>
#include <LibGfx/ImageFormats/ISOBMFF/JPEG2000Boxes.h>
#include <LibGfx/ImageFormats/ISOBMFF/Reader.h>
#include <LibGfx/ImageFormats/JPEG2000Loader.h>
#include <LibGfx/ImageFormats/QMArithmeticDecoder.h>
#include <LibTextCodec/Decoder.h>

#include <LibCore/File.h>
#include <LibGfx/ImageFormats/PNGWriter.h>

// Core coding system spec (.jp2 format): T-REC-T.800-201511-S!!PDF-E.pdf available here:
// https://www.itu.int/rec/dologin_pub.asp?lang=e&id=T-REC-T.800-201511-S!!PDF-E&type=items

// There is a useful example bitstream in the spec in:
// J.10 An example of decoding showing intermediate

// Extensions (.jpx format): T-REC-T.801-202106-S!!PDF-E.pdf available here:
// https://handle.itu.int/11.1002/1000/14666-en?locatt=format:pdf&auth

// rfc3745 lists the MIME type. It only mentions the jp2_id_string as magic number.

// A short overview of the JPEG2000 format:
//
// Image Decomposition
// -------------------
//
// 1. An image is first divided into independent tiles
// 2. Each tile is split into component tiles (one each for R, G, B, A)
// 3. Each component tiles undergoes Discrete Wavelet Transform (DWT)
//
// Resolution Levels and Subbands
// ------------------------------
//
// The DWT produces hierarchical resolution levels with these subbands:
// - Level 0: Single LL (Lowpass-Lowpass) subband
// - Level 1+: HL (Highpass-Lowpass), LH (Lowpass-Highpass), and HH (Highpass-Highpass) subbands
//
// Subband Layout:
// +-----+-----+----------+
// | LL0 | HL1 |          |
// +-----+-----+   HL2    |
// | LH1 | HH1 |          |
// +-----+-----+----------+
// |           |          |
// |    LH2    |    HH2   |
// |           |          |
// +-----------+----------+
//
// Precinct Structure
// ------------------
// - Precincts are rectangular regions that span all subbands within a resolution level
// - Typical size: 512k × 512k pixels
// - Most images contain only a single precinct due to this large size
// - "Precinct limited to a subband": portion of precinct covering one subband
//
// Layer System
// -----------
// - Coefficients' bitplanes can be stored separately
// - Groups of bitplanes form "layers"
// - For example, for an 8bpp image, layer 0 might contain the first two bitplanes, layer 1 the next two, etc.
// - Enables progressive refinement of image color resolution
//
// Codeblock Organization
// ----------------------
// - Each precinct is divided into codeblocks
// - A codeblock is the smallest coded unit in JPEG2000
// - Typical codeblock size: 64×64 pixels
// - Codeblocks store coefficient bitplanes from wavelet transformation
// - Independent arithmetic decoder contexts enable parallel decoding
//
// Packets
// -------
// "All compressed image data representing a specific tile, layer, component, resolution level and precinct appears in the
//  codestream in a contiguous segment called a packet."
// A packet contains a packet header, and information about all codeblocks in the packet.

namespace Gfx {

// A JPEG2000 image can be stored in a codestream with markers, similar to a JPEG image,
// or in a JP2 file, which is a container format based on boxes similar to ISOBMFF.

// This is the marker for the codestream version.
// T.800 Annex A, Codestream syntax, A.2 Information in the marker segments and A.3 Construction of the codestream
static constexpr u8 marker_id_string[] = { 0xFF, 0x4F, 0xFF, 0x51 };

// This is the marker for the box version.
// T.800 Annex I, JP2 file format syntax, I.5.1 JPEG 2000 Signature box
static constexpr u8 jp2_id_string[] = { 0x00, 0x00, 0x00, 0x0C, 0x6A, 0x50, 0x20, 0x20, 0x0D, 0x0A, 0x87, 0x0A };

// Table A.2 – List of markers and marker segments
// "Delimiting markers and marker segments"
#define J2K_SOC 0xFF4F // "Start of codestream"
#define J2K_SOT 0xFF90 // "Start of tile-part"
#define J2K_SOD 0xFF93 // "Start of data"
#define J2K_EOC 0xFFD9 // "End of codestream"
// "Fixed information marker segments"
#define J2K_SIZ 0xFF51 // "Image and tile size"
// "Functional marker segments"
#define J2K_COD 0xFF52 // "Coding style default"
#define J2K_COC 0xFF53 // "Coding style component"
#define J2K_RGN 0xFF5E // "Region-of-interest"
#define J2K_QCD 0xFF5C // "Quantization default"
#define J2K_QCC 0xFF5D // "Quantization component"
#define J2K_POC 0xFF5F // "Progression order change"
// "Pointer marker segments"
#define J2K_TLM 0xFF55 // "Tile-part lengths"
#define J2K_PLM 0xFF57 // "Packet length, main header"
#define J2K_PLT 0xFF58 // "Packet length, tile-part header"
#define J2K_PPM 0xFF60 // "Packed packet headers, main header"
#define J2K_PPT 0xFF61 // "Packed packet headers, tile-part header"
// "In-bit-stream markers and marker segments"
#define J2K_SOP 0xFF91 // "Start of packet"
#define J2K_EPH 0xFF92 // "End of packet header"
// "Informational marker segments"
#define J2K_CRG 0xFF63 // "Component registration"
#define J2K_COM 0xFF64 // "Comment"

// A.4.2 Start of tile-part (SOT)
struct StartOfTilePart {
    // "Tile index. This number refers to the tiles in raster order starting at the number 0."
    u16 tile_index { 0 }; // "Isot" in spec.

    // "Length, in bytes, from the beginning of the first byte of this SOT marker segment of the tile-part to
    //  the end of the data of that tile-part. Figure A.16 shows this alignment. Only the last tile-part in the
    //  codestream may contain a 0 for Psot. If the Psot is 0, this tile-part is assumed to contain all data until
    //  the EOC marker."
    u32 tile_part_length { 0 }; // "Psot" in spec.

    // "Tile-part index. There is a specific order required for decoding tile-parts; this index denotes the order
    //  from 0. If there is only one tile-part for a tile, then this value is zero. The tile-parts of this tile shall
    //  appear in the codestream in this order, although not necessarily consecutively."
    u8 tile_part_index { 0 }; // "TPsot" in spec.

    // "Number of tile-parts of a tile in the codestream. Two values are allowed: the correct number of tile-
    //  parts for that tile and zero. A zero value indicates that the number of tile-parts of this tile is not
    //  specified in this tile-part.
    u8 number_of_tile_parts { 0 }; // "TNsot" in spec.
};

static ErrorOr<StartOfTilePart> read_start_of_tile_part(ReadonlyBytes data)
{
    FixedMemoryStream stream { data };

    StartOfTilePart sot;
    sot.tile_index = TRY(stream.read_value<BigEndian<u16>>());
    sot.tile_part_length = TRY(stream.read_value<BigEndian<u32>>());
    sot.tile_part_index = TRY(stream.read_value<u8>());
    sot.number_of_tile_parts = TRY(stream.read_value<u8>());

    dbgln_if(JPEG2000_DEBUG, "JPEG2000ImageDecoderPlugin: SOT marker segment: tile_index={}, tile_part_length={}, tile_part_index={}, number_of_tile_parts={}", sot.tile_index, sot.tile_part_length, sot.tile_part_index, sot.number_of_tile_parts);

    return sot;
}

enum class SubBand {
    HorizontalLowpassVerticalLowpass,   // "LL" in spec
    HorizontalHighpassVerticalLowpass,  // "HL" in spec
    HorizontalLowpassVerticalHighpass,  // "LH" in spec
    HorizontalHighpassVerticalHighpass, // "HH" in spec
};

// A.5.1 Image and tile size (SIZ)
struct ImageAndTileSize {
    // "Denotes capabilities that a decoder needs to properly decode the codestream."
    u16 needed_decoder_capabilities { 0 }; // "Rsiz" in spec.

    // "Width of the reference grid."
    u32 width { 0 }; // "Xsiz" in spec.

    // "Height of the reference grid."
    u32 height { 0 }; // "Ysiz" in spec.

    // "Horizontal offset from the origin of the reference grid to the left side of the image area."
    u32 x_offset { 0 }; // "XOsiz" in spec.

    // "Vertical offset from the origin of the reference grid to the top side of the image area."
    u32 y_offset { 0 }; // "YOsiz" in spec.

    // "Width of one reference tile with respect to the reference grid."
    u32 tile_width { 0 }; // "XTsiz" in spec.

    // "Height of one reference tile with respect to the reference grid."
    u32 tile_height { 0 }; // "YTsiz" in spec.

    // "Horizontal offset from the origin of the reference grid to the left side of the first tile."
    u32 tile_x_offset { 0 }; // "XTOsiz" in spec.

    // "Vertical offset from the origin of the reference grid to the top side of the first tile."
    u32 tile_y_offset { 0 }; // "YTOsiz" in spec.

    // "Csiz" isn't stored in this struct. It corresponds to `components.size()`.

    struct ComponentInformation {
        // "Precision (depth) in bits and sign of the ith component samples."
        u8 depth_and_sign { 0 }; // "Ssiz" in spec.

        // Table A.11 – Component Ssiz parameter
        u8 bit_depth() const { return (depth_and_sign & 0x7F) + 1; }
        bool is_signed() const { return depth_and_sign & 0x80; }

        // "Horizontal separation of a sample of the ith component with respect to the reference grid."
        u8 horizontal_separation { 0 }; // "XRsiz" in spec.

        // "Vertical separation of a sample of the ith component with respect to the reference grid."
        u8 vertical_separation { 0 }; // "YRsiz" in spec.
    };
    Vector<ComponentInformation> components;

    // (B-5)
    u32 number_of_x_tiles() const { return ceil_div(width - x_offset, tile_width); }
    u32 number_of_y_tiles() const { return ceil_div(height - y_offset, tile_height); }

    IntPoint tile_2d_index_from_1d_index(u32 tile_index) const
    {
        // (B-6)
        return { tile_index % number_of_x_tiles(), tile_index / number_of_x_tiles() };
    }

    IntRect reference_grid_coordinates_for_tile(IntPoint tile_2d_index) const
    {
        int p = tile_2d_index.x();
        int q = tile_2d_index.y();
        int tx0 = max(tile_x_offset + p * tile_width, x_offset);      // (B-7)
        int ty0 = max(tile_y_offset + q * tile_height, y_offset);     // (B-8)
        int tx1 = min(tile_x_offset + (p + 1) * tile_width, width);   // (B-9)
        int ty1 = min(tile_y_offset + (q + 1) * tile_height, height); // (B-10)
        return { tx0, ty0, tx1 - tx0, ty1 - ty0 };                    // (B-11)
    }

    IntRect reference_grid_coordinates_for_tile_component(IntRect tile_rect, int component_index) const
    {
        // (B-12)
        int tcx0 = ceil_div(tile_rect.left(), static_cast<int>(components[component_index].horizontal_separation));
        int tcx1 = ceil_div(tile_rect.right(), static_cast<int>(components[component_index].horizontal_separation));
        int tcy0 = ceil_div(tile_rect.top(), static_cast<int>(components[component_index].vertical_separation));
        int tcy1 = ceil_div(tile_rect.bottom(), static_cast<int>(components[component_index].vertical_separation));
        return { tcx0, tcy0, tcx1 - tcx0, tcy1 - tcy0 }; // (B-13)
    }

    IntRect reference_grid_coordinates_for_tile_component(IntPoint tile_2d_index, int component_index) const
    {
        auto tile_rect = reference_grid_coordinates_for_tile(tile_2d_index);
        return reference_grid_coordinates_for_tile_component(tile_rect, component_index);
    }

    IntRect reference_grid_coordinates_for_ll_band(IntRect tile_rect, int component_index, int r, int N_L) const
    {
        // B.5
        // (B-14)
        auto component_rect = reference_grid_coordinates_for_tile_component(tile_rect, component_index);
        int denominator = 1 << (N_L - r);
        int trx0 = ceil_div(component_rect.left(), denominator);
        int try0 = ceil_div(component_rect.top(), denominator);
        int trx1 = ceil_div(component_rect.right(), denominator);
        int try1 = ceil_div(component_rect.bottom(), denominator);

        return { trx0, try0, trx1 - trx0, try1 - try0 };
    }
<<<<<<< HEAD

    IntRect reference_grid_coordinates_for_sub_band(IntRect tile_rect, int component_index, int n_b, SubBand sub_band) const
    {
        // B.5
        // Table B.1 – Quantities (xob, yob) for sub-band b
        int xob = 0;
        int yob = 0;
        if (sub_band == SubBand::HorizontalHighpassVerticalLowpass || sub_band == SubBand::HorizontalHighpassVerticalHighpass)
            xob = 1;
        if (sub_band == SubBand::HorizontalLowpassVerticalHighpass || sub_band == SubBand::HorizontalHighpassVerticalHighpass)
            yob = 1;
        VERIFY(n_b >= 1 || (n_b == 0 && sub_band == SubBand::HorizontalLowpassVerticalLowpass));
        int o_scale = 1 << (n_b - 1);

        // (B-15)
        auto component_rect = reference_grid_coordinates_for_tile_component(tile_rect, component_index);
        int denominator = 1 << n_b;
        int tbx0 = ceil_div(component_rect.left() - o_scale * xob, denominator);
        int tby0 = ceil_div(component_rect.top() - o_scale * yob, denominator);
        int tbx1 = ceil_div(component_rect.right() - o_scale * xob, denominator);
        int tby1 = ceil_div(component_rect.bottom() - o_scale * yob, denominator);

        return { tbx0, tby0, tbx1 - tbx0, tby1 - tby0 };
    }

    IntRect reference_grid_coordinates_for_sub_band(IntPoint tile_2d_index, int component_index, int n_b, SubBand sub_band) const
    {
        auto tile_rect = reference_grid_coordinates_for_tile(tile_2d_index);
        return reference_grid_coordinates_for_sub_band(tile_rect, component_index, n_b, sub_band);
    }
||||||| parent of 96a459873a9 (tweaks (reference_grid_coordinates_for_sub_band(); pass coding params and progression data to read_packet_header)
=======

    IntRect reference_grid_coordinates_for_sub_band(IntRect tile_rect, int component_index, int n_b, SubBand sub_band) const
    {
        // B.5
        // Table B.1 – Quantities (xob, yob) for sub-band b
        int xob = 0;
        int yob = 0;
        if (sub_band == SubBand::HorizontalHighpassVerticalLowpass || sub_band == SubBand::HorizontalHighpassVerticalHighpass)
            xob = 1;
        if (sub_band == SubBand::HorizontalLowpassVerticalHighpass || sub_band == SubBand::HorizontalHighpassVerticalHighpass)
            yob = 1;
        VERIFY(n_b >= 1 || (n_b == 0 && sub_band == SubBand::HorizontalLowpassVerticalLowpass));
        int o_scale = 1 << (n_b - 1);

        // (B-15)
        auto component_rect = reference_grid_coordinates_for_tile_component(tile_rect, component_index);
        int denominator = 1 << n_b;
        int tbx0 = ceil_div(component_rect.left() - o_scale * xob, denominator);
        int tby0 = ceil_div(component_rect.top() - o_scale * yob, denominator);
        int tbx1 = ceil_div(component_rect.right() - o_scale * xob, denominator);
        int tby1 = ceil_div(component_rect.bottom() - o_scale * yob, denominator);

        return { tbx0, tby0, tbx1 - tbx0, tby1 - tby0 };

    }

    IntRect reference_grid_coordinates_for_sub_band(IntPoint tile_2d_index, int component_index, int n_b, SubBand sub_band) const
    {
        auto tile_rect = reference_grid_coordinates_for_tile(tile_2d_index);
        return reference_grid_coordinates_for_sub_band(tile_rect, component_index, n_b, sub_band);
    }

>>>>>>> 96a459873a9 (tweaks (reference_grid_coordinates_for_sub_band(); pass coding params and progression data to read_packet_header)
};

static ErrorOr<ImageAndTileSize> read_image_and_tile_size(ReadonlyBytes data)
{
    FixedMemoryStream stream { data };

    ImageAndTileSize siz;
    siz.needed_decoder_capabilities = TRY(stream.read_value<BigEndian<u16>>());
    siz.width = TRY(stream.read_value<BigEndian<u32>>());
    siz.height = TRY(stream.read_value<BigEndian<u32>>());
    siz.x_offset = TRY(stream.read_value<BigEndian<u32>>());
    siz.y_offset = TRY(stream.read_value<BigEndian<u32>>());
    siz.tile_width = TRY(stream.read_value<BigEndian<u32>>());
    siz.tile_height = TRY(stream.read_value<BigEndian<u32>>());
    siz.tile_x_offset = TRY(stream.read_value<BigEndian<u32>>());
    siz.tile_y_offset = TRY(stream.read_value<BigEndian<u32>>());
    u16 component_count = TRY(stream.read_value<BigEndian<u16>>()); // "Csiz" in spec.

    // B.3 Image area division into tiles and tile-components
    // (B-3)
    if (!(0 <= siz.tile_x_offset && siz.tile_x_offset <= siz.x_offset && 0 <= siz.tile_y_offset && siz.tile_y_offset <= siz.y_offset))
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Invalid tile offset");
    // (B-4)
    if (!(siz.tile_width + siz.tile_x_offset > siz.x_offset && siz.tile_height + siz.tile_y_offset > siz.y_offset))
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Invalid tile size");

    for (size_t i = 0; i < component_count; ++i) {
        ImageAndTileSize::ComponentInformation component;
        component.depth_and_sign = TRY(stream.read_value<u8>());
        if (component.bit_depth() > 38)
            return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Invalid component depth");
        component.horizontal_separation = TRY(stream.read_value<u8>());
        component.vertical_separation = TRY(stream.read_value<u8>());
        siz.components.append(component);
    }

    // XXX reject unknown bits in needed_decoder_capabilities (can mean all kinds of exotic things like SSO in T.801)

    dbgln_if(JPEG2000_DEBUG, "JPEG2000ImageDecoderPlugin: SIZ marker segment: needed_decoder_capabilities={}, width={}, height={}, x_offset={}, y_offset={}, tile_width={}, tile_height={}, tile_x_offset={}, tile_y_offset={}", siz.needed_decoder_capabilities, siz.width, siz.height, siz.x_offset, siz.y_offset, siz.tile_width, siz.tile_height, siz.tile_x_offset, siz.tile_y_offset);
    dbgln_if(JPEG2000_DEBUG, "JPEG2000ImageDecoderPlugin: SIZ marker segment: {} components:", component_count);
    for (auto [i, component] : enumerate(siz.components))
        dbgln_if(JPEG2000_DEBUG, "JPEG2000ImageDecoderPlugin: SIZ marker segment: component[{}]: is_signed={}, bit_depth={}, horizontal_separation={}, vertical_separation={}", i, component.is_signed(), component.bit_depth(), component.horizontal_separation, component.vertical_separation);

    return siz;
}

// Data shared by COD and COC marker segments
struct CodingStyleParameters {
    // Table A.20 – Transformation for the SPcod and SPcoc parameters
    enum class Transformation {
        Irreversible_9_7_Filter = 0,
        Reversible_5_3_Filter = 1,
    };

    // Table A.15 – Coding style parameter values of the SPcod and SPcoc parameters
    // "Number of decomposition levels, N_L, Zero implies no transformation."
    u8 number_of_decomposition_levels { 0 };
    u8 code_block_width_exponent { 0 };  // "xcb" in spec; 2 already added.
    u8 code_block_height_exponent { 0 }; // "ycb" in spec; 2 already added.
    u8 code_block_style { 0 };
    Transformation transformation { Transformation::Irreversible_9_7_Filter };

    // Table A.19 – Code-block style for the SPcod and SPcoc parameters
    bool uses_selective_arithmetic_coding_bypass() const { return code_block_style & 1; }
    bool reset_context_probabilities() const { return code_block_style & 2; }
    bool uses_termination_on_each_coding_pass() const { return code_block_style & 4; }
    bool uses_vertically_causal_context() const { return code_block_style & 8; }
    bool uses_predictable_termination() const { return code_block_style & 0x10; }
    bool uses_segmentation_symbols() const { return code_block_style & 0x20; }

    // If has_explicit_precinct_size is false, this contains the default { 15, 15 } number_of_decomposition_levels + 1 times.
    // If has_explicit_precinct_size is true, this contains number_of_decomposition_levels + 1 explicit values stored in the COD marker segment.
    struct PrecinctSize {
        u8 PPx { 0 };
        u8 PPy { 0 };
    };
    Vector<PrecinctSize> precinct_sizes;
};

static ErrorOr<CodingStyleParameters> read_coding_style_parameters(ReadonlyBytes data, StringView name, bool has_explicit_precinct_size)
{
    FixedMemoryStream stream { data };

    CodingStyleParameters parameters;

    parameters.number_of_decomposition_levels = TRY(stream.read_value<u8>());
    if (parameters.number_of_decomposition_levels > 32)
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Invalid number of decomposition levels");

    // Table A.18 – Width or height exponent of the code-blocks for the SPcod and SPcoc parameters
    u8 xcb = (TRY(stream.read_value<u8>()) & 0xF) + 2;
    u8 ycb = (TRY(stream.read_value<u8>()) & 0xF) + 2;
    if (xcb > 10 || ycb > 10 || xcb + ycb > 12)
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Invalid code block size");
    parameters.code_block_width_exponent = xcb;
    parameters.code_block_height_exponent = ycb;

    parameters.code_block_style = TRY(stream.read_value<u8>());

    u8 transformation = TRY(stream.read_value<u8>());
    if (transformation > 1)
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Invalid transformation");
    parameters.transformation = static_cast<CodingStyleParameters::Transformation>(transformation);

    if (has_explicit_precinct_size) {
        for (size_t i = 0; i < parameters.number_of_decomposition_levels + 1u; ++i) {
            u8 b = TRY(stream.read_value<u8>());

            // Table A.21 – Precinct width and height for the SPcod and SPcoc parameters
            CodingStyleParameters::PrecinctSize precinct_size;
            precinct_size.PPx = b & 0xF;
            precinct_size.PPy = b >> 4;
            if ((precinct_size.PPx == 0 || precinct_size.PPy == 0) && i > 0)
                return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Invalid precinct size");
            parameters.precinct_sizes.append(precinct_size);
        }
    } else {
        for (size_t i = 0; i < parameters.number_of_decomposition_levels + 1u; ++i)
            parameters.precinct_sizes.append({ 15, 15 });
    }

    dbgln_if(JPEG2000_DEBUG, "JPEG2000ImageDecoderPlugin: {} marker segment: number_of_decomposition_levels={}, code_block_width_exponent={}, code_block_height_exponent={}", name, parameters.number_of_decomposition_levels, parameters.code_block_width_exponent, parameters.code_block_height_exponent);
    dbgln_if(JPEG2000_DEBUG, "JPEG2000ImageDecoderPlugin: {} marker segment: code_block_style={}, transformation={}", name, parameters.code_block_style, (int)parameters.transformation);
    if (has_explicit_precinct_size) {
        dbgln_if(JPEG2000_DEBUG, "JPEG2000ImageDecoderPlugin: {} marker segment: {} explicit precinct sizes:", name, parameters.precinct_sizes.size());
        for (auto [i, precinct_size] : enumerate(parameters.precinct_sizes))
            dbgln_if(JPEG2000_DEBUG, "JPEG2000ImageDecoderPlugin: {} marker segment: precinct_size[{}]: PPx={}, PPy={}", name, i, precinct_size.PPx, precinct_size.PPy);
    }

    return parameters;
}

// A.6.1 Coding style default (COD)
struct CodingStyleDefault {
    // Table A.13 – Coding style parameter values for the Scod parameter
    bool has_explicit_precinct_size { false };
    bool may_use_SOP_marker { false };
    bool may_use_EPH_marker { false };

    // Table A.16 – Progression order for the SGcod, SPcoc, and Ppoc parameters
    // B.12 Progression order
    enum ProgressionOrder {
        LayerResolutionComponentPosition = 0,
        ResolutionLayerComponentPosition = 1,
        ResolutionPositionComponentLayer = 2,
        PositionComponentResolutionLayer = 3,
        ComponentPositionResolutionLayer = 4,
    };

    // Table A.17 – Multiple component transformation for the SGcod parameters
    enum MultipleComponentTransformationType {
        None = 0,
        MultipleComponentTransformationUsed = 1, // See Annex G
    };

    // Table A.14 – Coding style parameter values of the SGcod parameter
    ProgressionOrder progression_order { LayerResolutionComponentPosition };
    u16 number_of_layers { 0 };
    MultipleComponentTransformationType multiple_component_transformation_type { None };

    CodingStyleParameters parameters;
};

static ErrorOr<CodingStyleDefault> read_coding_style_default(ReadonlyBytes data)
{
    FixedMemoryStream stream { data };

    CodingStyleDefault cod;

    u8 Scod = TRY(stream.read_value<u8>());
    cod.has_explicit_precinct_size = Scod & 1;
    cod.may_use_SOP_marker = Scod & 2;
    cod.may_use_EPH_marker = Scod & 4;

    u32 SGcod = TRY(stream.read_value<BigEndian<u32>>());
    u8 progression_order = SGcod >> 24;
    if (progression_order > 4)
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Invalid progression order");
    cod.progression_order = static_cast<CodingStyleDefault::ProgressionOrder>(progression_order);

    cod.number_of_layers = (SGcod >> 8) & 0xFFFF;
    if (cod.number_of_layers == 0)
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Invalid number of layers");

    u8 multiple_component_transformation_type = SGcod & 0xFF;
    if (multiple_component_transformation_type > 1)
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Invalid multiple component transformation type");
    cod.multiple_component_transformation_type = static_cast<CodingStyleDefault::MultipleComponentTransformationType>(multiple_component_transformation_type);

    dbgln_if(JPEG2000_DEBUG, "JPEG2000ImageDecoderPlugin: COD marker segment: has_explicit_precinct_size={}, may_use_SOP_marker={}, may_use_EPH_marker={}", cod.has_explicit_precinct_size, cod.may_use_SOP_marker, cod.may_use_EPH_marker);
    dbgln_if(JPEG2000_DEBUG, "JPEG2000ImageDecoderPlugin: COD marker segment: progression_order={}, number_of_layers={}, multiple_component_transformation_type={}", (int)cod.progression_order, cod.number_of_layers, (int)cod.multiple_component_transformation_type);

    cod.parameters = TRY(read_coding_style_parameters(data.slice(stream.offset()), "COD"sv, cod.has_explicit_precinct_size));

    return cod;
}

// A.6.2 Coding style component (COC)
struct CodingStyleComponent {
    u16 component_index { 0 }; // "Ccoc" in spec.

    // Table A.23 – Coding style parameter values for the Scoc parameter
    bool has_explicit_precinct_size { false }; // "Scoc" in spec.

    CodingStyleParameters parameters;
};

static ErrorOr<CodingStyleComponent> read_coding_style_component(ReadonlyBytes data, size_t number_of_components)
{
    FixedMemoryStream stream { data };

    // Table A.22 – Coding style component parameter values
    CodingStyleComponent coc;
    if (number_of_components < 257)
        coc.component_index = TRY(stream.read_value<u8>());
    else
        coc.component_index = TRY(stream.read_value<BigEndian<u16>>());

    u8 Scoc = TRY(stream.read_value<u8>());
    coc.has_explicit_precinct_size = Scoc & 1;

    dbgln_if(JPEG2000_DEBUG, "JPEG2000ImageDecoderPlugin: COC marker segment: component_index={}", coc.component_index);
    coc.parameters = TRY(read_coding_style_parameters(data.slice(TRY(stream.tell())), "COC"sv, coc.has_explicit_precinct_size));

    return coc;
}

// A.6.4 Quantization default (QCD)
struct QuantizationDefault {
    enum QuantizationStyle {
        NoQuantization = 0,
        ScalarDerived = 1,
        ScalarExpounded = 2,
    };
    QuantizationStyle quantization_style { NoQuantization };
    u8 number_of_guard_bits { 0 };

    struct ReversibleStepSize {
        u8 exponent { 0 };
    };
    struct IrreversibleStepSize {
        u16 mantissa { 0 };
        u8 exponent { 0 };
    };

    // Stores a Vector<ReversibleStepSize> if quantization_style is NoQuantization, and a Vector<IrreversibleStepSize> otherwise.
    // The size of the vector is >= 3*number_of_decomposition_levels + 1 if quantization_style is not ScalarDerived, and 1 otherwise.
    using StepSizeType = Variant<Empty, Vector<ReversibleStepSize>, Vector<IrreversibleStepSize>>;
    StepSizeType step_sizes;
};

static ErrorOr<QuantizationDefault> read_quantization_default(ReadonlyBytes data, StringView marker_name = "QCD"sv)
{
    FixedMemoryStream stream { data };

    QuantizationDefault qcd;

    u8 sqcd = TRY(stream.read_value<u8>());
    u8 quantization_style = sqcd & 0x1F;
    if (quantization_style > 2)
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Invalid quantization style");
    qcd.quantization_style = static_cast<QuantizationDefault::QuantizationStyle>(quantization_style);
    qcd.number_of_guard_bits = sqcd >> 5;

    qcd.step_sizes = TRY([&]() -> ErrorOr<QuantizationDefault::StepSizeType> {
        if (quantization_style == QuantizationDefault::NoQuantization) {
            // Table A.29 – Reversible step size values for the SPqcd and SPqcc parameters (reversible transform only)
            if (data.size() < 2)
                return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Not enough data for QCD marker segment");
            u8 number_of_decomposition_levels = (data.size() - 2) / 3;

            Vector<QuantizationDefault::ReversibleStepSize> reversible_step_sizes;
            for (size_t i = 0; i < 1u + 3u * number_of_decomposition_levels; ++i)
                reversible_step_sizes.append({ static_cast<u8>(TRY(stream.read_value<u8>()) >> 3) });
            return reversible_step_sizes;
        }

        // Table A.30 – Quantization values for the SPqcd and SPqcc parameters (irreversible transformation only)
        if (data.size() < 3)
            return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Not enough data for QCD marker segment");
        u8 number_of_decomposition_levels = 0;
        if (quantization_style == QuantizationDefault::ScalarExpounded)
            number_of_decomposition_levels = (data.size() - 3) / 6;

        Vector<QuantizationDefault::IrreversibleStepSize> irreversible_step_sizes;
        for (size_t i = 0; i < 1u + 3u * number_of_decomposition_levels; ++i) {
            u16 value = TRY(stream.read_value<BigEndian<u16>>());
            QuantizationDefault::IrreversibleStepSize step_size;
            step_size.mantissa = value & 0x7FF;
            step_size.exponent = value >> 11;
            irreversible_step_sizes.append(step_size);
        }
        return irreversible_step_sizes;
    }());

    dbgln_if(JPEG2000_DEBUG, "JPEG2000ImageDecoderPlugin: {} marker segment: quantization_style={}, number_of_guard_bits={}", marker_name, (int)qcd.quantization_style, qcd.number_of_guard_bits);
    qcd.step_sizes.visit(
        [](Empty) { VERIFY_NOT_REACHED(); },
        [&](Vector<QuantizationDefault::ReversibleStepSize> const& step_sizes) {
            dbgln_if(JPEG2000_DEBUG, "JPEG2000ImageDecoderPlugin: {} marker segment: {} step sizes:", marker_name, step_sizes.size());
            for (auto [i, step_size] : enumerate(step_sizes)) {
                dbgln_if(JPEG2000_DEBUG, "JPEG2000ImageDecoderPlugin: {} marker segment: step_size[{}]: exponent={}", marker_name, i, step_size.exponent);
            }
        },
        [&](Vector<QuantizationDefault::IrreversibleStepSize> const& step_sizes) {
            dbgln_if(JPEG2000_DEBUG, "JPEG2000ImageDecoderPlugin: {} marker segment: {} step sizes:", marker_name, step_sizes.size());
            for (auto [i, step_size] : enumerate(step_sizes)) {
                dbgln_if(JPEG2000_DEBUG, "JPEG2000ImageDecoderPlugin: {} marker segment: step_size[{}]: mantissa={}, exponent={}", marker_name, i, step_size.mantissa, step_size.exponent);
            }
        });

    return qcd;
}

// A.6.5 Quantization component (QCC)
struct QuantizationComponent {
    u16 component_index { 0 }; // "Cqcc" in spec.
    QuantizationDefault qcd;
};

static ErrorOr<QuantizationComponent> read_quantization_component(ReadonlyBytes data, size_t number_of_components)
{
    FixedMemoryStream stream { data };

    QuantizationComponent qcc;
    if (number_of_components < 257)
        qcc.component_index = TRY(stream.read_value<u8>());
    else
        qcc.component_index = TRY(stream.read_value<BigEndian<u16>>());

    dbgln_if(JPEG2000_DEBUG, "JPEG2000ImageDecoderPlugin: QCC marker segment: component_index={}", qcc.component_index);
    qcc.qcd = TRY(read_quantization_default(data.slice(TRY(stream.tell())), "QCC"sv));

    return qcc;
}

// A.9.2 Comment (COM)
struct Comment {
    enum CommentType {
        Binary = 0,
        ISO_IEC_8859_15 = 1,
    };
    CommentType type { Binary }; // "Rcom" in spec.
    ReadonlyBytes data;
};

static ErrorOr<Comment> read_comment(ReadonlyBytes data)
{
    FixedMemoryStream stream { data };

    Comment com;
    u16 comment_type = TRY(stream.read_value<BigEndian<u16>>());
    if (comment_type > 1)
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Invalid comment type");
    com.type = static_cast<Comment::CommentType>(comment_type);
    com.data = data.slice(TRY(stream.tell()));

    dbgln_if(JPEG2000_DEBUG, "JPEG2000ImageDecoderPlugin: COM marker segment: comment_type={}, size()={}", (int)com.type, com.data.size());
    if (com.type == Comment::ISO_IEC_8859_15)
        dbgln_if(JPEG2000_DEBUG, "JPEG2000ImageDecoderPlugin: COM marker segment, ISO/IEC 8859-15 text: '{}'", TRY(TextCodec::decoder_for("ISO-8859-15"sv)->to_utf8(StringView { com.data })));

    return com;
}

// B.12 Progression order
struct ProgressionData {
    int layer { 0 };
    int resolution_level { 0 };
    int component { 0 };
    int precinct { 0 };

    bool operator==(ProgressionData const&) const = default;
};

class ProgressionIterator {
public:
    virtual ~ProgressionIterator() = default;

    virtual bool has_next() const = 0;
    virtual ProgressionData next() = 0;
};

// B.12.1.1 Layer-resolution level-component-position progression
class LayerResolutionLevelComponentPositionProgressionIterator : public ProgressionIterator {
public:
    // XXX Supporting POC packets will probably require changes to this
    LayerResolutionLevelComponentPositionProgressionIterator(int number_of_layers, int max_number_of_decomposition_levels, int component_count, Function<int(int resolution_level, int component)> number_of_precincts);
    virtual bool has_next() const override;
    virtual ProgressionData next() override;

private:
    Function<int(int resolution_level, int component)> m_number_of_precincts;
    ProgressionData m_next {};
    ProgressionData m_end {};
};

LayerResolutionLevelComponentPositionProgressionIterator::LayerResolutionLevelComponentPositionProgressionIterator(int number_of_layers, int max_number_of_decomposition_levels, int component_count, Function<int(int resolution_level, int component)> number_of_precincts)
    : m_number_of_precincts(move(number_of_precincts))
{
    m_end.layer = number_of_layers;
    m_end.resolution_level = max_number_of_decomposition_levels + 1;
    m_end.component = component_count;
    m_end.precinct = m_number_of_precincts(m_next.resolution_level, m_next.component);
}

bool LayerResolutionLevelComponentPositionProgressionIterator::has_next() const
{
    return m_next != ProgressionData { m_end.layer, 0, 0, 0 };
}

ProgressionData LayerResolutionLevelComponentPositionProgressionIterator::next()
{
    ProgressionData current_data = m_next;

    // B.12.1.1 Layer-resolution level-component-position progression
    // "for each l = 0,..., L – 1
    //      for each r = 0,..., Nmax
    //          for each i = 0,..., Csiz – 1
    //              for each k = 0,..., numprecincts – 1
    //                  packet for component i, resolution level r, layer l, and precinct k.
    //  Here, L is the number of layers and Nmax is the maximum number of decomposition levels, N_L, used in any component of the tile."
    // FIXME: This always iterates up to Nmax, instead of just N_l of each component. That means several of the iteration results will be invalid and skipped.
    // (This is a performance issue, not a correctness issue.)

    ++m_next.precinct;
    if (m_next.precinct < m_end.precinct)
        return current_data;

    m_next.precinct = 0;
    ++m_next.component;
    if (m_next.component < m_end.component) {
        m_end.precinct = m_number_of_precincts(m_next.resolution_level, m_next.component);
        return current_data;
    }

    m_next.component = 0;
    ++m_next.resolution_level;
    if (m_next.resolution_level < m_end.resolution_level) {
        m_end.precinct = m_number_of_precincts(m_next.resolution_level, m_next.component);
        return current_data;
    }

    m_next.resolution_level = 0;
    m_end.precinct = m_number_of_precincts(m_next.resolution_level, m_next.component);

    ++m_next.layer;
    VERIFY(m_next.layer < m_end.layer || !has_next());

    return current_data;
}

// B.12.1.2 Resolution level-layer-component-position progression
class ResolutionLevelLayerComponentPositionProgressionIterator : public ProgressionIterator {
public:
    // XXX Supporting POC packets will probably require changes to this
    ResolutionLevelLayerComponentPositionProgressionIterator(int number_of_layers, int max_number_of_decomposition_levels, int component_count, Function<int(int resolution_level, int component)> number_of_precincts);
    virtual bool has_next() const override;
    virtual ProgressionData next() override;

private:
    Function<int(int resolution_level, int component)> m_number_of_precincts;
    ProgressionData m_next {};
    ProgressionData m_end {};
};

ResolutionLevelLayerComponentPositionProgressionIterator::ResolutionLevelLayerComponentPositionProgressionIterator(int number_of_layers, int max_number_of_decomposition_levels, int component_count, Function<int(int resolution_level, int component)> number_of_precincts)
    : m_number_of_precincts(move(number_of_precincts))
{
    m_end.layer = number_of_layers;
    m_end.resolution_level = max_number_of_decomposition_levels + 1;
    m_end.component = component_count;
    m_end.precinct = m_number_of_precincts(m_next.resolution_level, m_next.component);
}

bool ResolutionLevelLayerComponentPositionProgressionIterator::has_next() const
{
    return m_next != ProgressionData { 0, m_end.resolution_level, 0, 0 };
}

ProgressionData ResolutionLevelLayerComponentPositionProgressionIterator::next()
{
    ProgressionData current_data = m_next;

    // B.12.1.2 Resolution level-layer-component-position progression
    // "for each r = 0,..., Nmax
    //      for each l = 0,..., L – 1
    //          for each i = 0,..., Csiz – 1
    //              for each k = 0,..., numprecincts – 1
    //                  packet for component i, resolution level r, layer l, and precinct k."
    // FIXME: This always iterates up to Nmax, instead of just N_l of each component. That means several of the iteration results will be invalid and skipped.
    // (This is a performance issue, not a correctness issue.)

    ++m_next.precinct;
    if (m_next.precinct < m_end.precinct)
        return current_data;

    m_next.precinct = 0;
    ++m_next.component;
    if (m_next.component < m_end.component) {
        m_end.precinct = m_number_of_precincts(m_next.resolution_level, m_next.component);
        return current_data;
    }

    m_next.component = 0;
    ++m_next.layer;
    if (m_next.layer < m_end.layer) {
        m_end.precinct = m_number_of_precincts(m_next.resolution_level, m_next.component);
        return current_data;
    }

    m_next.layer = 0;

    ++m_next.resolution_level;
    if (has_next())
        m_end.precinct = m_number_of_precincts(m_next.resolution_level, m_next.component);
    VERIFY(m_next.resolution_level < m_end.resolution_level || !has_next());

    return current_data;
}


struct TilePartData {
    StartOfTilePart sot;
    Vector<Comment> coms;
    ReadonlyBytes data;
};

struct TileData {
    size_t index { 0 };
    Optional<CodingStyleDefault> cod;
    Vector<CodingStyleComponent> cocs;
    Optional<QuantizationDefault> qcd;
    Vector<QuantizationComponent> qccs;
    Vector<TilePartData> tile_parts;

    // FIXME: This will have to move and be reorganized come POC support.
    OwnPtr<ProgressionIterator> progression_iterator;

    IntRect rect;
};

// Data for a code-block. If multiple packets store bitplanes for the same code-block,
// this is the state stored for each codeblock across packets.
struct CodeBlockState {
    // B.10.7.1 Single codeword segment
    // "Lblock is a code-block state variable. [...] The value of Lblock is initially set to three."
    u32 Lblock { 3 };

    // Becomes true when the first packet including this codeblock is read.
    bool has_been_included_in_previous_packet { false };

    // One for each layer in this code-block.
    // XXX this is packet reading output data, while the struct stores packet reading input state.
    //     this should be somewhere else.
    Vector<ReadonlyBytes, 1> data;
};

// State needed to read packet headers.
struct PrecinctPacketState {
    Vector<CodeBlockState> code_block_state;
    JPEG2000::TagTree code_block_inclusion_tree;
    JPEG2000::TagTree p_tree;
};

struct DecodedCoefficients {
    IntRect rect;
    Vector<float> coefficients;

    // XXX these are used only during IDWT. Not sure they should be in this struct.
    Vector<float> scanline_buffer;
    Vector<float> scanline_buffer2;
    int scanline_start { 0 };
};

struct CodeblockBitplaneState {
    // Stores 1 bit significance and 1 bit sign for the 4 pixels in a vertical strip.
    Vector<u8> significance_and_sign;

    Vector<u16> magnitudes;

    // Stores bit index (not pass!) where the coefficient became significant.
    // XXX name sucks
    Vector<u8> became_significant_in_pass;

    // Stores the pass (not bit index!) the coefficient was coded in, even if it was coded as "not yet significant".
    // Will always be a significance propagation pass.
    // XXX use fewer vectors for all this state somehow
    Vector<u8> was_coded_in_pass;

    int current_bitplane { 0 };

    int pass { 0 };

    QMArithmeticDecoder::Context uniform_context;
    QMArithmeticDecoder::Context run_length_context;
    Array<QMArithmeticDecoder::Context, 17> all_other_contexts {};

    QMArithmeticDecoder arithmetic_decoder;

    Vector<ReadonlyBytes> layer_data;
    u32 original_p { 0 };
    int total_number_of_coding_passes { 0 };

    void reset_contexts()
    {
        // Table D.7 – Initial states for all contexts
        uniform_context = { 46, 0 };
        run_length_context = { 3, 0 };
        for (auto& context : all_other_contexts)
            context = { 0, 0 };
        all_other_contexts[0] = { 4, 0 }; // "All zero neighbours"
    }
};

// DecodedComponent has one of these for each subband at each resolution level.
struct PacketDecodingState {
    Vector<PrecinctPacketState> precinct_data;

    // indexed by precinct, then codeblock
    Vector<Vector<CodeblockBitplaneState>> codeblock_bitplane_state;
};

struct DecodedComponent {
    IntRect rect;
    DecodedCoefficients nLL; // r == 0 <=> N_L LL

    // XXX bad name maybe? each entry are 3 subbands already,
    // so subbands_bands or something? decompositions? decomposition_levels? needs self-consistent names.
    // decomposition_levels is probably closest to spec.
    // decompositions[r] <=> N_L - r HL, LH, HH
    Vector<Array<DecodedCoefficients, 3>> decompositions;

    // nLL_rects[i] <=> N_L - i + 1 LL rect
    Vector<IntRect> nLL_rects;

    ImageAndTileSize::ComponentInformation component_info;

    PacketDecodingState nLL_packet_state;
    Vector<Array<PacketDecodingState, 3>> decompositions_packet_state;

    // int component_index {};
    // IntRect tile_rect;

    // DecodedCoefficients idwt_result;
};

struct DecodedTile {
    Vector<DecodedComponent> components;
};

struct JPEG2000LoadingContext {
    enum class State {
        NotDecoded = 0,
        ImageDecoded,
        Error,
    };
    State state { State::NotDecoded };
    ReadonlyBytes codestream_data;
    size_t codestream_cursor { 0 };
    Optional<ReadonlyBytes> icc_data;

    IntSize size;

    ISOBMFF::BoxList boxes;

    // Data from marker segments:
    ImageAndTileSize siz;
    CodingStyleDefault cod;
    Vector<CodingStyleComponent> cocs;
    QuantizationDefault qcd;
    Vector<QuantizationComponent> qccs;
    Vector<Comment> coms;
    Vector<TileData> tiles;

    Vector<DecodedTile> decoded_tiles;

    RefPtr<Gfx::Bitmap> bitmap;
};

static CodingStyleParameters const& coding_style_parameters_for_component(JPEG2000LoadingContext const& context, TileData const& tile, size_t component_index)
{
    // Tile-part COC > Tile-part COD > Main COC > Main COD
    for (auto const& coc : tile.cocs) {
        if (coc.component_index == component_index)
            return coc.parameters;
    }

    if (tile.cod.has_value())
        return tile.cod->parameters;

    for (auto const& coc : context.cocs) {
        if (coc.component_index == component_index)
            return coc.parameters;
    }

    return context.cod.parameters;
}

static int number_of_decomposition_levels_for_component(JPEG2000LoadingContext const& context, TileData const& tile, size_t component_index)
{
    return coding_style_parameters_for_component(context, tile, component_index).number_of_decomposition_levels;
}

static int compute_max_number_of_decomposition_levels(JPEG2000LoadingContext const& context, TileData const& tile)
{
    int max_number_of_decomposition_levels = 0;
    for (size_t c = 0; c < context.siz.components.size(); ++c)
        max_number_of_decomposition_levels = max(max_number_of_decomposition_levels, number_of_decomposition_levels_for_component(context, tile, c));
    return max_number_of_decomposition_levels;
}

static ErrorOr<OwnPtr<ProgressionIterator>> make_progression_iterator(JPEG2000LoadingContext const& context, TileData const& tile)
{
    auto number_of_layers = tile.cod.value_or(context.cod).number_of_layers;

    auto number_of_precincts_from_resolution_level_and_component = [&](int r, int component_index) {
        auto ll_rect = context.siz.reference_grid_coordinates_for_ll_band(tile.rect, component_index, r, number_of_decomposition_levels_for_component(context, tile, component_index));

// dbgln("ll_rect: {} for tile rect {} component index {} r {}", ll_rect, tile.rect, component_index, r);

        // B.6
        // (B-16)
        int num_precincts_wide = 0;
        int num_precincts_high = 0;
        int PPx = coding_style_parameters_for_component(context, tile, component_index).precinct_sizes[r].PPx;
        int PPy = coding_style_parameters_for_component(context, tile, component_index).precinct_sizes[r].PPy;

        // XXX this needs this too :/
        // XXX or maybe it doesn't, because it's for the LL band and below is for subbands? aaarrrhhhhh
        // => probably don't need this
        // if (r > 0) {
            // PPx--;
            // PPy--;
        // }

        if (ll_rect.width() > 0)
            num_precincts_wide = ceil_div(ll_rect.right(), 1 << PPx) - (ll_rect.x() / (1 << PPx));
        if (ll_rect.height() > 0)
            num_precincts_high = ceil_div(ll_rect.bottom(), 1 << PPy) - (ll_rect.y() / (1 << PPy));
        // dbgln("num_precincts_wide: {}, num_precincts_high: {}", num_precincts_wide, num_precincts_high);
        return num_precincts_wide * num_precincts_high;
    };

    switch (tile.cod.value_or(context.cod).progression_order) {
    case CodingStyleDefault::ProgressionOrder::LayerResolutionComponentPosition:
        return make<LayerResolutionLevelComponentPositionProgressionIterator>(number_of_layers, compute_max_number_of_decomposition_levels(context, tile), context.siz.components.size(), move(number_of_precincts_from_resolution_level_and_component));
    case CodingStyleDefault::ResolutionLayerComponentPosition:
        return make<ResolutionLevelLayerComponentPositionProgressionIterator>(number_of_layers, compute_max_number_of_decomposition_levels(context, tile), context.siz.components.size(), move(number_of_precincts_from_resolution_level_and_component));
    case CodingStyleDefault::ResolutionPositionComponentLayer:
        return Error::from_string_literal("JPEG200Loader: ResolutionPositionComponentLayer progression order not yet supported");
    case CodingStyleDefault::PositionComponentResolutionLayer:
        return Error::from_string_literal("JPEG200Loader: PositionComponentResolutionLayer progression order not yet supported");
    case CodingStyleDefault::ComponentPositionResolutionLayer:
        return Error::from_string_literal("JPEG200Loader: ComponentPositionResolutionLayer progression order not yet supported");
    }
}


struct MarkerSegment {
    u16 marker;

    // OptionalNone for markers that don't have data.
    // For markers that do have data, this does not include the marker length data. (`data.size() + 2` is the value of the marker length field.)
    Optional<ReadonlyBytes> data;
};

static ErrorOr<u16> peek_marker(JPEG2000LoadingContext& context)
{
    if (context.codestream_cursor + 2 > context.codestream_data.size())
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Not enough data for marker");
    return *reinterpret_cast<BigEndian<u16> const*>(context.codestream_data.data() + context.codestream_cursor);
}

static ErrorOr<MarkerSegment> read_marker_at_cursor(JPEG2000LoadingContext& context)
{
    u16 marker = TRY(peek_marker(context));
    // "All markers with the marker code between 0xFF30 and 0xFF3F have no marker segment parameters. They shall be skipped by the decoder."
    // "The SOC, SOD and EOC are delimiting markers not marker segments, and have no explicit length information or other parameters."
    bool is_marker_segment = !(marker >= 0xFF30 && marker <= 0xFF3F) && marker != J2K_SOC && marker != J2K_SOD && marker != J2K_EOC;

    MarkerSegment marker_segment;
    marker_segment.marker = marker;

    if (is_marker_segment) {
        if (context.codestream_cursor + 4 > context.codestream_data.size())
            return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Not enough data for marker segment length");
        u16 marker_length = *reinterpret_cast<BigEndian<u16> const*>(context.codestream_data.data() + context.codestream_cursor + 2);
        if (marker_length < 2)
            return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Marker segment length too small");
        if (context.codestream_cursor + 2 + marker_length > context.codestream_data.size())
            return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Not enough data for marker segment data");
        marker_segment.data = ReadonlyBytes { context.codestream_data.data() + context.codestream_cursor + 4, marker_length - 2u };
    }

    context.codestream_cursor += 2;
    if (is_marker_segment)
        context.codestream_cursor += 2 + marker_segment.data->size();

    return marker_segment;
}

static ErrorOr<void> parse_codestream_main_header(JPEG2000LoadingContext& context)
{
    // Figure A.3 – Construction of the main header
    // "Required as the first marker"
    auto marker = TRY(read_marker_at_cursor(context));
    if (marker.marker != J2K_SOC)
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Expected SOC marker");

    // "Required as the second marker segment"
    marker = TRY(read_marker_at_cursor(context));
    if (marker.marker != J2K_SIZ)
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Expected SIZ marker");
    context.siz = TRY(read_image_and_tile_size(marker.data.value()));

    context.tiles.resize(context.siz.number_of_x_tiles() * context.siz.number_of_y_tiles());
    for (size_t i = 0; i < context.tiles.size(); ++i)
        context.tiles[i].index = i;

    context.decoded_tiles.resize(context.siz.number_of_x_tiles() * context.siz.number_of_y_tiles());

    bool saw_COD_marker = false;
    bool saw_QCD_marker = false;
    while (true) {
        u16 marker = TRY(peek_marker(context));
        switch (marker) {
        case J2K_COD:
        case J2K_COC:
        case J2K_QCD:
        case J2K_QCC:
        case J2K_RGN:
        case J2K_POC:
        case J2K_PPM:
        case J2K_TLM:
        case J2K_PLM:
        case J2K_CRG:
        case J2K_COM: {
            auto marker = TRY(read_marker_at_cursor(context));
            if (marker.marker == J2K_COD) {
                if (saw_COD_marker)
                    return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Multiple COD markers in main header");
                context.cod = TRY(read_coding_style_default(marker.data.value()));
                saw_COD_marker = true;
            } else if (marker.marker == J2K_COC) {
                context.cocs.append(TRY(read_coding_style_component(marker.data.value(), context.siz.components.size())));
            } else if (marker.marker == J2K_QCD) {
                if (saw_QCD_marker)
                    return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Multiple QCD markers in main header");
                context.qcd = TRY(read_quantization_default(marker.data.value()));
                saw_QCD_marker = true;
            } else if (marker.marker == J2K_QCC) {
                context.qccs.append(TRY(read_quantization_component(marker.data.value(), context.siz.components.size())));
            } else if (marker.marker == J2K_COM) {
                context.coms.append(TRY(read_comment(marker.data.value())));
            } else {
                // FIXME: These are valid main header markers. Parse contents.
                dbgln("JPEG2000ImageDecoderPlugin: marker {:#04x} not yet implemented", marker.marker);
                return Error::from_string_literal("JPEG2000ImageDecoderPlugin: marker not yet implemented");
            }
            break;
        }
        case J2K_SOT: {
            // SOT terminates the main header.
            // A.4.2: "There shall be at least one SOT in a codestream."
            if (!saw_COD_marker)
                return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Required COD marker not present in main header");
            if (!saw_QCD_marker)
                return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Required QCD marker not present in main header");

            // A.6.4: "there is not necessarily a correspondence with the number of sub-bands present because the sub-bands
            //         can be truncated with no requirement to correct [the QCD] marker segment."
            size_t step_sizes_count = context.qcd.step_sizes.visit(
                [](Empty) -> size_t { VERIFY_NOT_REACHED(); },
                [](Vector<QuantizationDefault::ReversibleStepSize> const& step_sizes) { return step_sizes.size(); },
                [](Vector<QuantizationDefault::IrreversibleStepSize> const& step_sizes) { return step_sizes.size(); });
            // FIXME: What if number_of_decomposition_levels is in context.cocs and varies by component?
            if (context.qcd.quantization_style != QuantizationDefault::ScalarDerived && step_sizes_count < context.cod.parameters.number_of_decomposition_levels * 3u + 1u)
                return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Not enough step sizes for number of decomposition levels");

            return {};
        }
        default:
            return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Unexpected marker in main header");
        }
    }
}

static ErrorOr<void> parse_codestream_tile_header(JPEG2000LoadingContext& context)
{
    // Figure A.4 – Construction of the first tile-part header of a given tile
    // Figure A.5 – Construction of a non-first tile-part header

    // "Required as the first marker segment of every tile-part header"
    auto tile_start = context.codestream_cursor;
    auto marker = TRY(read_marker_at_cursor(context));
    if (marker.marker != J2K_SOT)
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Expected SOT marker");
    auto start_of_tile = TRY(read_start_of_tile_part(marker.data.value()));
    // FIXME: Store start_of_tile on context somewhere.

    // context.tiles.resize(max(context.tiles.size(), (size_t)start_of_tile.tile_index + 1));
    // context.decoded_tiles.resize(max(context.decoded_tiles.size(), (size_t)start_of_tile.tile_index + 1));

    auto& tile = context.tiles[start_of_tile.tile_index];
    if (tile.index != start_of_tile.tile_index)
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Tile index out of order");

    if (tile.tile_parts.size() != start_of_tile.tile_part_index)
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Tile part index out of order");
    tile.tile_parts.append({});
    auto& tile_part = tile.tile_parts.last();
    tile_part.sot = start_of_tile;

    if (start_of_tile.tile_part_index == 0) {
        auto pq = context.siz.tile_2d_index_from_1d_index(tile.index);
        tile.rect = context.siz.reference_grid_coordinates_for_tile(pq);
dbgln("tile {} .rect: {}", tile.index, tile.rect);
        tile.progression_iterator = TRY(make_progression_iterator(context, tile));
    }

    bool found_start_of_data = false;
    while (!found_start_of_data) {
        u16 marker = TRY(peek_marker(context));
        switch (marker) {
        case J2K_SOD:
            // "Required as the last marker segment of every tile-part header"
            context.codestream_cursor += 2;
            found_start_of_data = true;
            break;
        case J2K_COD:
        case J2K_COC:
        case J2K_QCD:
        case J2K_QCC:
        case J2K_RGN:
            if (start_of_tile.tile_part_index != 0)
                return Error::from_string_literal("JPEG2000ImageDecoderPlugin: COD, COC, QCD, QCC, RGN markers are only valid in the first tile-part header");
            [[fallthrough]];
        case J2K_POC:
        case J2K_PPT:
        case J2K_PLT:
        case J2K_COM: {
            auto marker = TRY(read_marker_at_cursor(context));
            if (marker.marker == J2K_COD) {
                if (tile.cod.has_value())
                    return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Multiple COD markers in tile header");
                tile.cod = TRY(read_coding_style_default(marker.data.value()));
            } else if (marker.marker == J2K_COC) {
                tile.cocs.append(TRY(read_coding_style_component(marker.data.value(), context.siz.components.size())));
            } else if (marker.marker == J2K_QCD) {
                if (tile.qcd.has_value())
                    return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Multiple QCD markers in tile header");
                tile.qcd = TRY(read_quantization_default(marker.data.value()));
            } else if (marker.marker == J2K_QCC) {
                tile.qccs.append(TRY(read_quantization_component(marker.data.value(), context.siz.components.size())));
            } else if (marker.marker == J2K_COM) {
                tile_part.coms.append(TRY(read_comment(marker.data.value())));
            } else {
                // FIXME: These are valid main header markers. Parse contents.
                dbgln("JPEG2000ImageDecoderPlugin: marker {:#04x} not yet implemented in tile header", marker.marker);
                return Error::from_string_literal("JPEG2000ImageDecoderPlugin: marker not yet implemented in tile header");
            }
            break;
        }
        default:
            return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Unexpected marker in tile header");
        }
    }

    u32 tile_bitstream_length;
    if (start_of_tile.tile_part_length == 0) {
        // "If the Psot is 0, this tile-part is assumed to contain all data until the EOC marker."
        // Leave room for EOC marker.
        if (context.codestream_data.size() - context.codestream_cursor < 2)
            return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Not enough data for EOC marker");
        tile_bitstream_length = context.codestream_data.size() - context.codestream_cursor - 2;
    } else {
        u32 tile_header_length = context.codestream_cursor - tile_start;
        if (start_of_tile.tile_part_length < tile_header_length)
            return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Invalid tile part length");
        tile_bitstream_length = start_of_tile.tile_part_length - tile_header_length;
    }

    if (context.codestream_cursor + tile_bitstream_length > context.codestream_data.size())
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Not enough data for tile bitstream");
    tile_part.data = context.codestream_data.slice(context.codestream_cursor, tile_bitstream_length);

    context.codestream_cursor += tile_bitstream_length;
    dbgln_if(JPEG2000_DEBUG, "JPEG2000ImageDecoderPlugin: Tile bitstream length: {}", tile_bitstream_length);

    return {};
}

static ErrorOr<void> parse_codestream_tile_headers(JPEG2000LoadingContext& context)
{
    while (true) {
        auto marker = TRY(peek_marker(context));
        if (marker == J2K_EOC) {
            context.codestream_cursor += 2;
            break;
        }
        TRY(parse_codestream_tile_header(context));
    }

    if (context.codestream_cursor < context.codestream_data.size())
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Unexpected data after EOC marker");
    return {};
}

static ErrorOr<void> decode_jpeg2000_header(JPEG2000LoadingContext& context, ReadonlyBytes data)
{
    if (!JPEG2000ImageDecoderPlugin::sniff(data))
        return Error::from_string_literal("JPEG2000LoadingContext: Invalid JPEG2000 header");

    if (data.starts_with(marker_id_string)) {
        context.codestream_data = data;
        TRY(parse_codestream_main_header(context));
        context.size = { context.siz.width, context.siz.height };
        return {};
    }

    auto reader = TRY(Gfx::ISOBMFF::Reader::create(TRY(try_make<FixedMemoryStream>(data))));
    context.boxes = TRY(reader.read_entire_file());

    // I.2.2 File organization
    // "A particular order of those boxes in the file is not generally implied. However, the JPEG 2000 Signature box
    //  shall be the first box in a JP2 file, the File Type box shall immediately follow the JPEG 2000 Signature box
    //  and the JP2 Header box shall fall before the Contiguous Codestream box."
    if (context.boxes.size() < 4)
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Expected at least four boxes");

    // Required toplevel boxes: signature box, file type box, jp2 header box, contiguous codestream box.

    if (context.boxes[0]->box_type() != ISOBMFF::BoxType::JPEG2000SignatureBox)
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Expected JPEG2000SignatureBox as first box");
    if (context.boxes[1]->box_type() != ISOBMFF::BoxType::FileTypeBox)
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Expected FileTypeBox as second box");

    Optional<size_t> jp2_header_box_index;
    Optional<size_t> contiguous_codestream_box_index;
    for (size_t i = 2; i < context.boxes.size(); ++i) {
        if (context.boxes[i]->box_type() == ISOBMFF::BoxType::JPEG2000HeaderBox) {
            // "Within a JP2 file, there shall be one and only one JP2 Header box."
            if (jp2_header_box_index.has_value())
                return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Multiple JP2 Header boxes");
            jp2_header_box_index = i;
        }
        if (context.boxes[i]->box_type() == ISOBMFF::BoxType::JPEG2000ContiguousCodestreamBox && !contiguous_codestream_box_index.has_value()) {
            // "a conforming reader shall ignore all codestreams after the first codestream found in the file.
            //  Contiguous Codestream boxes may be found anywhere in the file except before the JP2 Header box."
            contiguous_codestream_box_index = i;
            if (!jp2_header_box_index.has_value() || contiguous_codestream_box_index.value() < jp2_header_box_index.value())
                return Error::from_string_literal("JPEG2000ImageDecoderPlugin: JP2 Header box must come before Contiguous Codestream box");
        }
    }

    if (!jp2_header_box_index.has_value())
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Expected JP2 Header box");
    if (!contiguous_codestream_box_index.has_value())
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Expected Contiguous Codestream box");

    // FIXME: JPEG2000ContiguousCodestreamBox makes a copy of the codestream data. That's too heavy for header scanning.
    // Add a mode to ISOBMFF::Reader where it only stores offsets for the codestream data and the ICC profile.
    auto const& codestream_box = static_cast<ISOBMFF::JPEG2000ContiguousCodestreamBox const&>(*context.boxes[contiguous_codestream_box_index.value()]);
    context.codestream_data = codestream_box.codestream.bytes();

    // Required child boxes of the jp2 header box: image header box, color box.

    Optional<size_t> image_header_box_index;
    Optional<size_t> color_header_box_index;
    auto const& header_box = static_cast<ISOBMFF::JPEG2000HeaderBox const&>(*context.boxes[jp2_header_box_index.value()]);
    for (size_t i = 0; i < header_box.child_boxes().size(); ++i) {
        auto const& subbox = header_box.child_boxes()[i];
        if (subbox->box_type() == ISOBMFF::BoxType::JPEG2000ImageHeaderBox) {
            if (image_header_box_index.has_value())
                return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Multiple Image Header boxes");
            image_header_box_index = i;
        }
        if (subbox->box_type() == ISOBMFF::BoxType::JPEG2000ColorSpecificationBox) {
            // T.800 says there should be just one 'colr' box, but T.801 allows several and says to pick the one with highest precedence.
            bool use_this_color_box;
            if (!color_header_box_index.has_value()) {
                use_this_color_box = true;
            } else {
                auto const& new_header_box = static_cast<ISOBMFF::JPEG2000ColorSpecificationBox const&>(*header_box.child_boxes()[i]);
                auto const& current_color_box = static_cast<ISOBMFF::JPEG2000ColorSpecificationBox const&>(*header_box.child_boxes()[color_header_box_index.value()]);
                use_this_color_box = new_header_box.precedence > current_color_box.precedence;
            }

            if (use_this_color_box)
                color_header_box_index = i;
        }
    }

    if (!image_header_box_index.has_value())
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Expected Image Header box");
    if (!color_header_box_index.has_value())
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Expected Color Specification box");

    auto const& image_header_box = static_cast<ISOBMFF::JPEG2000ImageHeaderBox const&>(*header_box.child_boxes()[image_header_box_index.value()]);
    context.size = { image_header_box.width, image_header_box.height };

    auto const& color_header_box = static_cast<ISOBMFF::JPEG2000ColorSpecificationBox const&>(*header_box.child_boxes()[color_header_box_index.value()]);
    if (color_header_box.method == 2 || color_header_box.method == 3)
        context.icc_data = color_header_box.icc_data.bytes();

    TRY(parse_codestream_main_header(context));
    auto size_from_siz = IntSize { context.siz.width, context.siz.height };
    if (size_from_siz != context.size) {
        // FIXME: If this is common, warn and use size from SIZ marker.
        dbgln("JPEG2000ImageDecoderPlugin: Image size from SIZ marker ({}) does not match image size from JP2 header ({})", size_from_siz, context.size);
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Image size from SIZ marker does not match image size from JP2 header");
    }

    return {};
}

namespace JPEG2000 {

// Tag trees are used to store the code-block inclusion bits and the zero bit-plane information.
// B.10.2 Tag trees
// "At every node of this tree the minimum integer of the (up to four) nodes below it is recorded. [...]
//  Level 0 is the lowest level of the tag tree; it contains the top node. [...]
//  Each node has a [...] current value, [...] initialized to zero. A 0 bit in the tag tree means that the minimum
//  (or the value in the case of the highest level) is larger than the current value and a 1 bit means that the minimum
//  (or the value in the case of the highest level) is equal to the current value.
//  For each contiguous 0 bit in the tag tree the current value is incremented by one.
//  Nodes at higher levels cannot be coded until lower level node values are fixed (i.e, a 1 bit is coded). [...]
//  Only the information needed for the current code-block is stored at the current point in the packet header."
// The example in Figure B.13 / Table B.5 is useful to understand what exactly "only the information needed" means.
struct TagTreeNode {
    u32 value { 0 };
    enum State {
        Pending,
        Final,
    };
    State state { Pending };
    Array<OwnPtr<TagTreeNode>, 4> children {};
    u32 level { 0 }; // 0 for leaf nodes, 1 for the next level, etc.

    bool is_leaf() const { return level == 0; }

    ErrorOr<u32> read_value(u32 x, u32 y, Function<ErrorOr<bool>()> const& read_bit, u32 start_value, Optional<u32> stop_at = {})
    {
        value = max(value, start_value);
        while (true) {
            if (stop_at.has_value() && value == stop_at.value())
                return value;

            if (state == Final) {
                if (is_leaf())
                    return value;
                u32 x_index = (x >> (level - 1)) & 1;
                u32 y_index = (y >> (level - 1)) & 1;
                return children[y_index * 2 + x_index]->read_value(x, y, read_bit, value, stop_at);
            }

            bool bit = TRY(read_bit());
// dbgln("read_value: x: {}, y: {}, bit: {}, value: {}", x, y, bit, value);
            if (!bit)
                value++;
            else
                state = Final;
        }
    }

    static ErrorOr<NonnullOwnPtr<TagTreeNode>> create(u32 x_count, u32 y_count, u32 level)
    {
        VERIFY(x_count > 0);
        VERIFY(y_count > 0);

        auto node = TRY(try_make<TagTreeNode>());
        node->level = level;
        if (node->is_leaf()) {
            VERIFY(x_count == 1);
            VERIFY(y_count == 1);
            return node;
        }

        u32 top_left_x_child_count = min(x_count, 1u << (max(level, 1) - 1));
        u32 top_left_y_child_count = min(y_count, 1u << (max(level, 1) - 1));
        for (u32 y = 0; y < 2; ++y) {
            for (u32 x = 0; x < 2; ++x) {
                u32 child_x_count = x == 1 ? x_count - top_left_x_child_count : top_left_x_child_count;
                u32 child_y_count = y == 1 ? y_count - top_left_y_child_count : top_left_y_child_count;
                if (child_x_count == 0 || child_y_count == 0)
                    continue;
                node->children[y * 2 + x] = TRY(create(child_x_count, child_y_count, level - 1));
            }
        }
        return node;
    }
};

TagTree::TagTree(NonnullOwnPtr<TagTreeNode> root)
    : m_root(move(root))
{
}

TagTree::TagTree() = default;
TagTree::TagTree(TagTree&&) = default;
TagTree::~TagTree() = default;
TagTree& TagTree::operator=(TagTree&& other) = default;

ErrorOr<TagTree> TagTree::create(u32 x_count, u32 y_count)
{
    auto level = ceil(log2(max(x_count, y_count)));
    return TagTree { TRY(TagTreeNode::create(x_count, y_count, level)) };
}

ErrorOr<u32> TagTree::read_value(u32 x, u32 y, Function<ErrorOr<bool>()> const& read_bit, Optional<u32> stop_at) const
{
    return m_root->read_value(x, y, read_bit, m_root->value, stop_at);
}

}

struct PacketContext {
    // precinct size
    // code-block size
    // precinct rect
    // code--block counts
    // sub-band rects
    // ProgressionData
    // CodingParameters
    int xcb_prime { 0 };
    int ycb_prime { 0 };
    IntRect precinct_rect;

    int num_precincts { 0 };
};

// XXX roles:
// * during packet decoding, store per-packet state like is included, number of coding passes, length of data, pointer to data, (p)
// * during code-block decoding, store per-code-block state like was-included-previously, block, pointer to coefficients
// => probably need to split this?
//    ...and keep it possible to read all packet headers first, and defer decode completely until after that
//    But also, Lblock etc means that packets for the same precinct/subband/component must have their headers read in order

// Data stored in packet headers. Stores data that's different across packets for the same codeblock.
struct CodeBlockPacketData {
    SubBand sub_band { SubBand::HorizontalLowpassVerticalLowpass };

    bool is_included { false };
    bool is_included_for_the_first_time { false };

    // XXX comment
    // number of zero bit planes; only stored the very first time a code-block is included
    u32 p { 0 };

    u8 number_of_coding_passes { 0 };

    u32 length_of_data { 0 };

    IntRect rect; // clipped to subband rect

    ReadonlyBytes data;
};

struct PacketSubBandData {
    Vector<CodeBlockPacketData> code_blocks;

    int codeblock_x_count { 0 };
    int codeblock_y_count { 0 };

    IntRect subband_rect;
};

struct PacketHeader {
    bool is_empty { false };

    // Only the first element is valid for r == 0 where we have LL. Else it's HL, LH, HH.
    Array<PacketSubBandData, 3> sub_bands;
};

IntRect aligned_enclosing_rect(IntRect outer_rect, IntRect inner_rect, int width_increment, int height_increment)
{
    int new_x = (inner_rect.x() / width_increment) * width_increment;
    int new_y = (inner_rect.y() / height_increment) * height_increment;
    int new_right = ceil_div(inner_rect.right(), width_increment) * width_increment;
    int new_bottom = ceil_div(inner_rect.bottom(), height_increment) * height_increment;
    return IntRect::intersection(outer_rect, IntRect::from_two_points({ new_x, new_y }, { new_right, new_bottom }));
}

static ErrorOr<PacketDecodingState*> get_or_create_code_block_packet_data(
    JPEG2000LoadingContext& context, TileData const& tile, ProgressionData const& progression_data, SubBand sub_band, int num_precincts, int code_block_x_count, int code_block_y_count);

ErrorOr<PacketHeader> read_packet_header(JPEG2000LoadingContext& context, Stream& stream, PacketContext const& packet_context, TileData const& tile, CodingStyleParameters const& coding_parameters, ProgressionData const& data)
{
    BigEndianInputBitStream bitstream { MaybeOwned { stream } };

    int N_L = coding_parameters.number_of_decomposition_levels;
    int r = data.resolution_level;
    u32 current_layer_index = data.layer;

    // B.9 Packets
    // "All compressed image data representing a specific tile, layer, component, resolution level and precinct appears in the
    //  codestream in a contiguous segment called a packet. Packet data is aligned at 8-bit (one byte) boundaries."

    // XXX repeat image, tile, component <=wavelet=> subband, precinct, code-block, packet, layer?
    // (Described in 5.3 Coding principles)

    PacketHeader header;
    // B.10 Packet header information coding
    // "The packets have headers with the following information:
    // - zero length packet;
    // - code-block inclusion;
    // - zero bit-plane information;
    // - number of coding passes;
    // - length of the code-block compressed image data from a given code-block."

    // The most useful section is B.10.8 Order of information within packet header,
    // which has an example packet header bitstream, and the data layout:
    // "bit for zero or non-zero length packet
    //  for each sub-band (LL or HL, LH and HH)
    //      for all code-blocks in this sub-band confined to the relevant precinct, in raster order
    //          code-block inclusion bits (if not previously included then tag tree, else one bit)
    //          if code-block included
    //              if first instance of code-block
    //                  zero bit-planes information
    //              number of coding passes included
    //              increase of code-block length indicator (Lblock)
    //              for each codeword segment
    //                  length of codeword segment"
    // We try to decode only the first packet header of only the LL subband for now. That covers several code-blocks still, though.
    // Let's just do the first code-block for now too. Decoding the tag trees requires knowing how many codeblocks are in this packet still,
    // to know the depth of the tag trees.

    // B.10.1 Bit-stuffing routine
    // "If the value of the byte is 0xFF, the next byte includes an extra zero bit stuffed into the MSB. Once all bits of the
    //  packet header have been assembled, the last byte is packed to the byte boundary and emitted."
    u8 last_full_byte { 0 };
    Function<ErrorOr<bool>()> read_bit = [&bitstream, &last_full_byte]() -> ErrorOr<bool> {
        if (bitstream.is_aligned_to_byte_boundary()) {
            if (last_full_byte == 0xFF) {
// dbgln("reading stuff bit");
               bool stuff_bit = TRY(bitstream.read_bit());
               if (stuff_bit)
                   return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Invalid bit-stuffing");
            }
            last_full_byte = 0;
        }
        bool bit = TRY(bitstream.read_bit());
//dbgln("reading bit {}", bit);
        last_full_byte = (last_full_byte << 1) | bit;
        return bit;
    };

    // Tag trees are used to store the code-block inclusion bits and the zero bit-plane information.
    // B.10.2 Tag trees
    // "At every node of this tree the minimum integer of the (up to four) nodes below it is recorded. [...]
    //  Level 0 is the lowest level of the tag tree; it contains the top node. [...]
    //  Each node has a [...] current value, [...] initialized to zero. A 0 bit in the tag tree means that the minimum
    //  (or the value in the case of the highest level) is larger than the current value and a 1 bit means that the minimum
    //  (or the value in the case of the highest level) is equal to the current value.
    //  For each contiguous 0 bit in the tag tree the current value is incremented by one.
    //  Nodes at higher levels cannot be coded until lower level node values are fixed (i.e, a 1 bit is coded). [...]
    //  Only the information needed for the current code-block is stored at the current point in the packet header."
    // The example in Figure B.13 / Table B.5 is useful to understand what exactly "only the information needed" means.

    // "bit for zero or non-zero length packet"
    // B.10.3 Zero length packet
    // "The first bit in the packet header denotes whether the packet has a length of zero (empty packet). The value 0 indicates a
    //  zero length; no code-blocks are included in this case. The value 1 indicates a non-zero length; this case is considered
    //  exclusively hereinafter."
    bool is_non_zero = TRY(read_bit());
    header.is_empty = !is_non_zero;
    if (header.is_empty) {
        dbgln_if(JPEG2000_DEBUG, "empty packet");
        return header;
    }

    // " for each sub-band (LL or HL, LH and HH)"
    static constexpr Array level_0_sub_bands { SubBand::HorizontalLowpassVerticalLowpass };
    static constexpr Array other_sub_bands { SubBand::HorizontalHighpassVerticalLowpass, SubBand::HorizontalLowpassVerticalHighpass, SubBand::HorizontalHighpassVerticalHighpass };
    auto sub_bands = r == 0 ? level_0_sub_bands.span() : other_sub_bands.span();

    for (auto [sub_band_index, sub_band] : enumerate(sub_bands)) {
        // dbgln("reading header info for sub-band {}", (int)sub_band);

        // B.9: "Only those code-blocks that contain samples from the relevant sub-band, confined to the precinct, have any representation in the packet."
        // Could loop over all codeblocks in precinct and skip those that don't intersect with the subband rect.
        // For now, does math instead, but probably want to do the stupid and simple thing before upstreaming.

        // Table F.1 – Decomposition level nb for sub-band b
        // XXX: Spec suggests that this ends with n_b = 1, but if N_L is 0, we have 0LL and nothing else.
        int n_b = r == 0 ? N_L : (N_L + 1 - r);
        auto rect = context.siz.reference_grid_coordinates_for_sub_band(tile.rect, data.component, n_b, sub_band);

        header.sub_bands[sub_band_index].subband_rect = rect;

        auto rect_covered_by_codeblocks = aligned_enclosing_rect(packet_context.precinct_rect, rect, 1 << packet_context.xcb_prime, 1 << packet_context.ycb_prime);

dbgln("n_b: {}", n_b);
dbgln("sub-band rect: {}", rect);
dbgln("precinct rect: {}", packet_context.precinct_rect); // XXX grok clips this to the tile rect; we only clip the codeblock rect further down
dbgln("rect covered by codeblocks: {}", rect_covered_by_codeblocks);

        auto codeblock_x_count = rect_covered_by_codeblocks.width() / (1 << packet_context.xcb_prime);
        auto codeblock_y_count = rect_covered_by_codeblocks.height() / (1 << packet_context.ycb_prime);

        header.sub_bands[sub_band_index].codeblock_x_count = codeblock_x_count;
        header.sub_bands[sub_band_index].codeblock_y_count = codeblock_y_count;
    dbgln("code-blocks per precinct: {}x{}", codeblock_x_count, codeblock_y_count);

        if (codeblock_x_count == 0 || codeblock_y_count == 0)
            continue;

        header.sub_bands[sub_band_index].code_blocks.resize(codeblock_x_count * codeblock_y_count);
        // auto& current_block = header.sub_bands[sub_band_index].code_blocks[0];

        auto& coefficients = *TRY(get_or_create_code_block_packet_data(context, tile, data, sub_band, packet_context.num_precincts, codeblock_x_count, codeblock_y_count));

        auto& code_block_inclusion_tree = coefficients.precinct_data[data.precinct].code_block_inclusion_tree;
        auto& p_tree = coefficients.precinct_data[data.precinct].p_tree;

        for (auto [code_block_index, current_block] : enumerate(header.sub_bands[sub_band_index].code_blocks)) {

            // auto& current_block_state = current_block.state;
            auto& current_block_state = coefficients.precinct_data[data.precinct].code_block_state[code_block_index];

            size_t code_block_x = code_block_index % codeblock_x_count;
            size_t code_block_y = code_block_index / codeblock_x_count;

            auto code_block_rect = IntRect { { rect_covered_by_codeblocks.x() + code_block_x * (1 << packet_context.xcb_prime), rect_covered_by_codeblocks.y() + code_block_y * (1 << packet_context.ycb_prime) }, { 1 << packet_context.xcb_prime, 1 << packet_context.ycb_prime } };
            current_block.rect = code_block_rect.intersected(rect);
dbgln("code-block rect: {}", current_block.rect);

            // B.10.4 Code-block inclusion
            bool is_included;
            if (current_block_state.has_been_included_in_previous_packet) {
                // "For code-blocks that have been included in a previous packet, a single bit is used to represent the information, where
                //  a 1 means that the code-block is included in this layer and a 0 means that it is not."
                is_included = TRY(read_bit());
            } else {
                // "For code-blocks that have not been previously included in any packet, this information is signalled with a separate tag
                //  tree code for each precinct as confined to a sub-band. The values in this tag tree are the number of the layer in which the
                //  current code-block is first included."
                is_included = TRY(code_block_inclusion_tree.read_value(code_block_x, code_block_y, read_bit, current_layer_index + 1)) <= current_layer_index;
            }
            dbgln_if(JPEG2000_DEBUG, "code-block inclusion: {}", is_included);
            current_block.is_included = is_included;

            if (!is_included)
                continue;

            // B.10.5 Zero bit-plane information
            // "If a code-block is included for the first time,
            //  [...] the number of actual bit-planes for which coding passes are generated is Mb – P
            //  [...] these missing bit-planes are all taken to be zero
            //  [...] The value of P is coded in the packet header with a separate tag tree for every precinct"
            // And Annex E, E.1 Inverse quantization procedure:
            // "Mb = G + exp_b - 1       (E-2)
            //  where the number of guard bits G and the exponent exp_b are specified in the QCD or QCC marker segments (see A.6.4 and A.6.5)."
            current_block.is_included_for_the_first_time = is_included && !current_block_state.has_been_included_in_previous_packet;
            if (current_block.is_included_for_the_first_time) {
                u32 p = TRY(p_tree.read_value(code_block_x, code_block_y, read_bit));
                dbgln("zero bit-plane information: {}", p);
                current_block.p = p;
                current_block_state.has_been_included_in_previous_packet = true;
                current_block.sub_band = sub_band;
            }

            // B.10.6 Number of coding passes
            // Table B.4 – Codewords for the number of coding passes for each code-block
            u8 number_of_coding_passes = TRY([&]() -> ErrorOr<u8> {
                if (!TRY(read_bit()))
                    return 1;
                if (!TRY(read_bit()))
                    return 2;

                u8 bits = TRY(read_bit());
                bits = (bits << 1) | TRY(read_bit());
                if (bits != 3)
                    return 3 + bits;

                bits = TRY(read_bit());
                bits = (bits << 1) | TRY(read_bit());
                bits = (bits << 1) | TRY(read_bit());
                bits = (bits << 1) | TRY(read_bit());
                bits = (bits << 1) | TRY(read_bit());
                if (bits != 31)
                    return 6 + bits;

                bits = TRY(read_bit());
                bits = (bits << 1) | TRY(read_bit());
                bits = (bits << 1) | TRY(read_bit());
                bits = (bits << 1) | TRY(read_bit());
                bits = (bits << 1) | TRY(read_bit());
                bits = (bits << 1) | TRY(read_bit());
                bits = (bits << 1) | TRY(read_bit());
                return 37 + bits;
            }());
            dbgln("number of coding passes: {} ({} bitplanes)", number_of_coding_passes, (number_of_coding_passes - 1) / 3 + 1);
            current_block.number_of_coding_passes = number_of_coding_passes;

            // B.10.7 Length of the compressed image data from a given code-block
            // XXX unclear to me to decide if we're dealing with B.10.7.1 or B.10.7.2.
            // FIXME: Figure out. For now, always B.10.7.1, but that won't always be right.

            // B.10.7.1 Single codeword segment
            // "A codeword segment is the number of bytes contributed to a packet by a code-block.
            //  The length of a codeword segment is represented by a binary number of length:
            //      bits = Lblock + ⌊log2(number_of_coding_passes)⌋
            //  where Lblock is a code-block state variable. A separate Lblock is used for each code-block in the precinct.
            //  The value of Lblock is initially set to three. The number of bytes contributed by each code-block is preceded by signalling
            //  bits that increase the value of Lblock, as needed. A signalling bit of zero indicates the current value of Lblock is sufficient.
            //  If there are k ones followed by a zero, the value of Lblock is incremented by k."
            u32 k = 0;
            while (TRY(read_bit()))
                k++;
            // dbgln("increment: {} bits", k);
            current_block_state.Lblock += k;
            u32 bits = current_block_state.Lblock + (u32)floor(log2(number_of_coding_passes));
            // dbgln("bits for length of codeword segment: {} bits", bits);
            if (bits > 32)
                return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Too many bits for length of codeword segment");
            u32 length = 0;
            for (u32 i = 0; i < bits; ++i) {
                bool bit = TRY(read_bit());
                length = (length << 1) | bit;
            }
            dbgln("length of codeword segment: {} bytes", length);
            current_block.length_of_data = length;

            // B.10.7.2 Multiple codeword segments
            // "Multiple codeword segments arise when a termination occurs between coding passes which are included in the packet"
            // XXX what's termination? reset of arithmetic coder context state?
            // "In normal operation (not selective arithmetic coding bypass), the arithmetic coder shall be terminated either
            //  at the end of every coding pass or only at the end of every code-block (see D.4.1)"
            // => This can only happen if uses_termination_on_each_coding_pass() or uses_selective_arithmetic_coding_bypass().
            //    We currently reject files with code_block_style != 0, so this cannot currently happen.
            //    Once we stop rejecting files with uses_termination_on_each_coding_pass() or uses_selective_arithmetic_coding_bypass()
            //    set, we must implement this.
            // FIXME: Implement.
        }
    }

    if (last_full_byte == 0xFF) {
dbgln("final stuff!");
        bool stuff_bit = TRY(read_bit());
        if (stuff_bit)
            return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Invalid bit-stuffing");
    }

    return header;
}

static ErrorOr<void> decode_tile_part(JPEG2000LoadingContext& context, TileData& tile, TilePartData& tile_part);
static ErrorOr<u32> decode_packet(JPEG2000LoadingContext& context, TileData& tile, ReadonlyBytes data);
static ErrorOr<void> decode_code_block(CodeblockBitplaneState& state, int M_b, CodeBlockPacketData& current_block   );
static void decoded_block_to_coefficients(CodeblockBitplaneState& state, CodeBlockPacketData& current_block, Vector<i16>& output, IntRect precinct_rect);

ErrorOr<void> decode_tile(JPEG2000LoadingContext& context, TileData& tile)
{
    auto const& cod = tile.cod.value_or(context.cod);

    if (cod.may_use_SOP_marker)
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: SOP marker not yet implemented");
    if (cod.may_use_EPH_marker)
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: EPH marker not yet implemented");

    // if (cod.number_of_layers != 1)
        // return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Cannot decode more than one layer yet");

    // Guaranteed by parse_codestream_tile_header.
    VERIFY(!tile.tile_parts.is_empty());

    for (auto& tile_part : tile.tile_parts)
        TRY(decode_tile_part(context, tile, tile_part));

    // if (tile.progression_iterator->has_next())
        // return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Not all progression orders were decoded");

    // return Error::from_string_literal("cannot decode tile yet");
    return {};
}

static ErrorOr<void> decode_tile_part(JPEG2000LoadingContext& context, TileData& tile, TilePartData& tile_part)
{
    // dbgln("decoding tile {} part {}", tile_part.sot.tile_index, tile_part.sot.tile_part_index);

    auto data = tile_part.data;

int n = 0;

    while (!data.is_empty()) {
        // if (n++ > 15) break; // XXX hacks
        (void)n;

        // XXX make this return combined codeblock data
        // XXX read headers first and do decoding in separate loop
        auto length = TRY(decode_packet(context, tile, data));
        data = data.slice(length);
    }

    return {};
}

static QuantizationDefault const& quantization_parameters_for_component(JPEG2000LoadingContext const& context, TileData const& tile, size_t component_index)
{
    // Tile-part QCC > Tile-part QCD > Main QCC > Main QCD
    for (auto const& qcc : tile.qccs) {
        if (qcc.component_index == component_index)
            return qcc.qcd;
    }

    if (tile.qcd.has_value())
        return tile.qcd.value();

    for (auto const& qcc : context.qccs) {
        if (qcc.component_index == component_index)
            return qcc.qcd;
    }

    return context.qcd;
}

static u8 get_exponent(QuantizationDefault const& quantization_parameters, SubBand sub_band, int resolution_level)
{
    switch (quantization_parameters.quantization_style) {
    case QuantizationDefault::QuantizationStyle::NoQuantization: {
        auto const& steps = quantization_parameters.step_sizes.get<Vector<QuantizationDefault::ReversibleStepSize>>();
        if (sub_band == SubBand::HorizontalLowpassVerticalLowpass) {
            VERIFY(resolution_level == 0);
            return steps[0].exponent;
        }
        VERIFY(resolution_level > 0);
        return steps[1 + (resolution_level - 1) * 3 + (int)sub_band - 1].exponent;
    }
    case QuantizationDefault::QuantizationStyle::ScalarDerived:
    case QuantizationDefault::QuantizationStyle::ScalarExpounded: {
        auto const& steps = quantization_parameters.step_sizes.get<Vector<QuantizationDefault::IrreversibleStepSize>>();

        if (quantization_parameters.quantization_style == QuantizationDefault::QuantizationStyle::ScalarDerived) {
            // Callers must use (E-5).
            return steps[0].exponent;
        }

        if (sub_band == SubBand::HorizontalLowpassVerticalLowpass) {
            VERIFY(resolution_level == 0);
            return steps[0].exponent;
        }
        VERIFY(resolution_level > 0);
        return steps[1 + (resolution_level - 1) * 3 + (int)sub_band - 1].exponent;
    }
    }
    VERIFY_NOT_REACHED();
}

static ErrorOr<DecodedTile*> get_or_create_decoded_tile(JPEG2000LoadingContext& context, TileData const& tile, int N_L)
{
    auto& decoded_tile = context.decoded_tiles[tile.index];
    if (decoded_tile.components.is_empty()) {
        dbgln("tile {} rect {}, making {} componennts", tile.index, tile.rect, context.siz.components.size());
        decoded_tile.components.resize(context.siz.components.size());
        for (auto [component_index, component] : enumerate(decoded_tile.components)) {
            int num_decomposition_levels = number_of_decomposition_levels_for_component(context, tile, component_index);
            dbgln("making {} sub-bands", num_decomposition_levels);
            component.decompositions.resize(num_decomposition_levels);
            component.decompositions_packet_state.resize(num_decomposition_levels);
            component.rect = context.siz.reference_grid_coordinates_for_tile_component(tile.rect, component_index);

            component.component_info = context.siz.components[component_index];

            // XXX this could be lazy (?)
            for (int r = 2; r <= num_decomposition_levels + 1; ++r) {
                int n_b = r == 0 ? N_L : (N_L + 1 - r); // always right branch
                auto rect = context.siz.reference_grid_coordinates_for_sub_band(tile.rect, component_index, n_b, SubBand::HorizontalLowpassVerticalLowpass);
dbgln("r {} llrect {}", r, rect);
                component.nLL_rects.append(rect);
            }
        }
    }
    return &decoded_tile;
}

static ErrorOr<DecodedCoefficients*> get_or_create_decoded_coefficients(JPEG2000LoadingContext& context, TileData const& tile, ProgressionData const& progression_data, SubBand sub_band, IntRect sub_band_rect)
{
    auto const coding_parameters = coding_style_parameters_for_component(context, tile, progression_data.component);
    auto& decoded_tile = *TRY(get_or_create_decoded_tile(context, tile, coding_parameters.number_of_decomposition_levels));

    int const r = progression_data.resolution_level;
    DecodedCoefficients& coefficients = r == 0 ? decoded_tile.components[progression_data.component].nLL : decoded_tile.components[progression_data.component].decompositions[r - 1][(int)sub_band - 1];
    if (coefficients.coefficients.is_empty()) {
        coefficients.rect = sub_band_rect;

// dbgln("resize to {}x{}", coefficients.size.width(), coefficients.size.height());
// dbgln("component {} level {} sub-band {} rect: {}", progression_data.component, r, (int)sub_band, coefficients.rect);
            coefficients.coefficients.resize(coefficients.rect.width() * coefficients.rect.height());
    }

    return &coefficients;
}

static ErrorOr<PacketDecodingState*> get_or_create_code_block_packet_data(JPEG2000LoadingContext& context, TileData const& tile, ProgressionData const& progression_data, SubBand sub_band, int num_precincts, int code_block_x_count, int code_block_y_count)
{
    auto const coding_parameters = coding_style_parameters_for_component(context, tile, progression_data.component);
    auto& decoded_tile = *TRY(get_or_create_decoded_tile(context, tile, coding_parameters.number_of_decomposition_levels));

    int const r = progression_data.resolution_level;
    PacketDecodingState& coefficients = r == 0 ? decoded_tile.components[progression_data.component].nLL_packet_state : decoded_tile.components[progression_data.component].decompositions_packet_state[r - 1][(int)sub_band - 1];
    if (coefficients.precinct_data.is_empty()) {
        coefficients.precinct_data.resize(num_precincts);
    }

    if (coefficients.precinct_data[progression_data.precinct].code_block_state.is_empty()) {
        coefficients.precinct_data[progression_data.precinct].code_block_state.resize(code_block_x_count * code_block_y_count);
        coefficients.precinct_data[progression_data.precinct].code_block_inclusion_tree = TRY(JPEG2000::TagTree::create(code_block_x_count, code_block_y_count));
        coefficients.precinct_data[progression_data.precinct].p_tree = TRY(JPEG2000::TagTree::create(code_block_x_count, code_block_y_count));
    }

    return &coefficients;
}

static ErrorOr<CodeblockBitplaneState*> get_or_create_code_block_bitplane_state(JPEG2000LoadingContext& context, TileData const& tile, ProgressionData const& progression_data, SubBand sub_band, int codeblock_index, int num_precincts, int code_block_x_count, int code_block_y_count)
{
    auto const coding_parameters = coding_style_parameters_for_component(context, tile, progression_data.component);
    auto& decoded_tile = *TRY(get_or_create_decoded_tile(context, tile, coding_parameters.number_of_decomposition_levels));

    int const r = progression_data.resolution_level;
    PacketDecodingState& coefficients = r == 0 ? decoded_tile.components[progression_data.component].nLL_packet_state : decoded_tile.components[progression_data.component].decompositions_packet_state[r - 1][(int)sub_band - 1];
    if (coefficients.codeblock_bitplane_state.is_empty()) {
        coefficients.codeblock_bitplane_state.resize(num_precincts);
    }

    if (coefficients.codeblock_bitplane_state[progression_data.precinct].is_empty()) {
        coefficients.codeblock_bitplane_state[progression_data.precinct].resize(code_block_x_count * code_block_y_count);
    }

    return &coefficients.codeblock_bitplane_state[progression_data.precinct][codeblock_index];
}


static ErrorOr<u32> decode_packet(JPEG2000LoadingContext& context, TileData& tile, ReadonlyBytes data)
{
    ProgressionData progression_data;
    do {
        if (!tile.progression_iterator->has_next())
            return Error::from_string_literal("JPEG2000ImageDecoderPlugin: No more progression orders but packets left");
        progression_data = tile.progression_iterator->next();

    // dbgln("progression order candidate: tile {} layer {}, resolution level: {}, component: {}, precinct {}", tile.index, progression_data.layer, progression_data.resolution_level, progression_data.component, progression_data.precinct);

    } while (progression_data.resolution_level > number_of_decomposition_levels_for_component(context, tile, progression_data.component));

    dbgln("progression order: tile {} layer {}, resolution level: {}, component: {}, precinct {}", tile.index, progression_data.layer, progression_data.resolution_level, progression_data.component, progression_data.precinct);

    // Compute tile size at resolution level r.
    int r = progression_data.resolution_level;
    int component_index = progression_data.component;
    auto const coding_parameters = coding_style_parameters_for_component(context, tile, component_index);

    auto ll_rect = context.siz.reference_grid_coordinates_for_ll_band(tile.rect, component_index, r, coding_parameters.number_of_decomposition_levels);

// dbgln("tile_rect: {}, ll_rect: {}", tile.rect, ll_rect);

    // B.6
    // (B-16)
    int num_precincts_wide = 0;
    int num_precincts_high = 0;
    int PPx = coding_parameters.precinct_sizes[r].PPx;
    int PPy = coding_parameters.precinct_sizes[r].PPy;

    if (ll_rect.width() != 0) {
        num_precincts_wide = ceil_div(ll_rect.right(), 1 << PPx) - (ll_rect.left() / (1 << PPx));
    }
    if (ll_rect.height() != 0) {
        num_precincts_high = ceil_div(ll_rect.bottom(), 1 << PPy) - (ll_rect.top() / (1 << PPy));
    }
    // dbgln("num_precincts_wide: {}, num_precincts_high: {}", num_precincts_wide, num_precincts_high);

    // "It can happen that numprecincts is 0 for a particular tile-component and resolution level. When this happens, there are no packets for this tile-component and resolution level."
    // XXX: handle


    // XXX spec says to do this only for computing xcb_prime, but apparently PPx/PPy have to be modified too?
    // Fixes first precinct size at r=1 in sunset-retro-levels-3-block-64x32-precinct-256x128-tile-699x299.jp2
    // (line 37-45 in sizes.txt correct with this - the 3 subbands for the first 3 precincts.
    // fourth precinct needs more tweaks.)
    // ...aha, if doing this only after computing num_precincts_wide/_high, we get the first 468 right

    auto precinct_origin = IntPoint { ll_rect.x() & ~((1 << PPx) - 1), ll_rect.y() & ~((1 << PPy) - 1) };

    if (r > 0) {
        PPx--;
        PPy--;
        precinct_origin /= 2;
    }

    // B.7
    // (B-17)
    // int xcb_prime = min(coding_parameters.code_block_width_exponent, r > 0 ? PPx - 1 : PPx);
    int xcb_prime = min(coding_parameters.code_block_width_exponent, PPx);

    // (B-18)
    // int ycb_prime = min(coding_parameters.code_block_height_exponent, r > 0 ? PPy - 1 : PPy);
    int ycb_prime = min(coding_parameters.code_block_height_exponent, PPy);

    // dbgln("PPX: {}, PPY: {}, xcb: {} , ycb: {}, xcb_prime: {}, ycb_prime: {}", PPx, PPy, coding_parameters.code_block_width_exponent, coding_parameters.code_block_height_exponent, xcb_prime, ycb_prime);

    // A precinct has sample size 2^PPx * 2^PPy, and a packet contains all code-blocks in a precinct.
    // A codeblock is a 2^xcb' * 2^ycb' block of samples (bounded by precinct size as described in B.7).
    // That means there are 2^(PPX - xcb') * 2^(PPy - ycb') code-blocks in a precinct, and also in a packet.
    // XXX where does the spec say that?
    // XXX also, if precincts are way larger than the image, then this here produces
    //     oodles of codeblocks outside the subband. Need to clip somewhere, but ideally the spec would say that somewhere.
    int precinct_x_index = progression_data.precinct % num_precincts_wide;
    int precinct_y_index = progression_data.precinct / num_precincts_wide;

// XXX this needs to also add the tile offset
    auto precinct_rect = IntRect({ precinct_x_index * (1 << PPx), precinct_y_index * (1 << PPy), 1 << PPx, 1 << PPy });

    // with this, 492 right, until resolution level 1 of tile 1
    // ...hmm this here aligns to the not-downscaled ll_rect coordinates.
    // probably have to do this before downscaling PPx / PPy and then scale down the whole offset?
    // precinct_rect.set_x(precinct_rect.x() + (ll_rect.x() & ~((1 << PPx) - 1)));
    // precinct_rect.set_y(precinct_rect.y() + (ll_rect.y() & ~((1 << PPy) - 1)));

    // woohoo with this we get all 2014 codeblock sizes right!
    precinct_rect.set_location(precinct_rect.location() + precinct_origin);

    PacketContext packet_context;
    packet_context.xcb_prime = xcb_prime;
    packet_context.ycb_prime = ycb_prime;
    packet_context.precinct_rect = precinct_rect;
    packet_context.num_precincts = num_precincts_wide * num_precincts_high;

    if (data.is_empty())
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Cannot handle tile-parts without any packets yet");

#if 0
dbg("raw header:");
for (int i = 0; i < 15; ++i) {
if (i % 4 == 0) dbg(" ");
dbg("{:02x}", data[i]);
}
dbgln();
#endif

    // XXX maybe pass in bytes instead of stream?
    FixedMemoryStream stream { data };
    auto header = TRY(read_packet_header(context, stream, packet_context, tile, coding_parameters, progression_data));
    if (header.is_empty) {
dbgln("empty packet per header; skipping");
        return stream.offset();
    }

    // FIXME: Read actual packet data too
    // That's Annex D.
    // D.3 Decoding passes over the bit-planes
    // "four coding passes:
    //  - significance coding,
    //  - sign coding,
    //  - magnitude refinement coding, and
    //  - cleanup coding."
    // "three coding passes over each bit-plane:
    //  - significance and sign coding in a significance propagation pass,
    //  - magnitude refinement coding in a magnitude refinement pass, and
    //  - cleanup and sign coding in a cleanup pass."
    // "The first bit-plane within the current block with a non-zero element has a cleanup pass only.
    //  The remaining bit-planes are decoded in three coding passes."

    // FIXME: Relax. Will need implementing D.5, D.6, D.7, and probably more.
    if (coding_parameters.code_block_style != 0)
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Code-block style not yet implemented");

    int number_of_sub_bands = r == 0 ? 1 : 3;

    // Set `data` on each codeblock on the packet.
    u32 offset = stream.offset();
    for (int i = 0; i < number_of_sub_bands; ++i) {
        for (auto const& [current_block_index, current_block] : enumerate(header.sub_bands[i].code_blocks)) {
            // FIXME: Are codeblocks on byte boundaries? => Looks like it. Find spec ref.
            // XXX Make read_packet_header() store codeblock byte ranges on CodeBlock instead of doing it here (?)

            // "The sequence of bytes actually included for any given code-block must not end in a 0xFF"
            // XXX: check?
            current_block.data = data.slice(offset, current_block.length_of_data);
            offset += current_block.length_of_data;

#if 1
// store current block's data in a file for debugging
    // dbgln("progression order: tile {} layer {}, resolution level: {}, component: {}, precinct {}", tile.index, progression_data.layer, progression_data.resolution_level, progression_data.component, progression_data.precinct);

auto filename = TRY(String::formatted("jp2k-codeblock-tile-{}-layer-{}-resolutionlevel-{}-component-{}-precinct-{}-subband-{}-codeblock-{}.bin",
    tile.index, progression_data.layer, progression_data.resolution_level, progression_data.component, progression_data.precinct, i, current_block_index));
    TRY(TRY(Core::File::open(filename, Core::File::OpenMode::ReadWrite | Core::File::OpenMode::Truncate))->write_until_depleted(current_block.data));
#endif
        }
    }

// XXX hacks
// if (progression_data.layer > 0) return offset;

    // Decode. (Could do this later / elsewhere.)
    for (int i = 0; i < number_of_sub_bands; ++i) {

        // Have to create the DecodedCoefficients before possibly skipping this subband, so that the non-empty dimension in the
        // rect matches an assert in IDWT.
        auto sub_band = r == 0 ? SubBand::HorizontalLowpassVerticalLowpass : (SubBand)(i + 1); // XXX store on SubBand?
        auto& coefficients = *TRY(get_or_create_decoded_coefficients(context, tile, progression_data, sub_band, header.sub_bands[i].subband_rect));

        if (header.sub_bands[i].codeblock_x_count == 0 || header.sub_bands[i].codeblock_y_count == 0) {
            dbgln("skipping sub-band {} with no code-blocks", i);
            continue;
        }

        // Annex E, E.1 Inverse quantization procedure:
        // "Mb = G + exp_b - 1       (E-2)
        //  where the number of guard bits G and the exponent exp_b are specified in the QCD or QCC marker segments (see A.6.4 and A.6.5)."
        auto quantization_parameters = quantization_parameters_for_component(context, tile, progression_data.component);
        auto exponent = get_exponent(quantization_parameters, sub_band, r);

        if (quantization_parameters.quantization_style == QuantizationDefault::QuantizationStyle::ScalarDerived) {
            int N_L = coding_parameters.number_of_decomposition_levels;
            // Table F.1 – Decomposition level nb for sub-band b
            // XXX: Spec suggests that this ends with n_b = 1, but if N_L is 0, we have 0LL and nothing else.
            int n_b = r == 0 ? N_L : (N_L + 1 - r);
            // (E-5)
            exponent = exponent - N_L + n_b;
            // same as `if (r != 0) exponent = exponent - (r - 1);`
        }

        int M_b = quantization_parameters.number_of_guard_bits + exponent - 1;
dbgln("exponent: {}, M_b: {}", exponent, M_b);

        // Read bitplanes of all code-blocks.
        for (auto [code_block_index, current_block] : enumerate(header.sub_bands[i].code_blocks)) {
            if (!current_block.is_included)
                continue;

            auto& state = *TRY(get_or_create_code_block_bitplane_state(context, tile, progression_data, sub_band, code_block_index, packet_context.num_precincts, header.sub_bands[i].codeblock_x_count, header.sub_bands[i].codeblock_y_count));

            if (current_block.is_included_for_the_first_time)
                state.original_p = current_block.p;

            state.layer_data.append(current_block.data);
            state.total_number_of_coding_passes += current_block.number_of_coding_passes;

            size_t total_size = 0;
            for (auto& data : state.layer_data)
                total_size += data.size();
            ByteBuffer combined_data = TRY(ByteBuffer::create_uninitialized(total_size));
            size_t offset = 0;
            for (auto& data : state.layer_data) {
                memcpy(combined_data.offset_pointer(offset), data.data(), data.size());
                offset += data.size();
            }

            if (current_block.is_included_for_the_first_time || true) {
                int w = current_block.rect.width();
                int h = current_block.rect.height();
                int num_strips = ceil_div(h, 4);

                TRY(state.significance_and_sign.try_resize(0));
                TRY(state.significance_and_sign.try_resize(w * num_strips));

                TRY(state.magnitudes.try_resize(0));
                TRY(state.magnitudes.try_resize(w * h));

                TRY(state.became_significant_in_pass.try_resize(0));
                TRY(state.became_significant_in_pass.try_resize(w * h));

                TRY(state.was_coded_in_pass.try_resize(0));
                TRY(state.was_coded_in_pass.try_resize(w * h));

                // state.current_bitplane = current_block.p;
                state.current_bitplane = state.original_p;
                // state.current_bitplane = state.original_p - 1;
                state.pass = 0;
                state.reset_contexts();

                // state.arithmetic_decoder = TRY(QMArithmeticDecoder::initialize(current_block.data));
                state.arithmetic_decoder = TRY(QMArithmeticDecoder::initialize(combined_data));

                // state.arithmetic_decoder.data().enqueue(current_block.data);
                // state.arithmetic_decoder.INITDEC();
            } else if (current_block.is_included && current_block.data.size() > 0) {
dbgln("enqueuing arithmetic decoder data size {}", current_block.data.size());
#if 0
                state.arithmetic_decoder = TRY(QMArithmeticDecoder::initialize(current_block.data));
#else
                state.arithmetic_decoder.data().append(current_block.data);
#endif
                // state.arithmetic_decoder.flush();
            }

// if (current_block.is_included_for_the_first_time)
            TRY(decode_code_block(state, M_b, current_block));
        }

        // Assemble bitplanes from all code-blocks into coefficients of the whole precinct.

        // XXX: the following comment is outdated
        // XXX: only do this for the first precinct, so that additional precincts don't clobber earlier precincts
        // or alternatively, only return one precint's worth of data here adn assemble outside
        // if (header.sub_bands[i].coefficients.is_empty())
            // header.sub_bands[i].coefficients.resize(header.sub_bands[i].subband_rect.width() * header.sub_bands[i].subband_rect.height());
        // ...I think this only does one precinct of data now, but it does the resize once per layer, which is at best wasteful.
        // But it should fill the combined bits from several layers into this, so it should at least work (?)
        auto clipped_precinct_rect = header.sub_bands[i].subband_rect.intersected(precinct_rect);
        Vector<i16> precinct_coefficents;
        precinct_coefficents.resize(clipped_precinct_rect.width() * clipped_precinct_rect.height());
        for (auto [code_block_index, current_block] : enumerate(header.sub_bands[i].code_blocks)) {
            if (!current_block.is_included)
                continue;

            // XXX: this never creates; have a non-creating getter too?
            auto& state = *TRY(get_or_create_code_block_bitplane_state(context, tile, progression_data, sub_band, code_block_index, packet_context.num_precincts, header.sub_bands[i].codeblock_x_count, header.sub_bands[i].codeblock_y_count));
            // XXX oh! only do this the last time round!
            decoded_block_to_coefficients(state, current_block, precinct_coefficents, clipped_precinct_rect);

#if 1
// store decompressed data in file for debugging
// XXX this writes the whole precinct, not just the codeblock :/
auto filename = MUST(String::formatted("jp2k-decompressed-tile-{}-r-{}-component-{}-precinct-{}-subband-{}-codeblock-{}.bin",
    tile.index, progression_data.resolution_level, progression_data.component, progression_data.precinct, i, code_block_index));
    auto file = MUST(Core::File::open(filename, Core::File::OpenMode::ReadWrite | Core::File::OpenMode::Truncate));

dbgln("writing uncompressed {} * {} = {}", current_block.rect.width(), current_block.rect.height(), current_block.rect.width() * current_block.rect.height());
    for (int y = 0; y < current_block.rect.height(); ++y) {
        for (int x = 0; x < current_block.rect.width(); ++x) {
            // dbgln("x {} y {}", x, y);
            i16 val = precinct_coefficents[(y + current_block.rect.top() - clipped_precinct_rect.top()) * clipped_precinct_rect.width() + (x + current_block.rect.left() - clipped_precinct_rect.left())];
            i32 val32 = val;
            MUST(file->write_value(val32));
        }
    }

    // for (i16 val : precinct_coefficents) {
    //     i32 val32 = val;
    //     MUST(file->write_value(val32));
    // }
#endif

        }

        // Convert precinct i16 coefficients into dequantized floats, and put into the subband buffer (which can have several precincts).
        // XXX do this only once, instead of once per packet. as-is, we do this conversion every time we decode a layer.
        // XXX could do this only before the IDWT, for the whole image at once, maybe

        // auto rect = header.sub_bands[i].subband_rect;
        auto rect = clipped_precinct_rect;
        int w = rect.width();
        int h = rect.height();

        // XXX here, append() a PrecinctInfo with rect clipped_precinct_rect, and with a Vector<IntRect> code_block rects.
        // want to do this per sub-band.

        for (int y = 0; y < h; ++y) {
            for (int x = 0; x < w; ++x) {
                float value = precinct_coefficents[y * w + x];

                // E.1 Inverse quantization procedure
                // We coefficients stores qbar_b.
                if (quantization_parameters.quantization_style != QuantizationDefault::QuantizationStyle::NoQuantization) {
                    // E.1.1 Irreversible transformation
                    auto R_I = context.siz.components[progression_data.component].bit_depth();

                    // Table E.1 – Sub-band gains
                    auto log_2_gain_b = sub_band == SubBand::HorizontalLowpassVerticalLowpass ? 0 : (sub_band == SubBand::HorizontalHighpassVerticalLowpass || sub_band == SubBand::HorizontalLowpassVerticalHighpass ? 1 : 2);
                    auto R_b = R_I + log_2_gain_b;

                    u16 mantissa;
                    if (quantization_parameters.quantization_style == QuantizationDefault::QuantizationStyle::ScalarDerived) {
                        // (E-5)
                        mantissa = quantization_parameters.step_sizes.get<Vector<QuantizationDefault::IrreversibleStepSize>>()[0].mantissa;
                    } else {
                        if (r == 0)
                            mantissa = quantization_parameters.step_sizes.get<Vector<QuantizationDefault::IrreversibleStepSize>>()[0].mantissa;
                        else
                            mantissa = quantization_parameters.step_sizes.get<Vector<QuantizationDefault::IrreversibleStepSize>>()[3 * (r - 1) + (int)sub_band].mantissa;
                    }

                    // (E-3)
                    float step_size = powf(2.0f, R_b - exponent) * (1.0f + mantissa / powf(2.0f, 11.0f));
                    // if (i == 0) dbgln("step size: {}", step_size);
                    value *= step_size; // XXX round and clamp

                    // XXX this is incomplete: if bitplanes are missing, need to scale up
                }

                // if (i == 0) dbgln("x {} y {} value {}", x, y, value);
                // if (i == 0) dbgln("index {} bounds {} / {}", y * w + h, coefficients.size, coefficients.coefficients.size());
                // coefficients.coefficients[y * w + x] = value; // XXX precinct bounds!

if (x == 15 && y == 31)
    dbgln("coefficient value: {}", value);

                auto subband_origin = header.sub_bands[i].subband_rect.location();
                coefficients.coefficients[(y + clipped_precinct_rect.top() - subband_origin.y()) * header.sub_bands[i].subband_rect.width() + (x + clipped_precinct_rect.left() - subband_origin.x())] = value; // XXX precinct bounds!
            }
        }

#if 0
{
    w = header.sub_bands[i].subband_rect.width();
    h = header.sub_bands[i].subband_rect.height();
    auto bitmap = TRY(Gfx::Bitmap::create(Gfx::BitmapFormat::BGRA8888, { w, h }));
    for (int y = 0; y < h; ++y) {
        for (int x = 0; x < w; ++x) {
            float value = coefficients.coefficients[y * w + x];

            // XXX this should happen after IDWT and after component transformation
            // G.1.2 Inverse DC level shifting of tile-components
            // (G-2)
            // XXX - 1 or not here? -1 looks better and matches spec text in G.1.2 but maybe not Table A.11 in A.5.1 (?)
            if (!context.siz.components[progression_data.component].is_signed())
                value += 1u << (context.siz.components[progression_data.component].bit_depth() - 1);

            u8 byte_value = (u8)clamp(value, 0.0f, 255.0f);
            //value = (value + 256) / 2;
            // dbgln("x {} y {} value {}", x, y, value);
            Color pixel;
            pixel.set_red(byte_value);
            pixel.set_green(byte_value);
            pixel.set_blue(byte_value);
            pixel.set_alpha(255);
            bitmap->set_pixel(x, y, pixel);
        }
    }

    auto name = TRY(String::formatted("image-tile-{}-layer-{}-precinct-{}-component-{}-level-{}-subband-{}.png", tile.index, progression_data.layer, progression_data.precinct, progression_data.component, progression_data.resolution_level, (int)sub_band));
    auto output_stream = TRY(Core::File::open(name, Core::File::OpenMode::Write));
    auto file = TRY(Core::OutputBufferedFile::create(move(output_stream)));
    auto bytes = TRY(Gfx::PNGWriter::encode(*bitmap));
    TRY(file->write_until_depleted(bytes));
}
#endif

    }

    // XXX could just read the packet header here and decode the code blocks later, in parallel.
    // (...since the header stores the length of the data, it's easy to skip over it.)
    return offset;
}

static void decoded_block_to_coefficients(CodeblockBitplaneState& state, CodeBlockPacketData& current_block, Vector<i16>& output, IntRect precinct_rect)
{
    int w = current_block.rect.width();
    int h = current_block.rect.height();

    Vector<u8>& significance_and_sign = state.significance_and_sign;
    Vector<u16>& magnitudes = state.magnitudes;

    // XXX: make method on state
    auto get_sign = [&](int x, int y) {
        auto strip_index = y / 4;
        auto strip_y = y % 4;
        auto strip_offset = strip_index * w;
        auto strip_value = significance_and_sign[strip_offset + x];
        return (strip_value & (1 << (strip_y + 4))) != 0;
    };

    // XXX bitplane to coefficient conversion should be a dedicated step somewhere else.
    // (...hmm, but this only outputs the bitplanes really, so it's fine? maybe?)
    for (int y = 0; y < h; ++y) {
        for (int x = 0; x < w; ++x) {
            auto sign = get_sign(x, y);
            auto magnitude = magnitudes[y * w + x];
            auto value = magnitude * (sign ? -1 : 1);

if (x == 15 && y == 31)
    dbgln("final value: {}", value);

            // XXX make relative to subband origin?
            // output.coefficients[(y + current_block.rect.top()) * output.subband_rect.width() + (x + current_block.rect.left())] = value;
            output[(y + current_block.rect.top() - precinct_rect.top()) * precinct_rect.width() + (x + current_block.rect.left() - precinct_rect.left())] = value;
        }
    }
}

static ErrorOr<void> decode_code_block(CodeblockBitplaneState& state, int M_b, CodeBlockPacketData& current_block)
{
    // Only have to early-return on these I think, but can also only happen in some multi-layer scenarios, which have other parts missing too.
    // if (!current_block.is_included)
        // return Error::from_string_literal("Cannot handle non-included codeblocks yet");
    VERIFY(current_block.is_included);

    QMArithmeticDecoder& arithmetic_decoder = state.arithmetic_decoder;

    // Strips of four vertical coefficients at a time.
    // State per coefficient:
    // - significance
    // - sign
    // - magnitude
    // Store this as a struct with:
    // - u8 with 2 bits for significance and sign for the four coefficients
    // - One u16 per coefficient for magnitude
    // FIXME: Store this external to this function since some bitplanes could
    //        be in a later packet.

    // B.7 Division of the sub-bands into code-blocks
    // "NOTE – Code-blocks in the partition may extend beyond the boundaries of the sub-band coefficients. When this happens, only the
    //  coefficients lying within the sub-band are coded using the method described in Annex D. The first stripe coded using this method
    //  corresponds to the first four rows of sub-band coefficients in the code-block or to as many such rows as are present."
    int w = current_block.rect.width();
    int h = current_block.rect.height();
    // dbgln("code-block rect: {}, sub-band {}", current_block.rect, (int)current_block.sub_band);

    Vector<u8>& significance_and_sign = state.significance_and_sign;
    Vector<u16>& magnitudes = state.magnitudes;

    auto is_significant = [&](int x, int y) {
        if (x < 0 || x >= w || y < 0 || y >= h)
            return false;
        auto strip_index = y / 4;
        auto strip_y = y % 4;
        auto strip_offset = strip_index * w;
        auto strip_value = significance_and_sign[strip_offset + x];
        return (strip_value & (1 << strip_y)) != 0;
    };
    auto get_sign = [&](int x, int y) {
        auto strip_index = y / 4;
        auto strip_y = y % 4;
        auto strip_offset = strip_index * w;
        auto strip_value = significance_and_sign[strip_offset + x];
        return (strip_value & (1 << (strip_y + 4))) != 0;
    };

    auto set_significant = [&](int x, int y, bool value) {
        auto strip_index = y / 4;
        auto strip_y = y % 4;
        auto strip_offset = strip_index * w;
        auto strip_value = significance_and_sign[strip_offset + x];
        if (value)
            strip_value |= 1 << strip_y;
        else
            strip_value &= ~(1 << strip_y);
        significance_and_sign[strip_offset + x] = strip_value;
    };
    auto set_sign = [&](int x, int y, bool value) {
        auto strip_index = y / 4;
        auto strip_y = y % 4;
        auto strip_offset = strip_index * w;
        auto strip_value = significance_and_sign[strip_offset + x];
        if (value)
            strip_value |= 1 << (strip_y + 4);
        else
            strip_value &= ~(1 << (strip_y + 4));
        significance_and_sign[strip_offset + x] = strip_value;
    };

    auto compute_context_ll_lh = [&](int x, int y) -> unsigned {
        // Table D.1 – Contexts for the significance propagation and cleanup coding passes
        u8 sum_h = is_significant(x - 1, y) + is_significant(x + 1, y);
        u8 sum_v = is_significant(x, y - 1) + is_significant(x, y + 1);
        u8 sum_d = is_significant(x - 1, y - 1) + is_significant(x - 1, y + 1) + is_significant(x + 1, y - 1) + is_significant(x + 1, y + 1);

        if (sum_h == 2)
            return 8;

        if (sum_h == 1) {
            if (sum_v >= 1)
                return 7;
            if (sum_d >= 1)
                return 6;
            return 5;
        }

        if (sum_v == 2)
            return 4;
        if (sum_v == 1)
            return 3;
        if (sum_d >= 2)
            return 2;
        if (sum_d == 1)
            return 1;

        return 0;
    };

    // Like compute_context_ll_lh but with sum_h and sum_v swapped
    auto compute_context_hl = [&](int x, int y) -> unsigned {
        // Table D.1 – Contexts for the significance propagation and cleanup coding passes
        u8 sum_h = is_significant(x - 1, y) + is_significant(x + 1, y);
        u8 sum_v = is_significant(x, y - 1) + is_significant(x, y + 1);
        u8 sum_d = is_significant(x - 1, y - 1) + is_significant(x - 1, y + 1) + is_significant(x + 1, y - 1) + is_significant(x + 1, y + 1);

        if (sum_v == 2)
            return 8;

        if (sum_v == 1) {
            if (sum_h >= 1)
                return 7;
            if (sum_d >= 1)
                return 6;
            return 5;
        }

        if (sum_h == 2)
            return 4;
        if (sum_h == 1)
            return 3;
        if (sum_d >= 2)
            return 2;
        if (sum_d == 1)
            return 1;

        return 0;
    };

    auto compute_context_hh = [&](int x, int y) -> unsigned {
        // Table D.1 – Contexts for the significance propagation and cleanup coding passes
        u8 sum_h = is_significant(x - 1, y) + is_significant(x + 1, y);
        u8 sum_v = is_significant(x, y - 1) + is_significant(x, y + 1);
        u8 sum_h_v = sum_h + sum_v;
        u8 sum_d = is_significant(x - 1, y - 1) + is_significant(x - 1, y + 1) + is_significant(x + 1, y - 1) + is_significant(x + 1, y + 1);

        if (sum_d >= 3)
            return 8;

        if (sum_d == 2) {
            if (sum_h_v >= 1)
                return 7;
            return 6;
        }

        if (sum_d == 1) {
            if (sum_h_v >= 2)
                return 5;
            if (sum_h_v == 1)
                return 4;
            return 3;
        }

        if (sum_h_v >= 2)
            return 2;
        if (sum_h_v == 1)
            return 1;

        return 0;
    };

    auto compute_context = [&](int x, int y) -> unsigned {
        switch (current_block.sub_band) {
        case SubBand::HorizontalLowpassVerticalLowpass:
        case SubBand::HorizontalLowpassVerticalHighpass:
            return compute_context_ll_lh(x, y);
        case SubBand::HorizontalHighpassVerticalLowpass:
            return compute_context_hl(x, y);
        case SubBand::HorizontalHighpassVerticalHighpass:
            return compute_context_hh(x, y);
        }
    };

    auto v_or_h_contribution = [&](IntPoint p, IntPoint d0, IntPoint d1) -> i8 {
        auto p0 = p + d0;
        auto p1 = p + d1;
        // Table D.2 – Contributions of the vertical (and the horizontal) neighbours to the sign context
        if (is_significant(p1.x(), p1.y())) {
            if (get_sign(p1.x(), p1.y()) == false) { // positive (XXX enum class)
                if (is_significant(p0.x(), p0.y()))
                    return get_sign(p0.x(), p0.y()) == false ? 1 : 0;
                return 1;
            }
            if (is_significant(p0.x(), p0.y()))
                return get_sign(p0.x(), p0.y()) == false ? 0 : -1;
            return -1;
        }
        if (is_significant(p0.x(), p0.y()))
            return get_sign(p0.x(), p0.y()) == false ? 1 : -1;
        return 0;
    };

    // header.block.number_of_coding_passes is probably number of bitplanes in this packet (?)
    // XXX optionally reinitialize contexts between bitplanes, depending on uses_termination_on_each_coding_pass().
    // Currently, we reinitialize after every code-block (which is what we usually want).
    // (Note that we don't reset between packets for same codeblocks.)

    // D.4 Initializing and terminating
    // Table D.7 – Initial states for all contexts
    QMArithmeticDecoder::Context& uniform_context = state.uniform_context;
    QMArithmeticDecoder::Context& run_length_context = state.run_length_context;
    Array<QMArithmeticDecoder::Context, 17>& all_other_contexts = state.all_other_contexts;

    auto read_sign_bit = [&](int x, int y) {
        // C2, Decode sign bit of current coefficient
        // Sign bit
        // D.3.2 Sign bit decoding
        // Table D.2 – Contributions of the vertical (and the horizontal) neighbours to the sign context
        i8 v_contribution = v_or_h_contribution({ x, y }, { 0, -1 }, { 0, 1 });
        i8 h_contribution = v_or_h_contribution({ x, y }, { -1, 0 }, { 1, 0 });
        // Table D.3 – Sign contexts from the vertical and horizontal contributions
        u8 context_label = 9;
        if (h_contribution == 0)
            context_label += abs(v_contribution);
        else
            context_label += 3 + h_contribution * v_contribution;
        u8 xor_bit = 0;
        if (h_contribution == -1 || (h_contribution == 0 && v_contribution == -1))
            xor_bit = 1;
        bool sign_bit = arithmetic_decoder.get_next_bit(all_other_contexts[context_label]) ^ xor_bit;
        // dbgln("sigprop sign_bit: {} (context {})", sign_bit, context_label);
        return sign_bit;
    };

    // Figure D.3 – Flow chart for all coding passes on a code-block bit-plane
    // Table D.10 – Decisions in the context model flow chart
    // Table D.11 – Decoding in the context model flow chart

    // XXX have to store this on the codeblock, so that we can continue decoding in the next packet.
    Vector<u8>& became_significant_in_pass = state.became_significant_in_pass;

    // Set even for coefficients that are not significant, if they had an explicit "not significant" bit.
    // XXX probably have to store this elsewhere too? Can layers start at a different pass?
    Vector<u8>& was_coded_in_pass = state.was_coded_in_pass;

    // D.2.1
    // "The number Nb(u, v) of decoded MSBs includes the number of all zero most significant bit-planes signalled in the packet header (see B.10.5)."
    // B.10.5 Zero bit-plane information
    // "the number of missing most significant bit-planes, P, may vary from code-block to code-block; these missing bit-planes are all taken to be zero."
    // (E-1) has value computation.

    // E.1
    // "Due to the nature of the three coding passes (see D.3), Nb(u, v) may be different for different coefficients within the same code-block."
    // ?? what why
    // Am I supposed to keep track when each coefficient becomes significant? Does Nb(u, v) include that information?

    // int num_bits = (current_block.number_of_coding_passes - 1) / 3 + 1; // /shruggie
    int num_bits = M_b - 1; // Spec indexes i starting 1, we (morally) start current_bitplane at 0.
    // int num_bits = M_b;
    dbgln("num_bits: {} (p {})", num_bits, current_block.p);

    auto significance_propagation_pass = [&](int current_bitplane, int pass) {
        // D.3.1 Significance propagation decoding pass
        for (int y = 0; y < h; y += 4) { // XXX does += 4 make sense here?
            int y_end = min(y + 4, h);
            int num_rows = y_end - y;
            for (int x = 0; x < w; ++x) {
                for (u8 coefficient_index = 0; coefficient_index < num_rows; ++coefficient_index) {
                    // dbgln("sigprop x: {}, y: {}", x, y + coefficient_index);

                    // D1, Is the current coefficient already significant?
                    if (!is_significant(x, y + coefficient_index)) {
                        // D2, Is the context bin zero? (see Table D.1)
                        u8 context = compute_context(x, y + coefficient_index);
                        if (context != 0) {
                            // C1, Decode significance bit of current coefficient (See D.3.1)
                            // u8 context = compute_context(x, y + coefficient_index); // PERF: could use `contexts` cache (needs invalidation then).
                            bool is_newly_significant = arithmetic_decoder.get_next_bit(all_other_contexts[context]);
                            // dbgln("sigprop is_newly_significant: {} (context {})", is_newly_significant, context);
                            // is_current_coefficient_significant = is_newly_significant;
                            set_significant(x, y + coefficient_index, is_newly_significant);
                            if (is_newly_significant) {
                                became_significant_in_pass[(y + coefficient_index) * w + x] = current_bitplane;
                                magnitudes[(y + coefficient_index) * w + x] |= 1 << (num_bits - current_bitplane); // XXX: correct?
                            }
                            was_coded_in_pass[(y + coefficient_index) * w + x] = pass;

                            // D3, Did the current coefficient just become significant?
                            if (is_newly_significant) {
                                bool sign_bit = read_sign_bit(x, y + coefficient_index);
                                set_sign(x, y + coefficient_index, sign_bit);
                            }
                        }
                    }
                    // D4, Are there more coefficients in the significance propagation?
                    // C0, Go to the next coefficient or column
                }
            }
        }
    };

    auto magnitude_refinement_pass = [&](int current_bitplane) {
        // D.3.3 Magnitude refinement pass
        for (int y = 0; y < h; y += 4) { // XXX does += 4 make sense here?
            int y_end = min(y + 4, h);
            int num_rows = y_end - y;
            // XXX maybe store a "is any pixel significant in this scanline" flag to skip entire scanlines? measure if that's worth it.
            for (int x = 0; x < w; ++x) {
                for (u8 coefficient_index = 0; coefficient_index < num_rows; ++coefficient_index) {
                    // dbgln("magnitude x: {}, y: {}", x, y + coefficient_index);

                    // D5, Is the coefficient insignificant?
                    if (!is_significant(x, y + coefficient_index))
                        continue;

                    // D6, Was the coefficient coded in the last significance propagation?
                    if (became_significant_in_pass[(y + coefficient_index) * w + x] != current_bitplane) {
                        // C3, Decode magnitude refinement pass bit of current coefficient
                        // Table D.4 – Contexts for the magnitude refinement coding passes
                        u8 context;
                        if (became_significant_in_pass[(y + coefficient_index) * w + x] == current_bitplane - 1) {
                            u8 sum_h = is_significant(x - 1, y + coefficient_index) + is_significant(x + 1, y + coefficient_index);
                            u8 sum_v = is_significant(x, y + coefficient_index - 1) + is_significant(x, y + coefficient_index + 1);
                            u8 sum_d = is_significant(x - 1, y + coefficient_index - 1) + is_significant(x - 1, y + coefficient_index + 1) + is_significant(x + 1, y + coefficient_index - 1) + is_significant(x + 1, y + coefficient_index + 1);
                            context = (sum_h + sum_v + sum_d) >= 1 ? 15 : 14;
                        } else {
                            context = 16;
                        }
                        bool magnitude_bit = arithmetic_decoder.get_next_bit(all_other_contexts[context]);
                        // dbgln("magnitude_bit: {} (context {})", magnitude_bit, context);
                        magnitudes[(y + coefficient_index) * w + x] |= magnitude_bit << (num_bits - current_bitplane);

if (x == 15 && (y + coefficient_index) == 31) {
    dbgln("bit {} at {} ({} - {}), {:#x}", magnitude_bit, num_bits - current_bitplane, num_bits, current_bitplane, magnitudes[(y + coefficient_index) * w + x]);
}

                    }

                    // D7, Are there more coefficients in the magnitude refinement pass?
                    // C0, Go to the next coefficient or column
                }
            }
        }
    };

    auto cleanup_pass = [&](int current_bitplane, int pass) {
        // Cleanup pass (textual description in D.3.4 Cleanup pass)
        // XXX have a "everything is significant" bit and skip this pass when it's set? Measure.
        for (int y = 0; y < h; y += 4) {
            int y_end = min(y + 4, h);
            int num_rows = y_end - y;
            for (int x = 0; x < w; ++x) {
                // dbgln("cleanup x: {}, y: {}", x, y);

                Array<u8, 4> contexts {};
                int num_undecoded = 0;
                for (int i = 0; i < 4; ++i) {
                    contexts[i] = compute_context(x, y + i);
                    // dbgln("context[{}]: {}", i, contexts[i]);
                    if (!is_significant(x, y + i))
                        ++num_undecoded;  // XXX probably redundant since this would imply a context being non-0
                }

                // D8, Are four contiguous undecoded coefficients in a column each with a 0 context?, See D.3.4
                bool are_four_contiguous_undecoded_coefficients_in_a_column_each_with_a_0_context = num_rows == 4 && num_undecoded == 4 && (contexts[0] + contexts[1] + contexts[2] + contexts[3] == 0);
                if (are_four_contiguous_undecoded_coefficients_in_a_column_each_with_a_0_context) {
                    // C4, Run-length context label
                    auto not_four_zeros = arithmetic_decoder.get_next_bit(run_length_context);
                    // dbgln("cleanup not_four_zeros: {} (run_length_context)", not_four_zeros);

                    // D11, Are the four contiguous bits all zero?
                    bool are_the_four_contiguous_bits_all_zero = !not_four_zeros;
                    if (!are_the_four_contiguous_bits_all_zero) {
                        // C5
                        u8 first_coefficient_index = arithmetic_decoder.get_next_bit(uniform_context);
                        first_coefficient_index = (first_coefficient_index << 1) | arithmetic_decoder.get_next_bit(uniform_context);
                        // dbgln("cleanup first_coefficient_index: {} (uniform context 2x)", first_coefficient_index);
                        u8 coefficient_index = first_coefficient_index;

                        bool is_first_coefficient = true;
                        bool is_current_coefficient_significant = true;
                        set_significant(x, y + coefficient_index, true);
                        became_significant_in_pass[(y + coefficient_index) * w + x] = current_bitplane;
                        magnitudes[(y + coefficient_index) * w + x] |= 1 << (num_bits - current_bitplane); // XXX: correct?

                        do {
                            if (!is_first_coefficient) {
                                // C0, Go to the next coefficient or column
                                ++coefficient_index;

                                // C1, Decode significance bit of current coefficient (See D.3.1)
                                u8 context = compute_context(x, y + coefficient_index); // PERF: could use `contexts` cache (needs invalidation then).
                                bool is_newly_significant = arithmetic_decoder.get_next_bit(all_other_contexts[context]);
                                // dbgln("cleanup is_newly_significant: {} (context: {})", is_newly_significant, context);
                                is_current_coefficient_significant = is_newly_significant;
                                set_significant(x, y + coefficient_index, is_newly_significant);
                                if (is_newly_significant) {
                                    became_significant_in_pass[(y + coefficient_index) * w + x] = current_bitplane;
                                    magnitudes[(y + coefficient_index) * w + x] |= 1 << (num_bits - current_bitplane); // XXX: correct?
                                }
                            }
                            is_first_coefficient = false;

                            // D3, Did the current coefficient just become significant?
                            if (is_current_coefficient_significant) {
                                bool sign_bit = read_sign_bit(x, y + coefficient_index);
                                set_sign(x, y + coefficient_index, sign_bit);
                            }

                            // D10, Are there more coefficients remaining of the four column coefficients?
                        } while (coefficient_index + 1 < num_rows);
                    }
                } else {
                    u8 coefficient_index = 0;
                    bool is_first_coefficient = true;
                    do {
                        if (!is_first_coefficient) {
                            // C0, Go to the next coefficient or column
                            ++coefficient_index;
                        }
                        is_first_coefficient = false;

                        // D9, Is the coefficient significant or has the bit already been coded during the Significance Propagation coding pass?
                        // Note: The significance propagation pass is pretty similar to this loop here.
                        bool is_significant_or_coded = is_significant(x, y + coefficient_index);
                        bool has_already_been_coded = pass > 0 && was_coded_in_pass[(y + coefficient_index) * w + x] == pass - 2;
                        if (!is_significant_or_coded && !has_already_been_coded) {
                            // C1, Decode significance bit of current coefficient
                            u8 context = compute_context(x, y + coefficient_index); // PERF: could use `contexts` cache (needs invalidation then).
                            // dbgln("alt context {}", context);
                            bool is_newly_significant = arithmetic_decoder.get_next_bit(all_other_contexts[context]);
                            // dbgln("cleanup alt is_newly_significant: {} (context {})", is_newly_significant, context);
                            set_significant(x, y + coefficient_index, is_newly_significant);
                            if (is_newly_significant) {
                                became_significant_in_pass[(y + coefficient_index) * w + x] = current_bitplane;
                                magnitudes[(y + coefficient_index) * w + x] |= 1 << (num_bits - current_bitplane); // XXX: correct?
                            }

                            // D3, Did the current coefficient just become significant?
                            if (is_newly_significant) {
                                bool sign_bit = read_sign_bit(x, y + coefficient_index);
                                set_sign(x, y + coefficient_index, sign_bit);
                            }
                        }
                        // D10, Are there more coefficients remaining of the four column coefficients?
                    } while (coefficient_index + 1 < num_rows);
                }
                // D12, Are there more coefficients in the cleanup pass?
                // C0, Go to the next coefficient or column
                // (Both done by loop.)
            }
        }
    };

    // XXX make current pass a state variable too, probably?
    int& current_bitplane = state.current_bitplane; // starts at P
    int& pass = state.pass;
    // pass = 2;
    // pass = 0;
dbgln("pass on entry {}, {} passes, p {}, {} bytes", pass, current_block.number_of_coding_passes, current_block.p, current_block.length_of_data);

            // B.10.5 Zero bit-plane information
            // "If a code-block is included for the first time,
            //  [...] the number of actual bit-planes for which coding passes are generated is Mb – P
            //  [...] these missing bit-planes are all taken to be zero
            //  [...] The value of P is coded in the packet header with a separate tag tree for every precinct"
            // And Annex E, E.1 Inverse quantization procedure:
            // "Mb = G + exp_b - 1       (E-2)
            //  where the number of guard bits G and the exponent exp_b are specified in the QCD or QCC marker segments (see A.6.4 and A.6.5)."

// state.original_p stores the number of zero bitplanes.

    // for (int pass_i = 0; pass_i < current_block.number_of_coding_passes - (int)current_block.p; ++pass_i, ++pass) {
    // for (int pass_i = 0; pass_i < state.total_number_of_coding_passes - (int)state.original_p; ++pass_i, ++pass) {
    for (int pass_i = 0; pass_i < state.total_number_of_coding_passes && current_bitplane < M_b; ++pass_i, ++pass) {
    // for (int pass_i = 0; pass_i < state.total_number_of_coding_passes; ++pass_i, ++pass) {
dbgln("pass {} bitplane {}", pass, current_bitplane);

        // D0, Is this the first bit-plane for the code-block?
        switch ((pass + 2) % 3) {
        case 0:
            // dbgln("significance propagation pass");
            significance_propagation_pass(current_bitplane, pass);
            break;
        case 1:
            // dbgln("magnitude refinement pass");
            magnitude_refinement_pass(current_bitplane);
            break;
        case 2:
            // dbgln("cleanup pass");
            cleanup_pass(current_bitplane, pass);
            ++current_bitplane;
            // if (current_bitplane == num_bits - (int)state.original_p) {
            // if (current_bitplane == M_b - (int)state.original_p) {
                // done = true;
            // }
            // if (current_bitplane == M_b + 1) {
            break;
        }

                // state.reset_contexts();

    }

// (This does nothing; all blocks are independent and we won't see this block again.)
// current_block.p += current_block.number_of_coding_passes;

#if 0
{
    auto bitmap = TRY(Gfx::Bitmap::create(Gfx::BitmapFormat::BGRA8888, { w, h }));
    for (int y = 0; y < h; ++y) {
        for (int x = 0; x < w; ++x) {
            //auto pixel = bitmap->scanline(y)[x];
            auto sign = get_sign(x, y);
            auto magnitude = magnitudes[y * w + x];
            auto value = magnitude * (sign ? -1 : 1);
            //value = (value + 256) / 2;
            // dbgln("x {} y {} value {}", x, y, value);
            Color pixel;
            pixel.set_red(value);
            pixel.set_green(value);
            pixel.set_blue(value);
            pixel.set_alpha(255);
            bitmap->set_pixel(x, y, pixel);
        }
    }

    static int n_images = 0;
    auto name = TRY(String::formatted("image-{}.png", n_images++));
    auto output_stream = TRY(Core::File::open(name, Core::File::OpenMode::Write));
    auto file = TRY(Core::OutputBufferedFile::create(move(output_stream)));
    auto bytes = TRY(Gfx::PNGWriter::encode(*bitmap));
    TRY(file->write_until_depleted(bytes));
}
#endif

    return {};
}

//  [[maybe_unused]] static ErrorOr<void> save_pyramid(DecodedCoefficients const& nLL, DecodedComponent const& t0)
 [[maybe_unused]] static ErrorOr<void> save_pyramid(JPEG2000LoadingContext const& context, int component_index)
{
    int w = context.siz.width;
    int h = context.siz.height;

    auto bitmap = TRY(Gfx::Bitmap::create(Gfx::BitmapFormat::BGRA8888, { w, h }));

    for (size_t tile_index = 0; tile_index < context.decoded_tiles.size(); ++tile_index) {
        auto& t0 = context.decoded_tiles[tile_index].components[component_index];
        auto& nLL = t0.nLL;

        auto ll_rect = nLL.rect;
        // ll_rect.set_location({});
        ll_rect.set_location(t0.rect.location());

        auto store = [&](DecodedCoefficients const& coefficients, IntPoint const& location, RefPtr<Gfx::Bitmap> const& bitmap) {
            for (int y = 0; y < coefficients.rect.height(); ++y) {
                for (int x = 0; x < coefficients.rect.width(); ++x) {
                    float value = coefficients.coefficients[y * coefficients.rect.width() + x];

                    if (!context.siz.components[component_index].is_signed())
                        value += 1u << (context.siz.components[component_index].bit_depth() - 1);

                    u8 byte_value = (u8)clamp(value, 0.0f, 255.0f);
                    bitmap->set_pixel(x + location.x(), y + location.y(), Color(byte_value, byte_value, byte_value));
                }
            }
        };

        store(nLL, ll_rect.location(), bitmap);

        for (size_t i = 0; i < t0.decompositions.size(); ++i) {
            auto& decomposition = t0.decompositions[i];

            if (decomposition[0].rect.height() < ll_rect.height())
                continue;

            VERIFY(decomposition[0].rect.height() == ll_rect.height());
            VERIFY(decomposition[1].rect.width() == ll_rect.width());
            VERIFY(decomposition[2].rect.width() == decomposition[0].rect.width());
            VERIFY(decomposition[2].rect.height() == decomposition[1].rect.height());

            store(decomposition[0], { ll_rect.right(), ll_rect.top() }, bitmap);
            store(decomposition[1], { ll_rect.left(), ll_rect.bottom() }, bitmap);
            store(decomposition[2], { ll_rect.right(), ll_rect.bottom() }, bitmap);

            ll_rect.set_right(ll_rect.right() + decomposition[0].rect.width());
            ll_rect.set_bottom(ll_rect.bottom() + decomposition[1].rect.height());
        }
    }
    static int n_images = 0;
    auto name = TRY(String::formatted("image-pyramid-{}.png", n_images++));
    auto output_stream = TRY(Core::File::open(name, Core::File::OpenMode::Write));
    auto file = TRY(Core::OutputBufferedFile::create(move(output_stream)));
    auto bytes = TRY(Gfx::PNGWriter::encode(*bitmap));
    TRY(file->write_until_depleted(bytes));

    return {};
}

// F.3 Inverse discrete wavelet transformation


// "SR" is for "subband reconstruction".
static ErrorOr<DecodedCoefficients> _2D_SR(IntRect, CodingStyleParameters::Transformation transformation, DecodedCoefficients ll, DecodedCoefficients const& hl, DecodedCoefficients const& lh, DecodedCoefficients const& hh);
static ErrorOr<DecodedCoefficients> _2D_INTERLEAVE(IntRect, DecodedCoefficients ll, DecodedCoefficients const& hl, DecodedCoefficients const& lh, DecodedCoefficients const& hh);
static ErrorOr<DecodedCoefficients> HOR_SR(CodingStyleParameters::Transformation transformation, DecodedCoefficients a);
static ErrorOr<DecodedCoefficients> VER_SR(CodingStyleParameters::Transformation transformation, DecodedCoefficients a);
static void _1D_SR(CodingStyleParameters::Transformation transformation, DecodedCoefficients& a, int start, int lower, int higher, int delta);
static void _1D_EXTR(CodingStyleParameters::Transformation transformation, DecodedCoefficients& a, int start, int lower, int higher, int delta);
static void _1D_FILTR(CodingStyleParameters::Transformation transformation, DecodedCoefficients& a, int start, int lower, int higher, int delta);

// F.3.1 The IDWT procedure
[[maybe_unused]] static ErrorOr<DecodedCoefficients> IDWT(CodingStyleParameters::Transformation transformation, DecodedComponent const& component)
{
    // Figure F.3 – The IDWT procedure
    // XXX make look more like spec
    auto ll = component.nLL;

    for (auto [r_minus_1, decomposition] : enumerate(component.decompositions)) {
        auto rect = component.nLL_rects[r_minus_1];
        ll = TRY(_2D_SR(rect, transformation, move(ll), decomposition[0], decomposition[1], decomposition[2]));
        // TRY(save_pyramid(ll, component));
    }

    return ll;
}

// F.3.2 The 2D_SR procedure
static ErrorOr<DecodedCoefficients> _2D_SR(IntRect rect, CodingStyleParameters::Transformation transformation, DecodedCoefficients ll, DecodedCoefficients const& hl, DecodedCoefficients const& lh, DecodedCoefficients const& hh)
{
    // Figure F.6 – The 2D_SR procedure
    auto a = TRY(_2D_INTERLEAVE(rect, move(ll), hl, lh, hh));
    a = TRY(HOR_SR(transformation, move(a)));
    return VER_SR(transformation, move(a));
}

// F.3.3 The 2D_INTERLEAVE procedure
static ErrorOr<DecodedCoefficients> _2D_INTERLEAVE(IntRect in_rect, DecodedCoefficients ll, DecodedCoefficients const& hl, DecodedCoefficients const& lh, DecodedCoefficients const& hh)
{
    VERIFY(ll.rect.height() == hl.rect.height());
    VERIFY(ll.rect.width() == lh.rect.width());
    VERIFY(hl.rect.width() == hh.rect.width());
    VERIFY(lh.rect.height() == hh.rect.height());

    // Figure F.8 – The 2D_INTERLEAVE procedure
    // "The values of u0, u1, v0, v1 used by the 2D_INTERLEAVE procedure are those of tbx0, tbx1, tby0, tby1
    //  corresponding to sub-band b = (lev – 1)LL (see definition in Equation (B-15))."
    // XXX why are these passed in in the spec
    // XXX is this computation even right
#if 0
    int u0 = ll.rect.left();
    int v0 = ll.rect.top();
    int w = ll.rect.width() + hl.rect.width();
    int u1 = u0 + w;
    int v1 = v0 + ll.rect.height() + lh.rect.height();
#else
    int u0 = in_rect.left();
    int v0 = in_rect.top();
    int w = in_rect.width();
    int u1 = in_rect.right();
    int v1 = in_rect.bottom();
#endif

    VERIFY(in_rect.width() == ll.rect.width() + hl.rect.width());
    VERIFY(in_rect.height() == ll.rect.height() + lh.rect.height());

    DecodedCoefficients a;

    a.rect = IntRect { { u0, v0 }, { u1 - u0, v1 - v0 } }; // XXX == in_rect
    TRY(a.coefficients.try_resize(a.rect.width() * a.rect.height()));

    // Leave enough room for max expansion in _1D_EXTR.
    a.scanline_buffer.resize(max(a.rect.width(), a.rect.height()) + 8);
    a.scanline_buffer2.resize(max(a.rect.width(), a.rect.height()) + 8);

// XXX urgh origins. need to make everything...uh, u0/v0 relative, yes? ah no, divided by, uh, 2?

    {
        auto const& b = ll;
        VERIFY(ceil_div(u1, 2) - ceil_div(u0, 2) == b.rect.width());
        VERIFY(ceil_div(v1, 2) - ceil_div(v0, 2) == b.rect.height());

        for (int v_b = ceil_div(v0, 2); v_b < ceil_div(v1, 2); ++v_b) {
            for (int u_b = ceil_div(u0, 2); u_b < ceil_div(u1, 2); ++u_b) {
                a.coefficients[(2 * v_b - v0) * w + (2 * u_b - u0)] = b.coefficients[(v_b - ceil_div(v0, 2)) * b.rect.width() + (u_b - ceil_div(u0, 2))];
            }
        }
    }

    {
        auto const& b = hl;
        VERIFY(floor_div(u1, 2) - floor_div(u0, 2) == b.rect.width());
        VERIFY(ceil_div(v1, 2) - ceil_div(v0, 2) == b.rect.height());

        for (int v_b = ceil_div(v0, 2); v_b < ceil_div(v1, 2); ++v_b) {
            for (int u_b = floor_div(u0, 2); u_b < floor_div(u1, 2); ++u_b) {
                a.coefficients[(2 * v_b - v0) * w + (2 * u_b + 1 - u0)] = b.coefficients[(v_b - ceil_div(v0, 2)) * b.rect.width() + (u_b - floor_div(u0, 2))];
            }
        }
    }

    {
        auto const& b = lh;
        VERIFY(ceil_div(u1, 2) - ceil_div(u0, 2) == b.rect.width());
        VERIFY(floor_div(v1, 2) - floor_div(v0, 2) == b.rect.height());

        for (int v_b = floor_div(v0, 2); v_b < floor_div(v1, 2); ++v_b) {
            for (int u_b = ceil_div(u0, 2); u_b < ceil_div(u1, 2); ++u_b) {
                // dbgln("v_b: {} u1: {} u_b: {}", v_b, u1, u_b);
                // a.coefficients[(2 * v_b + 1) * w + 2 * u_b] = b.coefficients[v_b * b.rect.width() + u_b];
                a.coefficients[(2 * v_b + 1 - v0) * w + (2 * u_b - u0)] = b.coefficients[(v_b - floor_div(v0, 2)) * b.rect.width() + (u_b - ceil_div(u0, 2))];

            }
        }
    }

    {
        auto const& b = hh;
        VERIFY(floor_div(u1, 2) - floor_div(u0, 2) == b.rect.width());
        VERIFY(floor_div(v1, 2) - floor_div(v0, 2) == b.rect.height());

        for (int v_b = floor_div(v0, 2); v_b < floor_div(v1, 2); ++v_b) {
            for (int u_b = floor_div(u0, 2); u_b < floor_div(u1, 2); ++u_b) {
                // a.coefficients[(2 * v_b + 1) * w + 2 * u_b + 1] = b.coefficients[v_b * b.rect.width() + u_b];
                a.coefficients[(2 * v_b + 1 - v0) * w + (2 * u_b + 1 - u0)] = b.coefficients[(v_b - floor_div(v0, 2)) * b.rect.width() + (u_b - floor_div(u0, 2))];
            }
        }
    }

    return a;
}

// F.3.4 The HOR_SR procedure
static ErrorOr<DecodedCoefficients> HOR_SR(CodingStyleParameters::Transformation transformation, DecodedCoefficients a)
{
    // XXX why are these passed in in the spec
    int u0 = a.rect.left();
    int v0 = a.rect.top();
    int u1 = a.rect.right();
    int v1 = a.rect.bottom();

    // Figure F.10 – The HOR_SR procedure
    int i0 = u0;
    int i1 = u1;
    for (int v = v0; v < v1; ++v) {
        _1D_SR(transformation, a, (v - v0) * a.rect.width(), i0, i1, 1);
    }
    return a;
}

// F.3.5 The VER_SR procedure
static ErrorOr<DecodedCoefficients> VER_SR(CodingStyleParameters::Transformation transformation, DecodedCoefficients a)
{
    // XXX why are these passed in in the spec
    int u0 = a.rect.left();
    int v0 = a.rect.top();;
    int u1 = a.rect.right();
    int v1 = a.rect.bottom();

    // Figure F.12 – The VER_SR procedure
    int i0 = v0;
    int i1 = v1;
    for (int u = u0; u < u1; ++u) {
        _1D_SR(transformation, a, (u - u0), i0, i1, a.rect.width());
    }

    return a;
}

// F.3.6 The 1D_SR procedure
// Figure F.14 – The 1D_SR procedure
static void _1D_SR(CodingStyleParameters::Transformation transformation, DecodedCoefficients& a, int start, int lower, int higher, int delta)
{
    // "For signals of length one (i.e., i0 = il – 1), the 1D_SR procedure sets the value of X(i0) to Y(i0) if i0 is an even integer, and X(i0) to Y(i0)/2 if i0 is an odd integer."
    if (lower == higher - 1) {
        if (lower % 2 == 0)
            a.coefficients[start] = a.coefficients[start];
        else
            a.coefficients[start] = a.coefficients[start] / 2;
        return;
    }

    // Figure F.14 – The 1D_SR procedure
    _1D_EXTR(transformation, a, start, lower, higher, delta);
    _1D_FILTR(transformation, a, start, lower, higher, delta);
}


// F.3.7 The 1D_EXTR procedure
static void _1D_EXTR(CodingStyleParameters::Transformation transformation, DecodedCoefficients& a, int start, int lower, int higher, int delta)
{
    // Table F.2 – Extension to the left
    int i_left;
    if (transformation == CodingStyleParameters::Transformation::Reversible_5_3_Filter) {
        i_left = lower % 2 == 0 ? 1 : 2;
    } else {
        VERIFY(transformation == CodingStyleParameters::Transformation::Irreversible_9_7_Filter);
        i_left = lower % 2 == 0? 3 : 4;
    }

    // Table F.3 – Extension to the right
    int i_right;
    if (transformation == CodingStyleParameters::Transformation::Reversible_5_3_Filter) {
        i_right = higher % 2 == 0 ? 1 : 2;
    } else {
        VERIFY(transformation == CodingStyleParameters::Transformation::Irreversible_9_7_Filter);
        i_right = higher % 2 == 0 ? 3 : 4;
    }

    // (F-4)
    // PSE is short for "Period Symmetric Extension".
    auto PSE = [](int i, int i0, int i1) {
        auto mod = [](int a, int b) {
            return (a % b + b) % b;
        };
        return i0 + min(mod(i - i0, 2 * (i1 - i0 - 1)), 2 * (i1 - i0 - 1) - mod(i - i0, 2 *(i1 - i0 - 1)));
    };

    for (int l = lower - i_left, i = 0; l < higher + i_right; ++l, ++i) {
        // XXX really `- lower` here? probably?
        a.scanline_buffer[i] = a.coefficients[start + (PSE(l, lower, higher) - lower) * delta];
    }
    a.scanline_start = i_left;
}

// F.3.8 The 1D_FILTR procedure
static void _1D_FILTR(CodingStyleParameters::Transformation transformation, DecodedCoefficients& a, int start, int lower, int higher, int delta)
{
    int i0 = lower;
    int i1 = higher;

    auto y_ext = [&](int i) {
        return a.scanline_buffer[i + a.scanline_start - lower];
    };

    auto x = [&](int i) -> float& {
        return a.scanline_buffer2[i + a.scanline_start - lower];
    };

    if (transformation == CodingStyleParameters::Transformation::Reversible_5_3_Filter) {
        // F.3.8.1 The 1D_FILTR_5-3R procedure
        // (F-5)
        for (int n = floor_div(i0, 2); n < floor_div(i1, 2) + 1; ++n)
            x(2 * n) = y_ext(2 * n) - floorf((y_ext(2 * n - 1) + y_ext(2 * n + 1) + 2) / 4.0f);

        // (F-6)
        for (int n = floor_div(i0, 2); n < floor_div(i1, 2); ++n)
            x(2 * n + 1) = y_ext(2 * n + 1) + floorf((x(2 * n) + x(2 * n + 2)) / 2.0f);
    } else {
        VERIFY(transformation == CodingStyleParameters::Transformation::Irreversible_9_7_Filter);

        // Table F.4 – Definition of lifting parameters for the 9-7 irreversible filter
        constexpr float alpha = -1.586'134'342'059'924f;
        constexpr float beta = -0.052'980'118'572'961f;
        constexpr float gamma = 0.882'911'075'530'934f;
        constexpr float delta = 0.443'506'852'043'971f;
        constexpr float kappa = 1.230'174'104'914'001f;

        // F.3.8.2 The 1D_FILTR_9-7I procedure
        // "Firstly, step 1 is performed for all values of n such that..."
        for (int n = floor_div(i0, 2) - 1; n < floor_div(i1, 2) + 2; ++n)
            x(2 * n) = kappa * y_ext(2 * n); // [STEP1]

        // "and step 2 is performed for all values of n such that..."
        for (int n = floor_div(i0, 2) - 2; n < floor_div(i1, 2) + 2; ++n)
            x(2 * n + 1) = (1 / kappa) * y_ext(2 * n + 1); // [STEP2]

        for (int n = floor_div(i0, 2) - 1; n < floor_div(i1, 2) + 2; ++n)
            x(2 * n) = x(2 * n) - delta * (x(2 * n - 1) + x(2 * n + 1)); // [STEP3]

        for (int n = floor_div(i0, 2) - 1; n < floor_div(i1, 2) + 1; ++n)
            x(2 * n + 1) = x(2 * n + 1) - gamma * (x(2 * n) + x(2 * n + 2)); // [STEP4]

        for (int n = floor_div(i0, 2); n < floor_div(i1, 2) + 1; ++n)
            x(2 * n) = x(2 * n) - beta * (x(2 * n - 1) + x(2 * n + 1)); // [STEP5]

        for (int n = floor_div(i0, 2); n < floor_div(i1, 2); ++n)
            x(2 * n + 1) = x(2 * n + 1) - alpha * (x(2 * n) + x(2 * n + 2)); // [STEP6]
    }

    for (int i = lower; i < higher; ++i)
        a.coefficients[start + (i - lower) * delta] = x(i);
}

static ErrorOr<void> decode_image(JPEG2000LoadingContext& context)
{
    TRY(parse_codestream_tile_headers(context));

// XXX these are component-tiles; rename
    for (auto& tile : context.tiles)
        TRY(decode_tile(context, tile));

    // XXX put bitplane to coefficient conversion here
    // ...maybe even do bitplane decoding here?

    // Per component:
    // - combined code-block images at each resolution level, storing all sub-bands
    // - bits from all layers combined
    // Maybe do this per-tile and then combine tiles after the fact?
    // Also, precincts.

    // IDWT
#if 0
    for (auto& tile : context.tiles) {
        for (auto [component_index, component] : enumerate(context.decoded_tiles[tile.index].components)) {

            TRY(save_pyramid(context.decoded_tiles[tile.index].components[component_index].nLL, context.decoded_tiles[tile.index].components[component_index]));


dbgln("idwt for tile {} component {}", tile.index, component_index);
            auto transformation = coding_style_parameters_for_component(context, tile, component_index).transformation;
            component.nLL = TRY(IDWT(transformation, component));
        }
    }
#else
    for (size_t component_index = 0; component_index < context.siz.components.size(); ++component_index) {
        // XXX instead of this, take max over all tile components since this can vary per tile and per component
        TRY(save_pyramid(context, component_index));
        for (size_t r_minus_1 = 0; r_minus_1 < context.cod.parameters.number_of_decomposition_levels; ++r_minus_1) {
            for (size_t tile_index = 0; tile_index < context.tiles.size(); ++tile_index) {
                auto& tile = context.tiles[tile_index];
                auto& component = context.decoded_tiles[tile.index].components[component_index];

                if (r_minus_1 >= component.decompositions.size())
                    continue;

                auto& decomposition = component.decompositions[r_minus_1];

dbgln("idwt for tile {} component {}", tile.index, component_index);
                auto transformation = coding_style_parameters_for_component(context, tile, component_index).transformation;

                auto rect = component.nLL_rects[r_minus_1];
                component.nLL = TRY(_2D_SR(rect, transformation, move(component.nLL), decomposition[0], decomposition[1], decomposition[2]));
            }
            TRY(save_pyramid(context, component_index));
        }
    }
#endif

    // Figure G.1 – Placement of the DC level shifting with component transformation
    for (auto& tile : context.tiles) {
        if (tile.cod.value_or(context.cod).multiple_component_transformation_type == CodingStyleDefault::MultipleComponentTransformationType::MultipleComponentTransformationUsed) {
            auto& decoded_tile = context.decoded_tiles[tile.index];
            if (decoded_tile.components.size() < 3)
                return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Multiple component transformation type but fewer than 3 components");

            // XXX reversible / irreversible depends on wavelet type, which is per component?!
            if (tile.cod.value_or(context.cod).parameters.transformation == CodingStyleParameters::Transformation::Reversible_5_3_Filter) {
                // G.2 Reversible multiple component transformation (RCT)
                // "The three components input into the RCT shall have the same separation on the reference grid and the same bit-depth."
                // XXX check
                auto& c0 = decoded_tile.components[0].nLL;
                auto& c1 = decoded_tile.components[1].nLL;
                auto& c2 = decoded_tile.components[2].nLL;
                int w = c0.rect.width();
                // XXX verify components have same dimensions
                for (int y = 0; y < c0.rect.height(); ++y) {
                    for (int x = 0; x < w; ++x) {
                        float Y = c0.coefficients[y * w + x];
                        float Cb = c1.coefficients[y * w + x];
                        float Cr = c2.coefficients[y * w + x];

                        float G = Y - floorf((Cb + Cr) / 4); // (G-6)
                        float R = Cr + G; // (G-7)
                        float B = Cb + G; // (G-8)

                        c0.coefficients[y * w + x] = R;
                        c1.coefficients[y * w + x] = G;
                        c2.coefficients[y * w + x] = B;
                    }
                }
            } else {
                VERIFY(context.cod.parameters.transformation == CodingStyleParameters::Transformation::Irreversible_9_7_Filter);
                // G.3 Irreversible multiple component transformation (ICT)
                // "The three components input into the ICT shall have the same separation on the reference grid and the same bit-depth."
                // XXX check
                auto& c0 = decoded_tile.components[0].nLL;
                auto& c1 = decoded_tile.components[1].nLL;
                auto& c2 = decoded_tile.components[2].nLL;
                int w = c0.rect.width();
                // XXX verify components have same dimensions
                for (int y = 0; y < c0.rect.height(); ++y) {
                    for (int x = 0; x < w; ++x) {
                        float Y = c0.coefficients[y * w + x];
                        float Cb = c1.coefficients[y * w + x];
                        float Cr = c2.coefficients[y * w + x];

                        float R = Y + 1.402f * Cr; // (G-12)
                        float G = Y - 0.34413f * Cb - 0.7141f * Cr; // (G-13)
                        float B = Y + 1.772f * Cb; // (G-14)

                        c0.coefficients[y * w + x] = R;
                        c1.coefficients[y * w + x] = G;
                        c2.coefficients[y * w + x] = B;
                    }
                }
            }
        }
    }

    // DC level shift
    // G.1.2 Inverse DC level shifting of tile-components
    // (G-2)
    // XXX - 1 or not here? -1 looks better and matches spec text in G.1.2 but maybe not Table A.11 in A.5.1 (?)
    for (auto& tile : context.tiles) {
        auto& decoded_tile = context.decoded_tiles[tile.index];
        VERIFY(context.siz.components.size() == decoded_tile.components.size());

        for (auto [component_index, component] : enumerate(decoded_tile.components)) {
            if (!context.siz.components[component_index].is_signed()) {
                for (auto& coefficient : component.nLL.coefficients)
                    coefficient += 1u << (context.siz.components[component_index].bit_depth() - 1);
            }

            // Convert to 8bpp.
            // XXX: Don't do this for files with palette.
            if (context.siz.components[component_index].bit_depth() != 8) {
                for (auto& coefficient : component.nLL.coefficients)
                    coefficient = 255 * coefficient / (1u << context.siz.components[component_index].bit_depth());
                    // coefficient = 255 * coefficient / ((1u << (context.siz.components[component_index].bit_depth() + 1)) - 1);
            }
        }
    }

    auto bitmap = TRY(Gfx::Bitmap::create(Gfx::BitmapFormat::BGRA8888, { context.siz.width, context.siz.height }));
    for (size_t tile_index = 0; tile_index < context.decoded_tiles.size(); ++tile_index) {
        auto& decoded_tile = context.decoded_tiles[tile_index];

        // XXX per-component subsampling
        VERIFY(decoded_tile.components[0].rect.size() == decoded_tile.components[0].nLL.rect.size());
        int w = decoded_tile.components[0].nLL.rect.width();
        int h = decoded_tile.components[0].nLL.rect.height();
        for (int y = 0; y < h; ++y) {
            for (int x = 0; x < w; ++x) {
                float value = decoded_tile.components[0].nLL.coefficients[y * w + x];

                u8 byte_value = round_to<u8>(clamp(value, 0.0f, 255.0f));
                u8 r = byte_value;
                u8 g = byte_value;
                u8 b = byte_value;
                u8 a = 255;

                // FIXME: look at component configuration
                if (context.decoded_tiles[0].components.size() == 3) {
                    g = round_to<u8>(clamp(decoded_tile.components[1].nLL.coefficients[y * w + x], 0.0f, 255.0f));
                    b = round_to<u8>(clamp(decoded_tile.components[2].nLL.coefficients[y * w + x], 0.0f, 255.0f));
                }
                else if (context.decoded_tiles[0].components.size() == 4) {
                    g = round_to<u8>(clamp(decoded_tile.components[1].nLL.coefficients[y * w + x], 0.0f, 255.0f));
                    b = round_to<u8>(clamp(decoded_tile.components[2].nLL.coefficients[y * w + x], 0.0f, 255.0f));
                    a = round_to<u8>(clamp(decoded_tile.components[3].nLL.coefficients[y * w + x], 0.0f, 255.0f));
                }

                //value = (value + 256) / 2;
                // dbgln("x {} y {} value {}", x, y, value);
                Color pixel;
                pixel.set_red(r);
                pixel.set_green(g);
                pixel.set_blue(b);
                pixel.set_alpha(a);
                bitmap->set_pixel(x + decoded_tile.components[0].rect.left(), y + decoded_tile.components[0].rect.top(), pixel);
            }
        }
    }

    context.bitmap = move(bitmap);

    return {};
}

bool JPEG2000ImageDecoderPlugin::sniff(ReadonlyBytes data)
{
    return data.starts_with(jp2_id_string) || data.starts_with(marker_id_string);
}

JPEG2000ImageDecoderPlugin::JPEG2000ImageDecoderPlugin()
{
    m_context = make<JPEG2000LoadingContext>();
}

JPEG2000ImageDecoderPlugin::~JPEG2000ImageDecoderPlugin() = default;

IntSize JPEG2000ImageDecoderPlugin::size()
{
    return m_context->size;
}

ErrorOr<NonnullOwnPtr<ImageDecoderPlugin>> JPEG2000ImageDecoderPlugin::create(ReadonlyBytes data)
{
    auto plugin = TRY(adopt_nonnull_own_or_enomem(new (nothrow) JPEG2000ImageDecoderPlugin()));
    TRY(decode_jpeg2000_header(*plugin->m_context, data));
    return plugin;
}

ErrorOr<ImageFrameDescriptor> JPEG2000ImageDecoderPlugin::frame(size_t index, Optional<IntSize>)
{
    if (index != 0)
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Invalid frame index");

    if (m_context->state == JPEG2000LoadingContext::State::Error)
        return Error::from_string_literal("JPEG2000ImageDecoderPlugin: Decoding failed");

    if (m_context->state < JPEG2000LoadingContext::State::ImageDecoded) {
        TRY(decode_image(*m_context));
        m_context->state = JPEG2000LoadingContext::State::ImageDecoded;
    }

    return ImageFrameDescriptor { m_context->bitmap, 0 };
}

ErrorOr<Optional<ReadonlyBytes>> JPEG2000ImageDecoderPlugin::icc_data()
{
    return m_context->icc_data;
}

NaturalFrameFormat JPEG2000ImageDecoderPlugin::natural_frame_format() const
{
    if (m_context->state == JPEG2000LoadingContext::State::Error)
        return NaturalFrameFormat::RGB;

    if (m_context->siz.components.size() == 1)
        return NaturalFrameFormat::Grayscale;

    // XXX look at component configuration.
    // XXX not clear what happens for CMYK that has CMYK profile :thonk:
    // if (m_context->components.size() == 4)
        // return NaturalFrameFormat::CMYK;

    return NaturalFrameFormat::RGB;
}

}
