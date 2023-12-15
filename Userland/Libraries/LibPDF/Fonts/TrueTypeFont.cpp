/*
 * Copyright (c) 2022, Matthew Olsson <mattco@serenityos.org>
 * Copyright (c) 2022, Julian Offenhäuser <offenhaeuser@protonmail.com>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibGfx/Font/OpenType/Font.h>
#include <LibGfx/Font/ScaledFont.h>
#include <LibGfx/Painter.h>
#include <LibPDF/CommonNames.h>
#include <LibPDF/Fonts/TrueTypeFont.h>
#include <LibPDF/Renderer.h>

namespace PDF {

PDFErrorOr<void> TrueTypeFont::initialize(Document* document, NonnullRefPtr<DictObject> const& dict)
{
    TRY(SimpleFont::initialize(document, dict));

    m_base_font_name = TRY(dict->get_name(document, CommonNames::BaseFont))->name();

    // If there's an embedded font program we use that; otherwise we try to find a replacement font
    if (dict->contains(CommonNames::FontDescriptor)) {
        auto descriptor = MUST(dict->get_dict(document, CommonNames::FontDescriptor));
        if (descriptor->contains(CommonNames::FontFile2)) {
            auto font_file_stream = TRY(descriptor->get_stream(document, CommonNames::FontFile2));
            m_font = TRY(OpenType::Font::try_load_from_externally_owned_memory(font_file_stream->bytes()));
        }
    }
    if (!m_font) {
        m_font = TRY(replacement_for(base_font_name().to_lowercase()));
    }

    VERIFY(m_font);
    return {};
}

Optional<float> TrueTypeFont::get_glyph_width(u8 char_code) const
{
    u8 code_point = char_code;
//    return m_font->glyph_width(char_code);

//    auto id = glyph_id_for_code_point(code_point);
    auto id = m_font->glyph_id_for_code_point(code_point);

//    auto metrics = glyph_metrics(id);
    float font_size = 12; // XXX pass in
    float units_per_em = m_font->units_per_em();
    float scale = (font_size * DEFAULT_DPI) / (POINTS_PER_INCH * units_per_em);
    auto metrics = m_font->glyph_metrics(id, scale, scale, font_size, font_size);

    return metrics.advance_width;
}

void TrueTypeFont::set_font_size(float)
{
    //m_font = m_font->with_size((font_size * POINTS_PER_INCH) / DEFAULT_DPI);
}

PDFErrorOr<void> TrueTypeFont::draw_glyph(Gfx::Painter& painter, Gfx::FloatPoint point, float width, u8 char_code, Renderer const& renderer)
{
    #if 0
    auto style = renderer.state().paint_style;

    // Undo shift in Glyf::Glyph::append_simple_path() via OpenType::Font::rasterize_glyph().
    auto position = point.translated(0, -m_font->pixel_metrics().ascent);

    if (style.has<Color>()) {
        painter.draw_glyph(position, char_code, *m_font, style.get<Color>());
    } else {
        // FIXME: Bounding box and sample point look to be pretty wrong
        style.get<NonnullRefPtr<Gfx::PaintStyle>>()->paint(Gfx::IntRect(position.x(), position.y(), width, 0), [&](auto sample) {
            painter.draw_glyph(position, char_code, *m_font, sample(Gfx::IntPoint(position.x(), position.y())));
        });
    }
    return {};
    #endif

    auto style = renderer.state().paint_style;

#if 0
    if (!m_font_program) {
        // Account for the reversed font baseline
        auto position = point.translated(0, -m_font->baseline());
        // FIXME: Bounding box and sample point look to be pretty wrong
        if (style.has<Color>()) {
            painter.draw_glyph(position, char_code, *m_font, style.get<Color>());
        } else {
            style.get<NonnullRefPtr<Gfx::PaintStyle>>()->paint(Gfx::IntRect(position.x(), position.y(), width, 0), [&](auto sample) {
                painter.draw_glyph(position, char_code, *m_font, sample(Gfx::IntPoint(position.x(), position.y())));
            });
        }
        return {};
    }
#endif

    float font_size = 12; // XXX pass in
    float units_per_em = m_font->units_per_em();
    float scale = (font_size * DEFAULT_DPI) / (POINTS_PER_INCH * units_per_em);

#if 0
    // FIXME: Possibly do this one day?
    auto effective_encoding = encoding();
    if (!effective_encoding)
        effective_encoding = m_font_program->encoding();
    if (!effective_encoding)
        effective_encoding = Encoding::standard_encoding();
    auto char_name = effective_encoding->get_name(char_code);
    auto translation = m_font_program->glyph_translation(char_name, width);
    point = point.translated(translation);
#endif
    u8 code_point = char_code;
    //auto id = glyph_id_for_code_point(code_point);
    auto id = m_font->glyph_id_for_code_point(code_point);

    // XXX glyph caching
    auto glyph_position = Gfx::GlyphRasterPosition::get_nearest_fit_for(point);
//    Type1GlyphCacheKey index { char_code, glyph_position.subpixel_offset, width };

    RefPtr<Gfx::Bitmap> bitmap;
//    auto maybe_bitmap = m_glyph_cache.get(index);
  //  if (maybe_bitmap.has_value()) {
    //    bitmap = maybe_bitmap.value();
    //} else {
//        bitmap = m_font_program->rasterize_glyph(char_name, width, glyph_position.subpixel_offset);
     bitmap = m_font->rasterize_glyph(id, scale, scale, glyph_position.subpixel_offset);
(void)width;
      //  m_glyph_cache.set(index, bitmap);
    //}

    if (style.has<Color>()) {
        painter.blit_filtered(glyph_position.blit_position, *bitmap, bitmap->rect(), [style](Color pixel) -> Color {
            return pixel.multiply(style.get<Color>());
        });
    } else {
        style.get<NonnullRefPtr<Gfx::PaintStyle>>()->paint(bitmap->physical_rect(), [&](auto sample) {
            painter.blit_filtered(glyph_position.blit_position, *bitmap, bitmap->rect(), [&](Color pixel) -> Color {
                // FIXME: Presumably we need to sample at every point in the glyph, not just the top left?
                return pixel.multiply(sample(glyph_position.blit_position));
            });
        });
    }
    return {};
}

}
