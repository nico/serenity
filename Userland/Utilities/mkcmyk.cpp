/*
 * Copyright (c) 2025, Nico Weber <thakis@chromium.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibCore/File.h>
#include <LibCore/MappedFile.h>
#include <LibGfx/Bitmap.h>
#include <LibGfx/ImageFormats/BMPWriter.h>
#include <LibMain/Main.h>

static ErrorOr<void> save_image(Gfx::Bitmap& frame, StringView out_path)
{
    auto stream = [out_path]() -> ErrorOr<NonnullOwnPtr<Core::OutputBufferedFile>> {
        auto output_stream = TRY(Core::File::open(out_path, Core::File::OpenMode::Write));
        return Core::OutputBufferedFile::create(move(output_stream));
    };


    ByteBuffer bytes = TRY(Gfx::BMPWriter::encode(frame));
    TRY(TRY(stream())->write_until_depleted(bytes));

    return {};
}

ErrorOr<int> serenity_main(Main::Arguments)
{
    auto frame = TRY(Gfx::Bitmap::create(Gfx::BitmapFormat::BGRA8888, { 3, 2 }));

    frame->set_pixel(0, 0, { 0, 0, 0, 0 });
    frame->set_pixel(1, 0, { 0, 0, 0, 255 });
    frame->set_pixel(2, 0, { 255, 255, 255, 0 });

    frame->set_pixel(0, 1, { 255, 0, 0, 0 });
    frame->set_pixel(1, 1, { 0, 255, 0, 0 });
    frame->set_pixel(2, 1, { 0, 0, 255, 0 });

    TRY(save_image(frame, "out.bmp"sv));

    return 0;
}

