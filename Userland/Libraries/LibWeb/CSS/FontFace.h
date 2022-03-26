/*
 * Copyright (c) 2022, Nico Weber <thakis@chromium.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/RefCounted.h>
#include <AK/String.h>

namespace Web::CSS {

class FontFace : public RefCounted<FontFace> {
    friend class Parser;

public:
    static NonnullRefPtr<FontFace> create() { return adopt_ref(*new FontFace); }

private:
    String m_font_face;
};

}
