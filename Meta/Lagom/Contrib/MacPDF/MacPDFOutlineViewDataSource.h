/*
 * Copyright (c) 2023, Nico Weber <thakis@chromium.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include "CocoaWrapper.h"

#include <LibPDF/Document.h>

// Objective-C wrapper of PDF::OutlineItem, to launder it through the NSOutlineViewDataSource protocol.
@interface OutlineItemWrapper : NSObject
{
    // NonnullRefPtr really, but Objective-C objects cannot be initialized with that.
@public
    RefPtr<PDF::OutlineItem> _item;
}
- (instancetype)initWithItem:(NonnullRefPtr<PDF::OutlineItem>)item;
@end

@interface MacPDFOutlineViewDataSource : NSObject <NSOutlineViewDataSource>

- (instancetype)initWithOutline:(RefPtr<PDF::OutlineDict>)outline;

@end
