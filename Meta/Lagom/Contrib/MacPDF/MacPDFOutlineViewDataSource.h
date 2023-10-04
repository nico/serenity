/*
 * Copyright (c) 2023, Nico Weber <thakis@chromium.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include "CocoaWrapper.h"

#include <LibPDF/Document.h>

@interface MacPDFOutlineViewDataSource : NSObject <NSOutlineViewDataSource>

- (instancetype)initWithOutline:(RefPtr<PDF::OutlineDict>)outline;

@end
