/*
 * Copyright (c) 2023, Nico Weber <thakis@chromium.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include "CocoaWrapper.h"

@interface MacPDFOutlineViewDataSource : NSObject <NSOutlineViewDataSource>

@property (nonatomic, strong) NSMutableArray<NSMutableDictionary *> *data;

@end
