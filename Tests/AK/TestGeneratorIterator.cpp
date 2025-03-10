/*
 * Copyright (c) 2025, Nico Weber <thakis@chromium.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/GeneratorIterator.h>
#include <LibTest/TestCase.h>

static Generator<int, Empty> generate_ints()
{
    co_yield 1;
    co_yield 2;
    co_return {};
}

TEST_CASE(sync_order)
{
    Vector<int> order;

    GeneratorIterator it = generate_ints();
    EXPECT(it.has_next());
    EXPECT_EQ(it.next(), 1);

    EXPECT(it.has_next());
    EXPECT_EQ(it.next(), 2);

    EXPECT(!it.has_next());
}
