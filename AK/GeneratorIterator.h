/*
 * Copyright (c) 2025, Nico Weber <thakis@chromium.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/Generator.h>

namespace AK {

template<typename T>
class GeneratorIterator {
public:
    GeneratorIterator(Generator<T, Empty> generator)
        : m_generator(move(generator))
    {
        generate_next();
    }

    bool has_next() const { return m_has_next; }

    T next()
    {
        auto result = move(m_next);
        generate_next();
        return result;
    }

private:
    Generator<T, Empty> m_generator;
    T m_next {};
    bool m_has_next { true };

    void generate_next()
    {
        auto coroutine = m_generator.next();
        VERIFY(coroutine.await_ready());
        coroutine.await_resume().visit(
            [&](T data) {
                m_next = move(data);
                m_has_next = true;
            },
            [&](Empty) {
                m_has_next = false;
            });
    }
};

}

#ifdef USING_AK_GLOBALLY
using AK::GeneratorIterator;
#endif
