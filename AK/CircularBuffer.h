/*
 * Copyright (c) 2022, Lucas Chollet <lucas.chollet@free.fr>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/ByteBuffer.h>
#include <AK/Error.h>
#include <AK/Noncopyable.h>

namespace AK {

class CircularBuffer {
    AK_MAKE_NONCOPYABLE(CircularBuffer);

public:
    static ErrorOr<CircularBuffer> create_empty(size_t size);
    static ErrorOr<CircularBuffer> create_initialized(ByteBuffer);

    CircularBuffer(CircularBuffer&& other) = default;
    CircularBuffer& operator=(CircularBuffer&& other) = default;

    ~CircularBuffer() = default;

    size_t write(ReadonlyBytes bytes);

    void write(u8 byte)
    {
        auto next_span = next_write_span();
        if (next_span.size() == 0)
            return;

        *next_span.data() = byte;

        m_used_space += 1;

        m_seekback_limit += 1;
        if (m_seekback_limit > capacity())
            m_seekback_limit = capacity();
    }

    Bytes read(Bytes bytes);
    ErrorOr<void> discard(size_t discarded_bytes);
    ErrorOr<size_t> fill_from_stream(Stream&);

    /// Compared to `read()`, this starts reading from an offset that is `distance` bytes
    /// before the current write pointer and allows for reading already-read data.
    ErrorOr<Bytes> read_with_seekback(Bytes bytes, size_t distance);

    ErrorOr<void> copy_from_seekback(size_t distance, size_t length);

    [[nodiscard]] size_t empty_space() const;
    [[nodiscard]] size_t used_space() const;
    [[nodiscard]] size_t capacity() const
    {
        return m_buffer.size();
    }

    [[nodiscard]] size_t seekback_limit() const;

    Optional<size_t> offset_of(StringView needle, Optional<size_t> from = {}, Optional<size_t> until = {}) const;

    void clear();

private:
    CircularBuffer(ByteBuffer);

    [[nodiscard]] bool is_wrapping_around() const
    {
        return capacity() <= m_reading_head + m_used_space;
    }

    [[nodiscard]] Bytes next_write_span()
    {
        //if (is_wrapping_around())
        if (m_buffer.size() <= m_reading_head + m_used_space)
            return m_buffer.span().slice(m_reading_head + m_used_space - capacity(), capacity() - m_used_space);
        return m_buffer.span().slice(m_reading_head + m_used_space, capacity() - (m_reading_head + m_used_space));
    }

    [[nodiscard]] ReadonlyBytes next_read_span() const;
    [[nodiscard]] ReadonlyBytes next_read_span_with_seekback(size_t distance) const;

    ByteBuffer m_buffer {};

    size_t m_reading_head {};
    size_t m_used_space {};
    size_t m_seekback_limit {};
};

}
