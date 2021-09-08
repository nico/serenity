/*
 * Copyright (c) 2021, Nico Weber <thakis@chromium.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/Types.h>
#include <Kernel/Prekernel/Arch/aarch64/MainIdRegister.h>
#include <Kernel/StdLib.h>

extern "C" [[noreturn]] void init();
extern "C" [[noreturn]] void init()
{
    auto mir = Prekernel::MainIdRegister::the();

    auto a [[maybe_unused]] = mir.implementer();
    auto i [[maybe_unused]] = mir.architecture();
    auto p [[maybe_unused]] = mir.part_num();

    for (;;) { }
}

// From Kernel/Prekernel/init.cpp
extern size_t __stack_chk_guard;
size_t __stack_chk_guard;
void* __dso_handle __attribute__((visibility("hidden")));

[[noreturn]] static void halt()
{
    for (;;) { }
    __builtin_unreachable();
}

extern "C" [[noreturn]] void __stack_chk_fail() __attribute__((used));
void __stack_chk_fail()
{
    halt();
}

void __assertion_failed(char const*, char const*, unsigned int, char const*)
{
    halt();
}

// From Kernel/StdLib.cpp
extern "C" {

//const void* memmem(const void* haystack, size_t haystack_length, const void* needle, size_t needle_length)
//{
//return AK::memmem(haystack, haystack_length, needle, needle_length);
//}

size_t strnlen(const char* str, size_t maxlen)
{
    size_t len = 0;
    for (; len < maxlen && *str; str++)
        len++;
    return len;
}

int strcmp(const char* s1, const char* s2)
{
    for (; *s1 == *s2; ++s1, ++s2) {
        if (*s1 == 0)
            return 0;
    }
    return *(const u8*)s1 < *(const u8*)s2 ? -1 : 1;
}

int memcmp(const void* v1, const void* v2, size_t n)
{
    auto* s1 = (const u8*)v1;
    auto* s2 = (const u8*)v2;
    while (n-- > 0) {
        if (*s1++ != *s2++)
            return s1[-1] < s2[-1] ? -1 : 1;
    }
    return 0;
}

int strncmp(const char* s1, const char* s2, size_t n)
{
    if (!n)
        return 0;
    do {
        if (*s1 != *s2++)
            return *(const unsigned char*)s1 - *(const unsigned char*)--s2;
        if (*s1++ == 0)
            break;
    } while (--n);
    return 0;
}

char* strstr(const char* haystack, const char* needle)
{
    char nch;
    char hch;

    if ((nch = *needle++) != 0) {
        size_t len = strlen(needle);
        do {
            do {
                if ((hch = *haystack++) == 0)
                    return nullptr;
            } while (hch != nch);
        } while (strncmp(haystack, needle, len) != 0);
        --haystack;
    }
    return const_cast<char*>(haystack);
}

// Functions that are automatically called by the C++ compiler.
// Declare them first, to tell the silly compiler that they are indeed being used.
//[[noreturn]] void __stack_chk_fail() __attribute__((used));
[[noreturn]] void __stack_chk_fail_local() __attribute__((used));
extern "C" int __cxa_atexit(void (*)(void*), void*, void*);
[[noreturn]] void __cxa_pure_virtual();

//[[noreturn]] void __stack_chk_fail()
//{
//VERIFY_NOT_REACHED();
//}

[[noreturn]] void __stack_chk_fail_local()
{
    VERIFY_NOT_REACHED();
}

extern "C" int __cxa_atexit(void (*)(void*), void*, void*)
{
    VERIFY_NOT_REACHED();
    return 0;
}

[[noreturn]] void __cxa_pure_virtual()
{
    VERIFY_NOT_REACHED();
}
}
