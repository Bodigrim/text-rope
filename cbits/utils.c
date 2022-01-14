/*
 * Copyright (c) 2021 Andrew Lelechenko <andrew.lelechenko@gmail.com>
 */

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#ifdef __x86_64__
#include <emmintrin.h>
#include <xmmintrin.h>
#endif

ssize_t _hs_text_lines_memchr0A(const void *arr, size_t off, size_t len)
{
  const void *ptr = memchr(arr + off, 0x0A, len);
  return ptr == NULL ? -1 : ptr - (arr + off);
}

ssize_t _hs_text_lines_length_utf8_as_utf16(const uint8_t *arr, size_t off, size_t len)
{
  const uint8_t *src = arr + off;
  const uint8_t *srcend = src + len;
  ssize_t ret = 0;

  while (src < srcend){

#ifdef __x86_64__
    if (src < srcend - 15){
      __m128i w128 = _mm_loadu_si128((__m128i *)src);
      // Which bytes are < 128?
      uint16_t mask = _mm_movemask_epi8(w128);
      if (!mask){
        src+=16;
        ret+=16;
        continue;
      }
    }
#endif

    if(src < srcend - 7){
      uint64_t w64;
      memcpy(&w64, src, sizeof(uint64_t));
      if (!(w64 & 0x8080808080808080ULL)){
        src+=8;
        ret+=8;
        continue;
      }
    }

    uint8_t leadByte = *src++;
    if (leadByte < 0x80){
      ret++;
    } else if (leadByte < 0xe0){
      ret++;
      src++;
    } else if (leadByte < 0xf0){
      ret++;
      src+=2;
    } else {
      ret+=2;
      src+=3;
    }
  }

  return ret;
}

ssize_t _hs_text_lines_take_utf8_as_utf16(const uint8_t *arr, size_t off, size_t len, size_t cnt)
{
  const uint8_t *src = arr + off;
  const uint8_t *srcend = src + len;

  while (src < srcend && cnt > 0){

#ifdef __x86_64__
    if (src < srcend - 15 && cnt >= 16){
      __m128i w128 = _mm_loadu_si128((__m128i *)src);
      // Which bytes are < 128?
      uint16_t mask = _mm_movemask_epi8(w128);
      if (!mask){
        src+=16;
        cnt-=16;
        continue;
      }
    }
#endif

    if(src < srcend - 7 && cnt >= 8){
      uint64_t w64;
      memcpy(&w64, src, sizeof(uint64_t));
      if (!(w64 & 0x8080808080808080ULL)){
        src+=8;
        cnt-=8;
        continue;
      }
    }

    uint8_t leadByte = *src++;
    if (leadByte < 0x80){
      cnt--;
    } else if (leadByte < 0xe0){
      cnt--;
      src++;
    } else if (leadByte < 0xf0){
      cnt--;
      src+=2;
    } else {
      if(cnt < 2) return -1;
      cnt-=2;
      src+=3;
    }
  }

  return src - arr - off;
}
