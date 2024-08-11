//////
// Intro

// Endian specific access types

// (c) 2011..2020 Sigrid/Jasper Neumann
// www.sirrida.de / programming.sirrida.de
// E-Mail: info@sirrida.de

// Granted to the public domain
// First version: 2013-02-14
// Last change: 2014-02-11

// Compile with
// Gnu C: g++ endian.cpp

#include "general.c"

t_32u bswap_32(t_32u q) {
  q = ((q & 0x00ff00ff) << 8) | ((q >> 8) & 0x00ff00ff);
  return (q << 16) | (q >> 16);
  }

#pragma pack(push,1)  // force byte packed structures

class tr_intel_32u {
// Little endian, Intel byte order, least significant byte first
private:
  t_32u mem;

public:
  t_32u value() {
  #ifdef ENDIAN_BIG
    return bswap_32(mem);
  #else
    return mem;
  #endif
    }

  void assign(t_32u q) {
  #ifdef ENDIAN_BIG
    mem = bswap_32(q);
  #else
    mem = q;
  #endif
    }
  };

class tr_motorola_32u {
// Big endian, Motorola byte order, most significant byte first
private:
  t_32u mem;

public:
  t_32u value() {
  #ifdef ENDIAN_BIG
    return mem;
  #else
    return bswap_32(mem);
  #endif
    }

  void assign(t_32u q) {
  #ifdef ENDIAN_BIG
    mem = q;
  #else
    mem = bswap_32(q);
  #endif
    }
  };

#pragma pack(pop)

#pragma pack(push,1)  // force byte packed structures

typedef struct {
  tr_intel_32u x;
  tr_motorola_32u y;
  } tr_exchange;

#pragma pack(pop)

int main(void) {
  tr_exchange v;
  t_32u r;
  t_longint loop;

  if (!init_general()) {
    return 1;
    }

  printf("Testing endian access records (in C++)...\n");

  if (sizeof(tr_exchange) != 8) {
    printf("tr_exchange defective\n");
    return 1;
    }
  if (bswap_32(0x12345678) != 0x78563412) {
    printf("bswap_32 defective\n");
    return 1;
    }
  if (bswap_32(0x87654321) != 0x21436587) {
    printf("bswap_32 defective\n");
    return 1;
    }

  r = 0;
  for (loop=1; loop<=1000000; ++loop) {
    r = r*256+t_32u(random_int(256));
    v.x.assign(r);
    if (v.x.value() != r) {
      printf("tr_intel_32u defective\n");
      return 1;
      }
    v.y.assign(r);
    if (v.y.value() != r) {
      printf("tr_motorola_32u defective\n");
      return 1;
      }
    }
  printf("OK\n");
  return 0;
  }

// eof.
