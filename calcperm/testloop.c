//////
// Intro

// Loops

// (c) 2011..2020 Sigrid/Jasper Neumann
// www.sirrida.de / programming.sirrida.de
// E-Mail: info@sirrida.de

// Granted to the public domain
// First version: 2013-02-23
// Last change: 2013-03-04

// Test program
// gcc

#include "general.c"
#include "modula.h"

#define FORx(t,v,s,e,x) \
  { \
    t v,_e_; \
    v=(s); \
    _e_=(e); \
    if (v<=_e_) { \
      while (true) { \
        { x } \
        if (v==_e_) \
          break; \
        ++v; \
        } \
      } \
    }

#define FOR1(t,v,s,e) \
  { \
    t v,_e_; \
    v=(s); \
    _e_=(e); \
    if (v<=_e_) { \
      while (true) { \
        {

#define FOR2(v) \
          } \
        if (v==_e_) \
          break; \
        ++v; \
        } \
      } \
    }

int main(void) {
  int count;
  t_bool ok;

  if (!init_general()) {
    return 1;
    }

  ok = true;
  printf("Testing loops...\n");

  // Prototype
  count = 0;
  {
    t_8u e=255;
    t_8u v=0;
    if (v<=e) {
      while (true) {
        ++count;
        if (v==e)  break;
        ++v;
        }
      }
    }
  IF count != 256 DO
    ok = false;
    printf("Error in prototype loop (0..255): count\n");
    END

  // One macro with statement parameter
  count = 0;
  FORx(t_8u, i, 0, 255,
    IF (int)i != count DO
      ok = false;
      printf("Error in FORx(0..255): value\n");
      END
    ++count;
    )
  IF count != 256 DO
    ok = false;
    printf("Error in FORx(0..255): count\n");
    END

  // Two macros
  count = 0;
  FOR1(t_8u, i, 0, 255)
    IF (int)i != count DO
      ok = false;
      printf("Error in FOR1/FOR2(0..255): value\n");
      END
    ++count;
    FOR2(i)
  IF count != 256 DO
    ok = false;
    printf("Error in FOR1/FOR2(0..255): count\n");
    END

  // Preferred solution, upward
  count = 0;
  FOR(t_8u, i, 0, 255)
    IF (int)i != count DO
      ok = false;
      printf("Error in FOR(0..255): value\n");
      END
    ++count;
    END
  IF count != 256 DO
    ok = false;
    printf("Error in FOR(0..255): count\n");
    END

  // Preferred solution, downward
  count = 0;
  FOR_REV(t_8u, i, 255, 0)
    IF (int)i != 255-count DO
      ok = false;
      printf("Error in FOR_REV(255..0): value\n");
      END
    ++count;
    END
  IF count != 256 DO
    ok = false;
    printf("Error in FOR_REV(255..0): count\n");
    END

  // FOR+break
  count = 0;
  FOR(t_8u, i, 0, 255)
    if (i==0) {}  // silence compiler
    ++count;
    IF count == 10 DO
      break;
      END
    END
  IF count != 10 DO
    ok = false;
    printf("Error in FOR+break\n");
    END

  // FOR+continue
  count = 0;
  FOR(t_8u, i, 0, 255)
    if (i==0) {}  // silence compiler
    ++count;
    IF count == 10 DO
      continue;
      END
    END
  IF count != 256 DO
    ok = false;
    printf("Error in FOR+continue\n");
    END

  // LOOP
  count = 0;
  LOOP(256)
    ++count;
    END
  IF count != 256 DO
    ok = false;
    printf("Error in LOOP: count\n");
    END

  // LOOP+break
  count = 0;
  LOOP(256)
    ++count;
    IF count == 10 DO
      break;
      END
    END
  IF count != 10 DO
    ok = false;
    printf("Error in LOOP+break\n");
    END

  // LOOP+continue
  count = 0;
  LOOP(256)
    ++count;
    IF count == 10 DO
      continue;
      END
    END
  IF count != 256 DO
    ok = false;
    printf("Error in LOOP+continue\n");
    END

  IF ok DO
    printf("OK\n");
    END

  return ok ? 0 : 1;
  }

// eof.
