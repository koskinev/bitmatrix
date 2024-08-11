//////
// Intro

// Switch on string

// (c) 2011..2020 Sigrid/Jasper Neumann
// www.sirrida.de / programming.sirrida.de
// E-Mail: info@sirrida.de

// Granted to the public domain
// First version: 2013-02-23
// Last change: 2013-04-01

// Test program
// g++
// see case_*.cpp for additional info

// Options for case_map.cpp and case_ms.cpp:
//   #define DEBUG_CASE_DUPE
//   #define DEBUG_CASE_STATISTICS
// Options for case_map.cpp:
//   #define USE_MEMCMP
//   #define CHECK_IDENTITY

#include "general.c"
#include "modula.h"

#include <iostream>
#include <string>
using namespace std;

// Choose one of these SWITCH implementations by activating one #include
// * These ones use C strings:
// #include "case_ifc.c"
// #include "case_ce.cpp"
// #include "case_map.cpp"
// * These ones use STL strings:
// #include "case_ifs.cpp"
// #include "case_hm.cpp"
#include "case_ms.cpp"

// Adapt the string type to the one needed by the #include above
// typedef t_char* t_mystring;  // for C strings
typedef string t_mystring;  // for STL strings

typedef struct {
  t_int i;
  t_mystring s;
  } tr_test;

// Sample strings taken from calcperm.cpp

const tr_test a1[]={
  { 1, "allow_bmi"},
  { 2, "allow_bswap"},
  { 3, "bonus_bit_permute_step"},
  { 4, "bonus_bit_permute_step_simple"},
  { 5, "bonus_gs"},
  { 6, "bonus_gs_rol"},
  { 7, "bonus_mask_rol"},
  { 8, "brief"},
  { 9, "comment_postfix"},
  {10, "comment_prefix"},
  {11, "cost_and"},
  {12, "cost_bit_permute_step"},
  {13, "cost_bit_permute_step_simple"},
  {14, "cost_bool"},
  {15, "cost_bswap"},
  {16, "cost_gather"},
  {17, "cost_gs"},
  {18, "cost_mask"},
  {19, "cost_mul"},
  {20, "cost_or"},
  {21, "cost_rotate"},
  {22, "cost_rotate_shift"},
  {23, "cost_scatter"},
  {24, "cost_shift"},
  {25, "cost_xor"},
  {26, "dump_input"},
  {27, "dump_inverse"},
  {28, "hex_postfix"},
  {29, "hex_prefix"},
  {30, "in_indexes"},
  {31, "in_base"},
  {32, "in_origin"},
  {33, "op_and"},
  {34, "op_assign"},
  {35, "op_bswap"},
  {36, "op_gather"},
  {37, "op_or"},
  {38, "op_pstep"},
  {39, "op_pstep_simple"},
  {40, "op_rol"},
  {41, "op_scatter"},
  {42, "op_shl"},
  {43, "op_shr"},
  {44, "op_xor"},
  {45, "opt_bswap"},
  {46, "opt_gs"},
  {47, "opt_rol"},
  {48, "opt_rol_ex"},
  {49, "output_c"},
  {50, "output_pas"},
  {51, "self_test"},
  {52, "test_benes"},
  {53, "test_bfly"},
  {54, "test_bit_groups"},
  {55, "test_bpc"},
  {56, "test_gather_scatter"},
  {57, "test_gather_shift"},
  {58, "test_gather_shift_sloppy"},
  {59, "test_ibfly"},
  {60, "test_mul"},
  {61, "test_sag"},
  {62, "test_shift_scatter"},
  {63, "test_shift_scatter_sloppy"},
  {64, "verbose"},
  {65, "###"},  // test default case
  {65, "bri@g"},  // test hash collision aka "brief" (case_ce.*)
  {0, ""} };  // end of array sentinel

const tr_test a2[]={
  {1, "abc"},
  {1, "def"},
  {1, "ghi"},
  {2, "jkl"},
  {2, "mno"},
  {3, "p1"},
  {4, ""},
  {5, "###"},  // test default case
  {0, ""} };  // end of array sentinel

int main(UNUSED(int argc), const char* argv[]) {
  t_bool ok;
  t_char* s0 = (t_char*) argv[1];
  t_mystring s;

  if (!init_general()) {
    return 1;
    }

  ok = true;

  IF s0 == 0 DO
    s = "";  // must be non-null
  ELSE
    s = s0;  // convert
    END

  cout << ("Testing switch on string in C++...\n");
  cout << "input: " << s << "\n";

  // Minimal SWITCH: empty
  SWITCH(s)
    END

  // Minimal SWITCH: empty, ""
  SWITCH("")
    END

  // Minimal SWITCH: empty, "x"
  SWITCH("x")
    END

  // Minimal SWITCH: one CASE
  SWITCH(s)
    CASE("abc")
    END

  // Minimal SWITCH: one CASE, ""
  SWITCH("")
    CASE("abc")
    END

  // Minimal SWITCH: one CASE, "x"
  SWITCH("x")
    CASE("abc")
    END

  // Minimal SWITCH: DEFAULT only
  SWITCH(s)
    DEFAULT
    END

  // Nested SWITCH
  SWITCH(s)
    CASE("abc")
      cout << ("abc found\n");
      SWITCH(s)
        CASE("abc") cout << ("abc found\n");
        DEFAULT cout << ("nothing found\n");
        END
    DEFAULT cout << ("nothing found\n");
    END

  // Multiple execution, multiple labels for one case via goto
  LOOP(3)
    SWITCH(s)
      CASE("abc") cout << ("abc found\n");
      CASE("def") cout << ("def found\n");
      CASE("ghi") cout << ("ghi found\n");
      CASE("jkl") cout << ("jkl found, jump to mno\n");  goto mno;
      CASE("mno")
      mno:
        cout << ("mno found\n");
      CASE("aaa") cout << ("aaa found\n");
      CASE("aab") cout << ("aab found\n");
      CASE("aba") cout << ("aba found\n");
      DEFAULT cout << ("nothing found\n");
      END
    END

  // Perfect hash on first character
  SWITCH(s)
    CASE("abc") cout << ("abc found\n");
    CASE("def") cout << ("def found\n");
    CASE("ghi") cout << ("ghi found\n");
    CASE("jkl") cout << ("jkl found\n");
    DEFAULT cout << ("nothing found\n");
    END

  // Multiple labels for one case via macro
  BEGIN
    t_int i,idx;

    i = -1;
    idx = 0;
    WHILE a2[idx].i != 0 DO
      SWITCH(a2[idx].s)
        CASE_MULTI("abc")  // Multiple case targets different lines...
        CASE_OR("def")
        CASE_OR("ghi")
        CASE_DO                                  i = 1;
        CASE_MULTI("jkl") CASE_OR("mno") CASE_DO i = 2;  // ...and on one line.
        CASE_MULTI0 CASE_OR("p1") CASE_DO        i = 3;  // With CASE_MULTI0.
        CASE("")                                 i = 4;  // Empty string too.
        DEFAULT                                  i = 5;
        END
      IF i != a2[idx].i DO
        ok = false;
        cout << "Internal error (a2[" << idx << "].i=" << a2[idx].i << ")!\n";
        END
      ++idx;
      END
    END

  // Superfluous break in SWITCH
  BEGIN
    int i;

    i = 0;
    LOOP(3)
      SWITCH("abc")
        CASE("abc")
          break;
        END
      i = i + 1;
      END
    IF i != 3 DO
      ok = false;
      cout << ("Internal error SWITCH+break!\n");
      END
    END

  // Measure time for large SWITCH
  LOOP(100000)
    t_int i,idx;

    i = -1;
    idx = 0;
    WHILE a1[idx].i != 0 DO
      SWITCH(a1[idx].s)
        CASE("allow_bmi")                     i =  1;
        CASE("allow_bswap")                   i =  2;
        CASE("bonus_bit_permute_step")        i =  3;
        CASE("bonus_bit_permute_step_simple") i =  4;
        CASE("bonus_gs")                      i =  5;
        CASE("bonus_gs_rol")                  i =  6;
        CASE("bonus_mask_rol")                i =  7;
        CASE("brief")                         i =  8;
        CASE("comment_postfix")               i =  9;
        CASE("comment_prefix")                i = 10;
        CASE("cost_and")                      i = 11;
        CASE("cost_bit_permute_step")         i = 12;
        CASE("cost_bit_permute_step_simple")  i = 13;
        CASE("cost_bool")                     i = 14;
        CASE("cost_bswap")                    i = 15;
        CASE("cost_gather")                   i = 16;
        CASE("cost_gs")                       i = 17;
        CASE("cost_mask")                     i = 18;
        CASE("cost_mul")                      i = 19;
        CASE("cost_or")                       i = 20;
        CASE("cost_rotate")                   i = 21;
        CASE("cost_rotate_shift")             i = 22;
        CASE("cost_scatter")                  i = 23;
        CASE("cost_shift")                    i = 24;
        CASE("cost_xor")                      i = 25;
        CASE("dump_input")                    i = 26;
        CASE("dump_inverse")                  i = 27;
        CASE("hex_postfix")                   i = 28;
        CASE("hex_prefix")                    i = 29;
        CASE("in_indexes")                    i = 30;
        CASE("in_base")                       i = 31;
        CASE("in_origin")                     i = 32;
        CASE("op_and")                        i = 33;
        CASE("op_assign")                     i = 34;
        CASE("op_bswap")                      i = 35;
        CASE("op_gather")                     i = 36;
        CASE("op_or")                         i = 37;
        CASE("op_pstep")                      i = 38;
        CASE("op_pstep_simple")               i = 39;
        CASE("op_rol")                        i = 40;
        CASE("op_scatter")                    i = 41;
        CASE("op_shl")                        i = 42;
        CASE("op_shr")                        i = 43;
        CASE("op_xor")                        i = 44;
        CASE("opt_bswap")                     i = 45;
        CASE("opt_gs")                        i = 46;
        CASE("opt_rol")                       i = 47;
        CASE("opt_rol_ex")                    i = 48;
        CASE("output_c")                      i = 49;
        CASE("output_pas")                    i = 50;
        CASE("self_test")                     i = 51;
        CASE("test_benes")                    i = 52;
        CASE("test_bfly")                     i = 53;
        CASE("test_bit_groups")               i = 54;
        CASE("test_bpc")                      i = 55;
        CASE("test_gather_scatter")           i = 56;
        CASE("test_gather_shift")             i = 57;
        CASE("test_gather_shift_sloppy")      i = 58;
        CASE("test_ibfly")                    i = 59;
        CASE("test_mul")                      i = 60;
        CASE("test_sag")                      i = 61;
        CASE("test_shift_scatter")            i = 62;
        CASE("test_shift_scatter_sloppy")     i = 63;
        CASE("verbose")                       i = 64;
        DEFAULT                               i = 65;
        END
      IF i != a1[idx].i DO
        ok = false;
        cout << "Internal error (a1[" << idx << "].i=" << a1[idx].i <<
          " => " << i << ")!\n";
        END
      ++idx;
      END
    END

  IF ok DO
    cout << ("OK\n");
    END

  return ok ? 0 : 1;
  }

// eof.
