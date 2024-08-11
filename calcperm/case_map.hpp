//////
// Intro

// Switch on string

// (c) 2011..2020 Sigrid/Jasper Neumann
// www.sirrida.de / programming.sirrida.de
// E-Mail: info@sirrida.de

// Granted to the public domain
// First version: 2013-02-23
// Last change: 2013-04-12

// Hash via optimized map.
// g++

// Options:
//   DEBUG_CASE_DUPE: Check for dupes at runtime: Output an error message.
//   DEBUG_CASE_STATISTICS: Dump a map statistics
//   USE_MEMCMP: Use memcmp instead of strcmp
//   CHECK_IDENTITY: Check string constants for identity beforehand

#ifndef unit__case_map
#define unit__case_map

#include "general.h"
#include "modula.h"

#include <iostream>
using namespace std;

mycall t_int floor_power_2(t_int x);
mycall t_int ceil_power_2(t_int x);

typedef struct tr_string_entry {
  struct tr_string_entry* next;
  t_char* s;
  t_int i;
#ifdef USE_MEMCMP
  t_int len_1;  // length+1, i.e. including 0 character
#endif
  } tr_string_entry;

#ifdef USE_MEMCMP
  #define STRING_ENTRY_EXT ,0
#else
  #define STRING_ENTRY_EXT
#endif

class tr_string_map {
public:
  mycall t_int (*hash)(const t_char* s);
  tr_string_entry** map;
  t_int nr_slots;
  t_int mask;
  volatile t_bool finalized;

  tr_string_map();
  t_bool init();
  void finalize();
  t_int find(const t_char* s, t_int not_found);
  };

#define LBL(x) (__LINE__-(t_int)(_o_)+2-(x))
  // The fiddling with _o_ shifts the range's lower bound to 0.

// On first run let _l_=0 and collect the CASE descriptors,
// then with _l_=1 apply string_map_finalize to determine
// the best suiting hash function, build a hash table,
// and thereby reverse the linking order of each chain
// which effectively links the first appearing CASE entries first.
// On this and all later runs (_l_=2) calculate the hash of the given string
// and loop through all strings of the corresponding hash slot.
// When the string is found, dispatch with switch to the given line.
#define SWITCH(s) \
{{ \
  enum {_o_ = __LINE__}; \
  static tr_string_map _m_; \
  t_int _i_; \
  t_int _l_; \
  for (_l_=(_m_.finalized?2:0); _l_<=2; ++_l_) { \
    if (_l_ == 2) { \
      _i_ = _m_.find((s), LBL(1)); \
      } \
    else if (_l_ == 0) { \
      if (_m_.init()) { \
        _l_ = 1; \
        continue; \
        } \
      _i_ = LBL(2); \
      } \
    else { \
      _m_.finalize(); \
      continue; \
      } \
    switch (_i_) { \
      case LBL(2): if (false) {

// Define one struct per CASE and link it before the previous one.
#define CASE(s) \
        break; \
        } \
      if (_l_ == 0) { \
        static tr_string_entry _e_ = {0, (s), LBL(0) STRING_ENTRY_EXT}; \
        _e_.next = _m_.map[0]; \
        _m_.map[0] = &_e_; \
        } \
      else \
        case LBL(0): {

#define DEFAULT \
        break; \
        } \
      if (_l_ != 0) \
        default: {

#define CASE_MULTI0 \
        break; \
        } \
      { \
        enum {_x_ = LBL(0)};

#define CASE_MULTI(s) \
        break; \
        } \
      { \
        enum {_x_ = LBL(0)}; \
        if (_l_ == 0) { \
          static tr_string_entry _e_ = {0, (s), (t_int)(_x_) STRING_ENTRY_EXT}; \
          _e_.next = _m_.map[0]; \
          _m_.map[0] = &_e_; \
          }

#define CASE_OR(s) \
        if (_l_ == 0) { \
          static tr_string_entry _e_ = {0, (s), (t_int)(_x_) STRING_ENTRY_EXT}; \
          _e_.next = _m_.map[0]; \
          _m_.map[0] = &_e_; \
          }

#define CASE_DO \
        case (t_int)(_x_): {} \
        } \
      if (_l_ != 0) {

#endif

// eof.
