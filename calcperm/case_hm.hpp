//////
// Intro

// Switch on string

// (c) 2011..2020 Sigrid/Jasper Neumann
// www.sirrida.de / programming.sirrida.de
// E-Mail: info@sirrida.de

// Granted to the public domain
// First version: 2013-02-20
// Last change: 2013-04-12

// Hash via hash map (STL, STL strings).
// You need G++ >= 4.5 for C++0x.
// g++ -std=c++0x
// Not checked for dupes.

#ifndef unit__case_hm
#define unit__case_hm

#include "general.h"
#include "modula.h"

#include <iostream>
#include <string>
#include <unordered_map>
using namespace std;

typedef unordered_map<string,t_int> string_map;
typedef pair<string,t_int> string_entry;

#define LBL(x) (__LINE__-(t_int)(_o_)+2-(x))
  // The fiddling with _o_ shifts the range's lower bound to 0.

// On first run let _l_=0 and collect the CASE descriptors
// into the hash map.
// On this and all later runs (_l_=2) look up the given string.
// When the string is found, dispatch with switch to the given line.
#define SWITCH(s) \
{{ \
  enum {_o_ = __LINE__}; \
  static string_map* _m_ = 0; \
  static volatile t_bool _f_ = false; \
  t_int _i_; \
  t_int _l_; \
  for (_l_=(_f_?2:0); _l_<=2; ++_l_) { \
    if (_l_ == 2) { \
      SYNC_SYNCHRONIZE_LOAD \
      auto _p_ = _m_->find(s); \
      if (_p_ == _m_->end()) { \
        _i_ = LBL(1); \
        } \
      else { \
        _i_ = _p_->second; \
        } \
      } \
    else if (_l_ == 0) { \
      SYNC_SYNCHRONIZE_LOAD \
      GLOBAL_MUTEX_WAIT \
      if (_f_) { \
        GLOBAL_MUTEX_SIGNAL \
        _l_ = 1; \
        continue; \
        } \
      _m_ = new string_map(); \
      _i_ = LBL(2); \
      } \
    else { \
      SYNC_SYNCHRONIZE_STORE \
      _f_ = true; \
      GLOBAL_MUTEX_SIGNAL \
      continue; \
      } \
    switch (_i_) { \
      case LBL(2): if (false) {

// Add one entry per CASE to the hash map.
#define CASE(s) \
        break; \
        } \
      if (_l_ == 0) { \
        _m_->insert(string_entry((s), LBL(0))); \
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
          _m_->insert(string_entry((s), (t_int)(_x_))); \
          }

#define CASE_OR(s) \
        if (_l_ == 0) { \
          _m_->insert(string_entry((s), (t_int)(_x_))); \
          }

#define CASE_DO \
        case (t_int)(_x_): {} \
        } \
      if (_l_ != 0) {

#endif

// eof.
