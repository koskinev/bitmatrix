//////
// Intro

// Switch on string

// (c) 2011..2020 Sigrid/Jasper Neumann
// www.sirrida.de / programming.sirrida.de
// E-Mail: info@sirrida.de

// Granted to the public domain
// First version: 2013-02-20
// Last change: 2013-03-04

// Open hash with constexpr.
// Hash values must not collide; compiler will complain.
// You need G++ >= 4.6 for constexpr.
// g++ -std=c++0x

#ifndef unit__case_ce
#define unit__case_ce

#include "general.h"
#include "modula.h"

#include <iostream>
using namespace std;

// At least g++ 4.6.3 is too stupid to allow t_char instead of char
// for constexpr functions; g++ 4.7.2 is OK.

constexpr long hash_1(const char* s) {
  return s[0] ? ((hash_1(&s[1])&0x02ffffff)*37+s[0]) : 0;
  // The masking with about 0x7fffffff/37 is there to avoid overflow
  }

#define SWITCH(s) \
  {{ \
    const char* _s_=(s); \
    switch (hash_1(_s_)) { \
      if (false) {{

#define CASE(s) \
          break; \
          }} \
      if (false) { \
        case hash_1(s): \
        if (strcmp(_s_,(s))==0) { \

#define DEFAULT \
          break; \
          }} \
      { default: {

#define CASE_MULTI0 \
          break; \
          }} \
      if (false) {

#define CASE_MULTI(s) \
          break; \
          }} \
      if (false) { \
        case hash_1(s): \
        if (strcmp(_s_,(s))!=0) { \
          break; \
          }

#define CASE_OR(s) \
        if (false) { \
          case hash_1(s): \
          if (strcmp(_s_,(s))!=0) { \
            break; \
            }}

#define CASE_DO {

#endif

// eof.
