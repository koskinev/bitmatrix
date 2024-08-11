//////
// Intro

// Modula-style program flow statements for C / C++

// (c) 2011..2020 Sigrid/Jasper Neumann
// www.sirrida.de / programming.sirrida.de
// E-Mail: info@sirrida.de

// Granted to the public domain
// First version: 2013-02-23
// Last change: 2013-04-11

// gcc / g++

// BEGIN statements END
// WHILE expr DO statements END
// IF expr DO statements {ELIF expr DO statements} [ELSE statements] END
// LOOP(n) statements END
// FOR/FOR_REV(type,loop_var,start,end) statements END
// The FOR* statements loop from s to e inclusive and are capable of
// looping over the whole range of a type such as 0..255 for unsigned char.
// For more info see modula.txt.

#define END }}}}}
  // Common end for all macros here.
  // Yes, it seems that there are far too many braces.
  // We need them for all macros including the string switch macros in case_*.*
  // to be usable with the same END macro.

#define BEGIN {{{{{
  // Block begin

#define WHILE {{{{ while (

#define IF {{{{ if (
#define ELIF } else if (
#define ELSE } else {

#define LOOP(n) {{{{ unsigned int _v_; for (_v_=(n); _v_!=0; --_v_) {
  // Perform n times. n must be >= 0.

#define DO ) {

// FOR / FOR_REV:
// Using GCC's __typeof__(t) one could even get rid of the type parameter t...

// Here is a quite effective but obscure solution.
// In most cases optimal code is generated.
// 2013-04-01 Sigrid/Jasper Neumann
#define FOR(t,v,s,e) \
  { \
    t _v_, _e_; \
    _v_ = (s); \
    _e_ = (e); \
    if (_v_ <= _e_) { \
      switch (0) { \
        while (_v_ != _e_) { \
          ++_v_; \
          default: \
          { \
            const t v = _v_;

#define FOR_REV(t,v,s,e) \
  { \
    t _v_, _e_; \
    _v_ = (s); \
    _e_ = (e); \
    if (_v_ >= _e_) { \
      switch (0) { \
        while (_v_ != _e_) { \
          --_v_; \
          default: \
          { \
            const t v = _v_;

// eof.
