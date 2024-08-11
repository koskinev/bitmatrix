//////
// Intro

// Switch on string

// (c) 2011..2020 Sigrid/Jasper Neumann
// www.sirrida.de / programming.sirrida.de
// E-Mail: info@sirrida.de

// Granted to the public domain
// First version: 2013-02-20
// Last change: 2013-03-04

// Stupid prototype with else-if chain (STL strings).
// You need a C++ compiler because of string.
// g++
// Not checked for dupes.

#ifndef unit__case_ifs
#define unit__case_ifs

#include "general.h"
#include "modula.h"

#include <iostream>
#include <string>
using namespace std;

// The switch is there to allow for an optional break
#define SWITCH(s) {{ switch(0) {case 0: { const string _s_ = (s); if (false) {
#define CASE(s) } else if (_s_==(s)) {
#define DEFAULT } else {

#define CASE_MULTI0 } else if ( 0
#define CASE_MULTI(s) } else if ( (_s_==(s))
#define CASE_OR(s) || (_s_==(s))
#define CASE_DO ) {

#endif

// eof.
