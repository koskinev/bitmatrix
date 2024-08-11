//////
// Intro

// Switch on string

// (c) 2011..2020 Sigrid/Jasper Neumann
// www.sirrida.de / programming.sirrida.de
// E-Mail: info@sirrida.de

// Granted to the public domain
// First version: 2013-02-20
// Last change: 2013-03-04

// Stupid prototype with else-if chain.
// gcc
// Not checked for dupes.

#ifndef unit__case_ifc
#define unit__case_ifc

#include "general.h"
#include "modula.h"

// The switch is there to allow for an optional break
#define SWITCH(s) {{ switch(0) {case 0: { const t_char* _s_=(s); if (false) {
#define CASE(s) } else if (strcmp(_s_,(s))==0) {
#define DEFAULT } else {

#define CASE_MULTI0 } else if ( 0
#define CASE_MULTI(s) } else if ( (strcmp(_s_,(s))==0)
#define CASE_OR(s) || (strcmp(_s_,(s))==0)
#define CASE_DO ) {

#endif

// eof.
