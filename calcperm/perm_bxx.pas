//////
// Intro

// Some bit hacks and permutations

// (c) 2011..2020 Sigrid/Jasper Neumann
// www.sirrida.de / programming.sirrida.de
// E-Mail: info@sirrida.de

// Granted to the public domain
// First version: 2013-02-15
// Last change: 2013-02-15

// Local include file to define some derived constants.

const bits = 1 shl ld_bits;
const lo_bit = t_bits(1);
const hi_bit = lo_bit shl (bits-1);
// const all_bits = ...  // depending on compiler, see perm_b*

type t_bit_index = -1..bits-1;  // subrange 0..bits-1; -1:don't care
  // used to specify bit indexes

const no_index = t_bit_index(-1);  // don't care / wildcard

type t_subword_set=t_bit_index;
  // used to specify a set of t_subword

type t_subword = 0..ld_bits;  // subrange 0..ld_bits
  // used to specify log_2 of subword size, see a_subword

type ta_subword = array [0..ld_bits-1] of t_subword;
  // bit index indexes

type ta_index = array [0..bits-1] of t_bit_index;
  // bit indexes

const a_subword: array [0..9] of string[6] = (
             // sw bits
  'Bit',     // 0  1
  'Nyp',     // 1  2, name stolen from Donald E. Knuth
  'Nibble',  // 2  4
  'Byte',    // 3  8
  'Word',    // 4  16
  'DWord',   // 5  32
  'QWord',   // 6  64
  'OWord',   // 7  128
  'YWord',   // 8  256
  'ZWord');  // 9  512

// eof.
