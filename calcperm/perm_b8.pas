//////
// Intro

// Some bit hacks and permutations

// (c) 2011..2020 Sigrid/Jasper Neumann
// www.sirrida.de / programming.sirrida.de
// E-Mail: info@sirrida.de

// Granted to the public domain
// First version: 2011-02
// Last change: 2012-09-23

// Here the adaptations are made to enable perm_bas.pas
// to be compiled for a word size of 8 bit.
// perm_bas.pas must be included afterwards.


//////
// Our base for the bit hacks

const ld_bits = 3;  // log_2 of used bit size (here: 8 bit)
const ld_bits_factorial = 1*2*3;

type t_bits = t_8u;  // 1 shl ld_bits bits, unsigned

{$i perm_bxx.pas }


//////
// Derived stuff

const all_bits = hi_bit+(hi_bit-1);  // GNU Pascal does not like t_bits(-1)

const a_stage_fwd: ta_subword = (0,1,2);
const a_stage_bwd: ta_subword = (2,1,0);


//////
// Constant masks; must be adapted for other word sizes

const a_bfly_mask: array [0..ld_bits] of t_bits = (
  // 0..ld_bits
  // For butterfly ops
  // = all_bits div ((1 shl (1 shl i)) + 1)
  $55,   // 0
  $33,   // 1
  $0f,   // 2
  $ff);  // 3

const a_bfly_lo: array [0..ld_bits] of t_bits = (
  // 0..ld_bits
  // For auxiliary butterfly ops
  // a_bfly_mask with only lowest bit of runs set, index off by 1
  // = all_bits div ((1 shl (1 shl i)) - 1)
  $ff,   // 0
  $55,   // 1 => a_bfly_mask[0]
  $11,   // 2
  $01);  // 3

const a_bfly_hi: array [0..ld_bits] of t_bits = (
  // Inverted a_bfly_mask with only highest bit of runs set, index off by 1.
  // = (a_bfly_lo[] shr 1)+hi_bit
  // = a_bfly_lo[] shl ((1 shl sw)-1)
  // = a_bfly_lo[] ror 1
  $ff,   // 0
  $aa,   // 1 => not a_bfly_mask[0]
  $88,   // 2
  $80);  // 3

const a_sw_base: array [0..ld_bits] of t_bits = (
  // 0..ld_bits
  // (lo_bit shl (1 shl sw)) - 1; correct even for sw=ld_bits
  $01,   // 0
  $03,   // 1
  $0f,   // 2
  $ff);  // 3

const a_shuffle_mask: array [0..ld_bits-2] of t_bits = (
  // 0..ld_bits-2
  // For [un]shuffle
  // a_shuffle_mask[i] = a_bfly_mask[i+1] and not a_bfly_mask[i]
  // => bit_index_swap
  $22,   // 0
  $0c);  // 1

const a_prim_swap: array [0..ld_bits-1] of t_bits = (
  // 0..ld_bits-1
  // For prim_swap
  // Sum must fill all but highest bit
  // a_prim_swap[i] = a_bfly_lo[i+1] shl ((1 shl i) - 1)
  $55,   // 0
  $22,   // 1
  $08);  // 2

// eof.
