//////
// Intro

// Some bit hacks and permutations

// (c) 2011..2020 Sigrid/Jasper Neumann
// www.sirrida.de / programming.sirrida.de
// E-Mail: info@sirrida.de

// Granted to the public domain
// First version: 2011-02
// Last change: 2012-09-19

// Here the adaptations are made to enable perm_bas.pas
// to be compiled for a word size of 32 bit.
// perm_bas.pas must be included afterwards.


//////
// Our base for the bit hacks

const ld_bits = 5;  // log_2 of used bit size (here: 32 bit)
const ld_bits_factorial = 1*2*3*4*5;

type t_bits = t_32u;  // 1 shl ld_bits bits, unsigned

{$i perm_bxx.pas }


//////
// Derived stuff

const all_bits = hi_bit+(hi_bit-1);  // GNU Pascal does not like t_bits(-1)

const a_stage_fwd: ta_subword = (0,1,2,3,4);
const a_stage_bwd: ta_subword = (4,3,2,1,0);


//////
// Constant masks; must be adapted for other word sizes

const a_bfly_mask: array [0..ld_bits] of t_bits = (
  // 0..ld_bits
  // For butterfly ops
  // = all_bits div ((1 shl (1 shl i)) + 1)
  $55555555,   // 0
  $33333333,   // 1
  $0f0f0f0f,   // 2
  $00ff00ff,   // 3
  $0000ffff,   // 4
  $ffffffff);  // 5

const a_bfly_lo: array [0..ld_bits] of t_bits = (
  // 0..ld_bits
  // For auxiliary butterfly ops
  // a_bfly_mask with only lowest bit of runs set, index off by 1
  // = all_bits div ((1 shl (1 shl i)) - 1)
  $ffffffff,   // 0
  $55555555,   // 1 => a_bfly_mask[0]
  $11111111,   // 2
  $01010101,   // 3
  $00010001,   // 4
  $00000001);  // 5

const a_bfly_hi: array [0..ld_bits] of t_bits = (
  // Inverted a_bfly_mask with only highest bit of runs set, index off by 1.
  // = (a_bfly_lo[] shr 1)+hi_bit
  // = a_bfly_lo[] shl ((1 shl sw)-1)
  // = a_bfly_lo[] ror 1
  $ffffffff,   // 0
  $aaaaaaaa,   // 1 => not a_bfly_mask[0]
  $88888888,   // 2
  $80808080,   // 3
  $80008000,   // 4
  $80000000);  // 5

const a_sw_base: array [0..ld_bits] of t_bits = (
  // 0..ld_bits
  // (lo_bit shl (1 shl sw)) - 1; correct even for sw=ld_bits
  $00000001,   // 0
  $00000003,   // 1
  $0000000f,   // 2
  $000000ff,   // 3
  $0000ffff,   // 4
  $ffffffff);  // 5

const a_shuffle_mask: array [0..ld_bits-2] of t_bits = (
  // 0..ld_bits-2
  // For [un]shuffle
  // a_shuffle_mask[i] = a_bfly_mask[i+1] and not a_bfly_mask[i]
  // => bit_index_swap
  $22222222,   // 0
  $0c0c0c0c,   // 1
  $00f000f0,   // 2
  $0000ff00);  // 3

const a_prim_swap: array [0..ld_bits-1] of t_bits = (
  // 0..ld_bits-1
  // For prim_swap
  // Sum must fill all but highest bit
  // a_prim_swap[i] = a_bfly_lo[i+1] shl ((1 shl i) - 1)
  $55555555,   // 0
  $22222222,   // 1
  $08080808,   // 2
  $00800080,   // 3
  $00008000);  // 4

// eof.
