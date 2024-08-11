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
// to be compiled for a word size of 64 bit.
// perm_bas.pas must be included afterwards.


//////
// Our base for the bit hacks

const ld_bits = 6;  // log_2 of used bit size (here: 64 bit)
const ld_bits_factorial = 1*2*3*4*5*6;

type t_bits = t_64u;  // 1 shl ld_bits bits, unsigned

{$i perm_bxx.pas }


//////
// Derived stuff

//const all_bits = hi_bit+(hi_bit-1);  // GNU Pascal does not like t_bits(-1)
const all_bits = t_bits(-1);  // needed by Delphi for bits=64

const a_stage_fwd: ta_subword = (0,1,2,3,4,5);
const a_stage_bwd: ta_subword = (5,4,3,2,1,0);


//////
// Constant masks; must be adapted for other word sizes

const a_bfly_mask: array [0..ld_bits] of t_bits = (
  // 0..ld_bits
  // For butterfly ops
  // = all_bits div ((1 shl (1 shl i)) + 1)
  $5555555555555555,   // 0
  $3333333333333333,   // 1
  $0f0f0f0f0f0f0f0f,   // 2
  $00ff00ff00ff00ff,   // 3
  $0000ffff0000ffff,   // 4
  $00000000ffffffff,   // 5
  all_bits         );  // 6

const a_bfly_lo: array [0..ld_bits] of t_bits = (
  // 0..ld_bits
  // For auxiliary butterfly ops
  // a_bfly_mask with only lowest bit of runs set, index off by 1
  // = all_bits div ((1 shl (1 shl i)) - 1)
  all_bits         ,   // 0
  $5555555555555555,   // 1 => a_bfly_mask[0]
  $1111111111111111,   // 2
  $0101010101010101,   // 3
  $0001000100010001,   // 4
  $0000000100000001,   // 5
  $0000000000000001);  // 6

const a_bfly_hi: array [0..ld_bits] of t_bits = (
  // Inverted a_bfly_mask with only highest bit of runs set, index off by 1.
  // = (a_bfly_lo[] shr 1)+hi_bit
  // = a_bfly_lo[] shl ((1 shl sw)-1)
  // = a_bfly_lo[] ror 1
  all_bits         ,   // 0
  $aaaaaaaaaaaaaaaa,   // 1 => not a_bfly_mask[0]
  $8888888888888888,   // 2
  $8080808080808080,   // 3
  $8000800080008000,   // 4
  $8000000080000000,   // 5
  $8000000000000000);  // 6

const a_sw_base: array [0..ld_bits] of t_bits = (
  // 0..ld_bits
  // (lo_bit shl (1 shl sw)) - 1; correct even for sw=ld_bits
  $0000000000000001,   // 0
  $0000000000000003,   // 1
  $000000000000000f,   // 2
  $00000000000000ff,   // 3
  $000000000000ffff,   // 4
  $00000000ffffffff,   // 5
  all_bits         );  // 6

const a_shuffle_mask: array [0..ld_bits-2] of t_bits = (
  // 0..ld_bits-2
  // For [un]shuffle
  // a_shuffle_mask[i] = a_bfly_mask[i+1] and not a_bfly_mask[i]
  // => bit_index_swap
  $2222222222222222,   // 0
  $0c0c0c0c0c0c0c0c,   // 1
  $00f000f000f000f0,   // 2
  $0000ff000000ff00,   // 3
  $00000000ffff0000);  // 4

const a_prim_swap: array [0..ld_bits-1] of t_bits = (
  // 0..ld_bits-1
  // For prim_swap
  // Sum must fill all but highest bit
  // a_prim_swap[i] = a_bfly_lo[i+1] shl ((1 shl i) - 1)
  $5555555555555555,   // 0
  $2222222222222222,   // 1
  $0808080808080808,   // 2
  $0080008000800080,   // 3
  $0000800000008000,   // 5
  $0000000080000000);  // 6

// eof.
