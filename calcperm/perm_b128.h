//////
// Intro

// Some bit hacks and permutations

// (c) 2011..2020 Sigrid/Jasper Neumann
// www.sirrida.de / programming.sirrida.de
// E-Mail: info@sirrida.de

// Granted to the public domain
// First version: 2011-02
// Last change: 2012-09-23

// Here the adaptations are made to enable perm_bas.c
// to be compiled for a word size of 128 bit.
// perm_bas.c must be included afterwards.


//////
// Our base for the bit hacks

#define ld_bits 7
  // log_2 of used bit size (here: 128 bit)
#define ld_bits_factorial (1*2*3*4*5*6*7)

typedef __uint128_t t_bits;  // 1<<ld_bits bits, unsigned

#include "perm_bxx.h"


//////
// Derived stuff

const ta_subword a_stage_fwd = {0,1,2,3,4,5,6};
const ta_subword a_stage_bwd = {6,5,4,3,2,1,0};


//////
// Constant masks; must be adapted for other word sizes

const t_bits a_bfly_mask[]={
  // 0..ld_bits
  // For butterfly ops
  // = all_bits / ((1 << (1 << i)) + 1)
  (((t_bits)(0x5555555555555555ULL))<<64)+0x5555555555555555ULL,   // 0
  (((t_bits)(0x3333333333333333ULL))<<64)+0x3333333333333333ULL,   // 1
  (((t_bits)(0x0f0f0f0f0f0f0f0fULL))<<64)+0x0f0f0f0f0f0f0f0fULL,   // 2
  (((t_bits)(0x00ff00ff00ff00ffULL))<<64)+0x00ff00ff00ff00ffULL,   // 3
  (((t_bits)(0x0000ffff0000ffffULL))<<64)+0x0000ffff0000ffffULL,   // 4
  (((t_bits)(0x00000000ffffffffULL))<<64)+0x00000000ffffffffULL,   // 5
  (((t_bits)(0x0000000000000000ULL))<<64)+0xffffffffffffffffULL,   // 6
  (((t_bits)(0xffffffffffffffffULL))<<64)+0xffffffffffffffffULL};  // 7

const t_bits a_bfly_lo[]={
  // 0..ld_bits
  // For auxiliary butterfly ops
  // a_bfly_mask with only lowest bit of runs set, index off by 1
  // = all_bits / ((1 << (1 << i)) - 1)
  (((t_bits)(0xffffffffffffffffULL))<<64)+0xffffffffffffffffULL,   // 0
  (((t_bits)(0x5555555555555555ULL))<<64)+0x5555555555555555ULL,   // 1 => a_bfly_mask[0]
  (((t_bits)(0x1111111111111111ULL))<<64)+0x1111111111111111ULL,   // 2
  (((t_bits)(0x0101010101010101ULL))<<64)+0x0101010101010101ULL,   // 3
  (((t_bits)(0x0001000100010001ULL))<<64)+0x0001000100010001ULL,   // 4
  (((t_bits)(0x0000000100000001ULL))<<64)+0x0000000100000001ULL,   // 5
  (((t_bits)(0x0000000000000001ULL))<<64)+0x0000000000000001ULL,   // 6
  (((t_bits)(0x0000000000000000ULL))<<64)+0x0000000000000001ULL};  // 7

const t_bits a_bfly_hi[]={
  // Inverted a_bfly_mask with only highest bit of runs set, index off by 1.
  // = (a_bfly_lo[] >> 1)+hi_bit
  // = a_bfly_lo[] << ((1 << sw)-1)
  // = a_bfly_lo[] ror 1
  (((t_bits)(0xffffffffffffffffULL))<<64)+0xffffffffffffffffULL,   // 0
  (((t_bits)(0xaaaaaaaaaaaaaaaaULL))<<64)+0xaaaaaaaaaaaaaaaaULL,   // 1 => ~a_bfly_mask[0]
  (((t_bits)(0x8888888888888888ULL))<<64)+0x8888888888888888ULL,   // 2
  (((t_bits)(0x8080808080808080ULL))<<64)+0x8080808080808080ULL,   // 3
  (((t_bits)(0x8000800080008000ULL))<<64)+0x8000800080008000ULL,   // 4
  (((t_bits)(0x8000000080000000ULL))<<64)+0x8000000080000000ULL,   // 5
  (((t_bits)(0x8000000000000000ULL))<<64)+0x8000000000000000ULL,   // 6
  (((t_bits)(0x8000000000000000ULL))<<64)+0x0000000000000000ULL};  // 7

const t_bits a_sw_base[]={
  // 0..ld_bits
  // (lo_bit << (1 << sw)) - 1; correct even for sw=ld_bits
  (((t_bits)(0x0000000000000000ULL))<<64)+0x0000000000000001ULL,   // 0
  (((t_bits)(0x0000000000000000ULL))<<64)+0x0000000000000003ULL,   // 1
  (((t_bits)(0x0000000000000000ULL))<<64)+0x000000000000000fULL,   // 2
  (((t_bits)(0x0000000000000000ULL))<<64)+0x00000000000000ffULL,   // 3
  (((t_bits)(0x0000000000000000ULL))<<64)+0x000000000000ffffULL,   // 4
  (((t_bits)(0x0000000000000000ULL))<<64)+0x00000000ffffffffULL,   // 5
  (((t_bits)(0x0000000000000000ULL))<<64)+0xffffffffffffffffULL,   // 6
  (((t_bits)(0xffffffffffffffffULL))<<64)+0xffffffffffffffffULL};  // 7

const t_bits a_shuffle_mask[]={
  // 0..ld_bits-2
  // For [un]shuffle
  // a_shuffle_mask[i] = a_bfly_mask[i+1] & ~a_bfly_mask[i]
  // => bit_index_swap
  (((t_bits)(0x2222222222222222ULL))<<64)+0x2222222222222222ULL,   // 0
  (((t_bits)(0x0c0c0c0c0c0c0c0cULL))<<64)+0x0c0c0c0c0c0c0c0cULL,   // 1
  (((t_bits)(0x00f000f000f000f0ULL))<<64)+0x00f000f000f000f0ULL,   // 2
  (((t_bits)(0x0000ff000000ff00ULL))<<64)+0x0000ff000000ff00ULL,   // 3
  (((t_bits)(0x00000000ffff0000ULL))<<64)+0x00000000ffff0000ULL,   // 4
  (((t_bits)(0x0000000000000000ULL))<<64)+0xffffffff00000000ULL};  // 5

const t_bits a_prim_swap[]={
  // 0..ld_bits-1
  // For prim_swap
  // Sum must fill all but highest bit
  // a_prim_swap[i] = a_bfly_lo[i+1] << ((1 << i) - 1)
  (((t_bits)(0x5555555555555555ULL))<<64)+0x5555555555555555ULL,   // 0
  (((t_bits)(0x2222222222222222ULL))<<64)+0x2222222222222222ULL,   // 1
  (((t_bits)(0x0808080808080808ULL))<<64)+0x0808080808080808ULL,   // 2
  (((t_bits)(0x0080008000800080ULL))<<64)+0x0080008000800080ULL,   // 3
  (((t_bits)(0x0000800000008000ULL))<<64)+0x0000800000008000ULL,   // 5
  (((t_bits)(0x0000000080000000ULL))<<64)+0x0000000080000000ULL,   // 6
  (((t_bits)(0x0000000000000000ULL))<<64)+0x8000000000000000ULL};  // 7

// eof.
