//////
// Intro

// Some bit hacks and permutations

// (c) 2011..2020 Sigrid/Jasper Neumann
// www.sirrida.de / programming.sirrida.de
// E-Mail: info@sirrida.de

// Granted to the public domain
// First version: 2011-02
// Last change: 2012-09-19

// Here the adaptations are made to enable perm_bas.c
// to be compiled for a word size of 16 bit.
// perm_bas.c must be included afterwards.


//////
// Our base for the bit hacks

#define ld_bits 4
  // log_2 of used bit size (here: 16 bit)
#define ld_bits_factorial (1*2*3*4)

typedef t_16u t_bits;  // 1<<ld_bits bits, unsigned

#include "perm_bxx.h"


//////
// Derived stuff

const ta_subword a_stage_fwd = {0,1,2,3};
const ta_subword a_stage_bwd = {3,2,1,0};


//////
// Constant masks; must be adapted for other word sizes

const t_bits a_bfly_mask[]={
  // 0..ld_bits
  // For butterfly ops
  // = all_bits / ((1 << (1 << i)) + 1)
  0x5555,   // 0
  0x3333,   // 1
  0x0f0f,   // 2
  0x00ff,   // 3
  0xffff};  // 4

const t_bits a_bfly_lo[]={
  // 0..ld_bits
  // For auxiliary butterfly ops
  // a_bfly_mask with only lowest bit of runs set, index off by 1
  // = all_bits / ((1 << (1 << i)) - 1)
  0xffff,   // 0
  0x5555,   // 1 => a_bfly_mask[0]
  0x1111,   // 2
  0x0101,   // 3
  0x0001};  // 4

const t_bits a_bfly_hi[]={
  // Inverted a_bfly_mask with only highest bit of runs set, index off by 1.
  // = (a_bfly_lo[] >> 1)+hi_bit
  // = a_bfly_lo[] << ((1 << sw)-1)
  // = a_bfly_lo[] ror 1
  0xffff,   // 0
  0xaaaa,   // 1 => ~a_bfly_mask[0]
  0x8888,   // 2
  0x8080,   // 3
  0x8000};  // 4

const t_bits a_sw_base[]={
  // 0..ld_bits
  // (lo_bit << (1 << sw)) - 1; correct even for sw=ld_bits
  0x0001,   // 0
  0x0003,   // 1
  0x000f,   // 2
  0x00ff,   // 3
  0xffff};  // 4

const t_bits a_shuffle_mask[]={
  // 0..ld_bits-2
  // For [un]shuffle
  // a_shuffle_mask[i] = a_bfly_mask[i+1] & ~a_bfly_mask[i]
  // => bit_index_swap
  0x2222,   // 0
  0x0c0c,   // 1
  0x00f0};  // 2

const t_bits a_prim_swap[]={
  // 0..ld_bits-1
  // For prim_swap
  // Sum must fill all but highest bit
  // a_prim_swap[i] = a_bfly_lo[i+1] << ((1 << i) - 1)
  0x5555,   // 0
  0x2222,   // 1
  0x0808,   // 2
  0x0080};  // 3

// eof.
