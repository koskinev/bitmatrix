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
// to be compiled for a word size of 32 bit.
// perm_bas.c must be included afterwards.


//////
// Our base for the bit hacks

#define ld_bits 5  // log_2 of used bit size (here: 32 bit)
#define ld_bits_factorial (1*2*3*4*5)

typedef t_32u t_bits;  // 1<<ld_bits bits, unsigned

#include "perm_bxx.h"


//////
// Derived stuff

const ta_subword a_stage_fwd = {0,1,2,3,4};
const ta_subword a_stage_bwd = {4,3,2,1,0};


//////
// Constant masks; must be adapted for other word sizes

const t_bits a_bfly_mask[]={
  // 0..ld_bits
  // For butterfly ops
  // = all_bits / ((1 << (1 << i)) + 1)
  0x55555555,   // 0
  0x33333333,   // 1
  0x0f0f0f0f,   // 2
  0x00ff00ff,   // 3
  0x0000ffff,   // 4
  0xffffffff};  // 5

const t_bits a_bfly_lo[]={
  // 0..ld_bits
  // For auxiliary butterfly ops
  // a_bfly_mask with only lowest bit of runs set, index off by 1
  // = all_bits / ((1 << (1 << i)) - 1)
  0xffffffff,   // 0
  0x55555555,   // 1 => a_bfly_mask[0]
  0x11111111,   // 2
  0x01010101,   // 3
  0x00010001,   // 4
  0x00000001};  // 5

const t_bits a_bfly_hi[]={
  // Inverted a_bfly_mask with only highest bit of runs set, index off by 1.
  // = (a_bfly_lo[] >> 1)+hi_bit
  // = a_bfly_lo[] << ((1 << sw)-1)
  // = a_bfly_lo[] ror 1
  0xffffffff,   // 0
  0xaaaaaaaa,   // 1 => ~a_bfly_mask[0]
  0x88888888,   // 2
  0x80808080,   // 3
  0x80008000,   // 4
  0x80000000};  // 5

const t_bits a_sw_base[]={
  // 0..ld_bits
  // (lo_bit << (1 << sw)) - 1; correct even for sw=ld_bits
  0x00000001,   // 0
  0x00000003,   // 1
  0x0000000f,   // 2
  0x000000ff,   // 3
  0x0000ffff,   // 4
  0xffffffff};  // 5

const t_bits a_shuffle_mask[]={
  // 0..ld_bits-2
  // For [un]shuffle
  // a_shuffle_mask[i] = a_bfly_mask[i+1] & ~a_bfly_mask[i]
  // => bit_index_swap
  0x22222222,   // 0
  0x0c0c0c0c,   // 1
  0x00f000f0,   // 2
  0x0000ff00};  // 3

const t_bits a_prim_swap[]={
  // 0..ld_bits-1
  // For prim_swap
  // Sum must fill all but highest bit
  // a_prim_swap[i] = a_bfly_lo[i+1] << ((1 << i) - 1)
  0x55555555,   // 0
  0x22222222,   // 1
  0x08080808,   // 2
  0x00800080,   // 3
  0x00008000};  // 4

// eof.
