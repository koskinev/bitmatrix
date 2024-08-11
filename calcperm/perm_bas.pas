//////
// Intro

// Some bit hacks and permutations

// (c) 2011..2020 Sigrid/Jasper Neumann
// www.sirrida.de / programming.sirrida.de
// E-Mail: info@sirrida.de

// Granted to the public domain
// First version: 2011-02
// Last change: 2014-09-16

// This is a collection of some bit fiddling procedures.
// All routines act on a word size depending on the
// steering file included before (perm_b*.pas).
// These routines naturally adapt to several word (working) sizes
// of a bit length of a power of 2 (as long as the compiler allows).

// Auxiliary routines
//   - gcd
//   - mul_inv
//   - rol
//   - rol_lo
//   - rolc_lo
//   - gray_code
//   - inv_gray_code
//   - nr_1bits
//   - nr_leading_0bits
//   - nr_trailing_0bits
//   - is_contiguous_1bits
//   - tbm
//   - blend
//   - simd_odd
//   - bit_permute_step
//   - bit_permute_step_simple
//   - bit_permute_step2
//   - identity_perm
//   - invert_perm
//   - random_perm
//   - used_source_bits
//   - used_target_bits
// Bit index operations
//   - bit_index_complement
//   - bit_index_swap
//   - bit_index_swap_complement
//   - bit_index_ror
//   - transpose
//   - shuffle_power
//   - unshuffle_power
//   - permute_bpc
//   - invert_bpc
// Generalized Bit Reversal
//   - general_reverse_bits
//   - bswap
// Swap by primitives
//   - prim_swap
// Shuffle and unshuffle
//   - shuffle
//   - unshuffle
// Compress and expand
//   * Compress bit masks
//     - compress_mask_right
//     - compress_mask_left
//     - compress_mask
//   * Generate configuration
//     - gen_ce_right
//     - gen_ce_left
//   * Usage
//     - apply_compress_right
//     - apply_compress_left
//     - apply_expand_right
//     - apply_expand_left
//   * Compound
//     - compress_right
//     - compress_left
//     - compress
//     - expand_right
//     - expand_left
//     - expand
// Butterfly network
//   - butterfly
//   - bfly
//   - ibfly
//   - bfly_parity
// Rotate via butterfly
//   * Generate configuration
//     - gen_frot
//     - gen_vrot
//   * Compound
//     - fror_bfly
//     - frol_bfly
//     - frot_bfly
//     - vror_bfly
//     - vrol_bfly
//     - vrot_bfly
// SWAR rotate
//   * frol
//   * fror
//   * frot
//   * frolc
//   * frorc
//   * frotc
//   * vrol
//   * vror
//   * vrot
// Compress/expand-flip via butterfly
//   * Generate configuration
//     - gen_cef_right
//     - gen_cef_left
//   * Compound
//     - compress_flip_right
//     - compress_flip_left
//     - compress_flip
//     - expand_flip_right
//     - expand_flip_left
//     - expand_flip
// Omega/flip
//   - omega
//   - flip
// Permutations via Benes network
//   - gen_benes_ex
//   - gen_benes
//   - benes_fwd
//   - benes_bwd
//   - benes_fwd_ex
//   - benes_bwd_ex
//   - benes_parity


//////
// Random replacement

var my_randseed: t_bits;

function random_bits:t_bits;
// random(x) is only valid for x up to 32767
// Replace with your own generator if you want.

// var x: t_bits;
// begin
//   x := random(32767);
//   x := (x shl 12) + random(32767);
//   x := (x shl 12) + random(32767);
//   random_bits := x;
//   end;
begin
  my_randseed := my_randseed * $08088405 + 1;  // random as in Delphi (32 bit)
    // this alternates between even and odd numbers
  random_bits := my_randseed + t_bits(random(32767));  // we correct this
  end;


//////
// Auxiliary stuff

type t_direction=(right,left);

function gcd(a,b:t_longint):t_longint;
// Greatest common divisor.
var
  t: t_longint;
begin
  // a = abs(a);
  // b = abs(b);
  while b <> 0 do begin
    t := a mod b;
    a := b;
    b := t;
    end;
  gcd := a;
  end;

function mul_inv(x:t_bits):t_bits;
// Calculate multiplicative inverse, i.e. mul_inv(x)*x=1.
// The result is defined for all odd values of x.
// For even x we simply return 0.
// See Hacker's Delight, 10.16 "Exact division by constants"
// Multiplicative inverse modulo 2**bits by Newton's method.
var
  xn,t: t_bits;
begin
  if not odd(x) then
    mul_inv := 0  // only defined for odd numbers
  else begin
    xn := x;
    while true do begin
      t := x * xn;
      if t = 1 then
        BREAK;
      xn := xn * (2 - t);
      end;
    mul_inv := xn;
    end;
  end;

function rol(x:t_bits; rot:t_int):t_bits;
// INLINE
// Rotate left a complete word.
// x: value to be rotated
// rot: rotate count, negative values will rotate right
// 1 cycle (should be if inlined)
begin
  // A shift by >= bits is undefined.
  // We take care for this case with the "and (bits-1)" below.
  // For many CPUs this stunt is not neccessary.
  rol := (x shl (rot and (bits-1))) or (x shr ((-rot) and (bits-1)));
  end;

function rol_lo(x:t_bits; rot:t_int; sw:t_subword):t_bits;
// Rotate left. This is not a SWAR operation.
// x: value to be rotated
// rot: rotate count, negative values will rotate right
// sw: log_2 of the number of affected bits, must be <=ld_bits
// Only the low (1 shl sw) bits of x are processed and returned.
var
  b: t_int;  // # affected bits
  r: t_int;  // rot mod b
  m: t_bits;  // mask for affected bits
begin
  if sw >= ld_bits then begin
    // Prevent shifting by >= bits at [*].
    rol_lo := rol(x, rot);
    end
  else begin
    b := 1 shl sw;

    // m := a_sw_base[sw];
    m := (lo_bit shl b)-1;  // [*] m must become -1 for sw=bits

    r := rot and (b-1);
    x := x and m;
    x := (x shl r) or (x shr (b-r));  // [*]
    rol_lo := x and m;
    end;
  end;

function rolc_lo(x:t_bits; rot:t_int; sw:t_subword):t_bits;
// Rotate left and complement. This is not a SWAR operation.
// x: value to be rotated
// rot: rotate count, negative values will rotate/complement right
// sw: log_2 of the number of affected bits, must be <=ld_bits
// Only the low (1 shl sw) bits of x are processed and returned.
var
  b: t_int;  // # affected bits
  r: t_int;  // rot mod b
  m: t_bits;  // mask for affected bits
begin
  b := 1 shl sw;

  m := a_sw_base[sw];  // m must become all_bits for sw=bits

  r := rot and (b-1);
  x := x and m;
  if b-r >= bits then begin
    // Prevent shifting by b-r >= bits.
    x := (x shl r) xor ((lo_bit shl r)-1);
    end
  else begin
    // x := (x shl r) or ((x shr (b-r)) xor ((lo_bit shl r)-1));  // same result
    x := ((x shl r) or (x shr (b-r))) xor ((lo_bit shl r)-1);
    // x := rol_lo(x,r) xor ((lo_bit shl r)-1);
    end;
  if (rot and b) <> 0 then begin
    x := not x;
    end;
  rolc_lo := x and m;
  end;

function gray_code(x:t_bits):t_bits;
// See Hacker's Delight, 13.1 "Gray Code"
begin
  gray_code := x xor (x shr 1);
  end;

function inv_gray_code(x:t_bits):t_bits;
// See Hacker's Delight, 13.1
var
  i: t_int;
begin
  for i := 0 to ld_bits-1 do begin  // UNROLL
    x := x xor (x shr (1 shl i));
    end;
  inv_gray_code := x;
  end;

function nr_1bits(x:t_bits):t_int;
// Count the set bits, aka population count.
// See Hacker's Delight, 5.1 "Counting 1-Bits"
var
  i: t_int;
begin
  for i := 0 to ld_bits-1 do begin  // UNROLL
    x := (x and a_bfly_mask[i]) + ((x shr (1 shl i)) and a_bfly_mask[i]);
    end;
  nr_1bits := t_int(x);
  end;

function nr_leading_0bits(x:t_bits):t_int;
// See Hacker's Delight, 5.3 "Counting Leading 0's"
// floor(log_2(x))
// 0 => bits
var
  res, i: t_int;
begin
  if x = 0 then begin
    res := bits;
    end
  else begin
    res := bits-1;
    for i := ld_bits-1 downto 0 do begin  // UNROLL
      if (x and not a_bfly_mask[i]) <> 0 then begin
        x := x shr (1 shl i);
        res := res - (1 shl i);
        end;
      end;
    end;
  nr_leading_0bits := res;
  end;

function nr_trailing_0bits(x:t_bits):t_int;
// See Hacker's Delight, 5.4 "Counting Trailing 0's"
// 0 => bits
var
  res, i: t_int;
begin
  if x = 0 then begin
    res := bits;
    end
  else begin
    res := 0;
    for i := ld_bits-1 downto 0 do begin  // UNROLL
      if (x and a_sw_base[i]) = 0 then begin
        x := x shr (1 shl i);
        res := res + (1 shl i);
        end;
      end;
    end;
  nr_trailing_0bits := res;
  end;

function is_contiguous_1bits(x:t_bits):t_bool;
// Is x a contiguous bit string?
begin
  is_contiguous_1bits := ((((x - 1) or x) + 1) and x) = 0;
  end;

function tbm(x:t_bits; mode:t_int):t_bits;
// General trailing bit modifications.
// 2014-02-11 Sigrid/Jasper Neumann
begin
  case mode and $1f of
    $00: x := 0;
    $01: x := x and not(x+1);
    $02: x := not(x) and (x+1);
    $03: x := x xor (x+1);
    $04: x := not(x xor (x+1));
    $05: x := x or not(x+1);
    $06: x := not(x) or (x+1);
    $07: x := all_bits;
    $08: x := x and (x+1);
    $09: x := x;
    $0a: x := x+1;
    $0b: x := x or (x+1);
    $0c: x := not(x or (x+1));
    $0d: x := not(x)-1;
    $0e: x := not(x);
    $0f: x := not(x and (x+1));
    $10: x := 0;
    $11: x := not(x) and (x-1);
    $12: x := x and -x;
    $13: x := x xor (x-1);
    $14: x := x xor -x;
    $15: x := not(x) or (x-1);
    $16: x := x or -x;
    $17: x := all_bits;
    $18: x := x and (x-1);
    $19: x := x-1;
    $1a: x := x;
    $1b: x := x or (x-1);
    $1c: x := not(x or (x-1));
    $1d: x := not(x);
    $1e: x := -x;
    $1f: x := not(x and (x-1));
    end;
  tbm := x;
  end;

function blend(m,x,y:t_bits):t_bits;
// The bit equivalent to if m then x else y.
// O(1)
begin
  blend := (m and x) or (not m and y);  // or can be replaced by xor or +
  // blend := ((x or y) and m) xor y;
  end;

function simd_odd(x:t_bits; sw:t_subword):t_bits;
// Set mask denoting odd values, i.e. propagate low bit to the left.
// 2014-07-12 Sigrid/Jasper Neumann
// O(1)
var
  m: t_bits;
  lsb: t_bits;
  shift: t_int;
begin
  shift := 1 shl sw;
  m := a_bfly_lo[sw];
  lsb := x and m;
  lsb := ((lsb shl (shift-1)) shl 1) - lsb;  // transform lsb: 1 ==> -1
  // A shift by >= bits is undefined.
  // Here we do a double shift to avoid shift by bits.
  simd_odd := lsb;
  end;

function bit_permute_step(x,m:t_bits; shift:t_uint):t_bits;
// INLINE
// Can be replaced by bit_permute_step_simple,
// if for the relevant bits n the following holds:
// nr_1bits(bit_permute_step_simple(n,m,shift)) = nr_1bits(n)
// x86: >= 6/5 cycles
// ARM: >= 4/4 cycles
var
  t: t_bits;
begin
  // assert(((m shl shift) and m) = 0);
  // assert(((m shl shift) shr shift) = m);
  t := ((x shr shift) xor x) and m;
  x := x xor t;  t := t shl shift;   x := x xor t;  // x := (x xor t) xor (t shl shift);
  bit_permute_step := x;
  end;

function bit_permute_step_simple(x,m:t_bits; shift:t_uint):t_bits;
// INLINE
// Simplified replacement of bit_permute_step
// Can always be replaced by bit_permute_step (not vice-versa).
// x86: >= 5/4 (5/3) cycles
// ARM: >= 3/2 cycles
begin
  // assert(((m shl shift) and m) = 0);
  // assert(((m shl shift) shr shift) = m);
  // assert(((m shl shift) or m) = all_bits);  // for permutations
  bit_permute_step_simple := ((x and m) shl shift) or ((x shr shift) and m);
  end;

procedure bit_permute_step2(var x1,x2:t_bits; m:t_bits; shift:t_uint);
// INLINE
// Extended variant of bit_permute_step.
// Will be slow if not inlined.
var
  t: t_bits;
begin
  t := ((x2 shr shift) xor x1) and m;
  x1 := x1 xor t;
  x2 := x2 xor (t shl shift);
  end;

procedure identity_perm(var tgt:ta_index);
var
  i: t_int;
begin
  for i := 0 to bits-1 do begin
    tgt[i] := i;
    end;
  end;

procedure invert_perm(const src:ta_index; var tgt:ta_index);
var
  i: t_int;
begin
  for i := 0 to bits-1 do begin
    tgt[i] := no_index;
    end;
  for i := 0 to bits-1 do begin
    if src[i] <> no_index then begin
      tgt[src[i]] := i;
      end;
    end;
  end;

procedure random_perm(var tgt:ta_index);
var
  j,q: t_int;
  x: t_int;
begin
  for q := 0 to bits-1 do begin
    tgt[q] := q;
    end;

  for q := 1 to bits-1 do begin
    j := random_int(q+1);  // 0..q
    x := tgt[q];
    tgt[q] := tgt[j];
    tgt[j] := x;
    end;
  end;

function used_source_bits(const perm:ta_index):t_bits;
// 2012-09-14 Sigrid/Jasper Neumann
var
  i: t_int;
  n_src: t_bits;
begin
  n_src := 0;  // set of needed source bits
  for i := 0 to bits-1 do begin
    if perm[i] <> no_index then begin
      n_src := n_src or (lo_bit shl perm[i]);
      end;
    end;
  used_source_bits := n_src;
  end;

function used_target_bits(const perm:ta_index):t_bits;
// 2012-09-14 Sigrid/Jasper Neumann
var
  i: t_int;
  n_tgt: t_bits;
  mask: t_bits;
begin
  n_tgt := 0;  // set of needed target bits
  mask := 1;
  for i := 0 to bits-1 do begin
    if perm[i] <> no_index then begin
      n_tgt := n_tgt or mask;
      end;
    mask := mask shl 1;
    end;
  used_target_bits := n_tgt;
  end;


//////
// Bit index operations

function bit_index_complement(x:t_bits; k:t_subword):t_bits;
// INLINE
// See ARM System Developer's Guide, 7.6.2 "Bit Permutations"
// as used in the loop of general_reverse_bits.
var
  shift: t_int;
  m: t_bits;
begin
  shift := 1 shl k;
  m := a_bfly_mask[k];
  bit_index_complement := bit_permute_step_simple(x,m,shift);
  end;

function bit_index_swap(x:t_bits; j,k:t_subword):t_bits;
// INLINE
// See ARM System Developer's Guide, 7.6.2 "Bit Permutations"
// => shuffle, unshuffle
var
  shift: t_int;
  q: t_subword;
  m: t_bits;
begin
  if j <> k then begin
    if j < k then begin
      q := j;
      j := k;
      k := q;
      end;
    shift := (1 shl j) - (1 shl k);
    m := a_bfly_mask[j] and not a_bfly_mask[k];  // b_j=0 AND b_k=1
    x := bit_permute_step(x,m,shift);
    end;
  bit_index_swap := x;
  end;

function bit_index_swap_complement(x:t_bits; j,k:t_subword):t_bits;
// INLINE
// See ARM System Developer's Guide, 7.6.2 "Bit Permutations"
var
  shift: t_int;
  q: t_subword;
  m: t_bits;
begin
  if j <> k then begin
    if j < k then begin
      q := j;
      j := k;
      k := q;
      end;
    shift := (1 shl j) + (1 shl k);
    m := a_bfly_mask[j] and a_bfly_mask[k];  // b_j=0 AND b_k=0
    x := bit_permute_step(x,m,shift);
    end;
  bit_index_swap_complement := x;
  end;

function bit_index_ror(x:t_bits; ofs,field:t_subword; rot:t_int):t_bits;
// Rotate an bit index field to the right by rot.
// x+field+ofs=ld_bits
// x: upper bit indexes (unchanged)
// field: size of the affected bit string
// ofs: lower bit indexes (unchanged)
// Bit-parallel implementation: 2011-10-04 Sigrid/Jasper Neumann
var
  i,j,d,g,k,n: t_int;
begin
  if field > 0 then begin
    rot := rot mod t_int(field);
    if rot <> 0 then begin
      if rot < 0 then begin
        // we need a real modulo operation yielding 0..field-1
        rot := rot + field;
        end;
      g := gcd(field,rot);
      d := field div g;
      for i := 0 to g-1 do begin
        k := i;
        for j := 0 to d-2 do begin
          n := k + rot;
          if n >= field then begin
            // avoid mod
            n := n - field;
            end;
          x := bit_index_swap(x,n+ofs,k+ofs);
          k := n;
          end;
        end;
      end;
    end;
  bit_index_ror := x;
  end;

function transpose(x:t_bits; ld_fields,ld_col,ld_row:t_subword):t_bits;
// Transpose bit matrixes.
// ld_fields: width of the bit fields
// ld_col: ld(bit columns)
// ld_row: ld(bit rows)
// 2011-10-04 Sigrid/Jasper Neumann
begin
  transpose := bit_index_ror(x,ld_fields,ld_col+ld_row,ld_col);
  end;

function shuffle_power(x:t_bits; sw1,sw2:t_subword; pwr:t_int):t_bits;
// pwr times shuffle(x,sw1,sw2)
// See Hacker's Delight, 7.6/7.8 "Rearrangements and Index Transformations"
// 2011-10-04 Sigrid/Jasper Neumann
begin
  shuffle_power := bit_index_ror(x,sw1,sw2-sw1,-pwr);
  end;

function unshuffle_power(x:t_bits; sw1,sw2:t_subword; pwr:t_int):t_bits;
// pwr times unshuffle(x,sw1,sw2)
// See Hacker's Delight, 7.6/7.8 "Rearrangements and Index Transformations"
// 2011-10-04 Sigrid/Jasper Neumann
begin
  unshuffle_power := bit_index_ror(x,sw1,sw2-sw1,pwr);
  end;

function permute_bpc(x:t_bits; const tgt:ta_subword; k:t_subword_set):t_bits;
// Do a BPC permutation via bit index operation primitives.
// tgt: permutation vector, must hold 0..ld_bits-1, all different
// k: complement set, see general_reverse_bits
// 2012-02-07 Sigrid/Jasper Neumann
var
  src,inv_src: ta_subword;
  i,j,ii: t_subword;
  n,m,kk: t_subword_set;
begin
  for i := 0 to ld_bits-1 do begin
    src[i] := i;
    inv_src[i] := i;
    end;

  kk := 0;  // k for generated

  for i := 0 to ld_bits-1 do begin  // any order
    n := 1 shl i;
    ii := src[i];
    if tgt[i] = ii then begin
      if ((k xor kk) and n) <> 0 then begin
        x := bit_index_complement(x,i);
        end;
      end
    else begin
      j := inv_src[tgt[i]];
      m := 1 shl j;
      if ((k and n) <> 0) XOR ((kk and m) <> 0) then begin  // boolean xor
        x := bit_index_swap_complement(x,i,j);
        if (kk and n) = 0 then begin
          kk := kk or m;
          end
        else begin
          kk := kk and not m;
          end;
        end
      else begin
        x := bit_index_swap(x,i,j);
        if (kk and n) <> 0 then begin
          kk := kk or m;
          end
        else begin
          kk := kk and not m;
          end;
        end;

      inv_src[ii] := j;
      src[j] := ii;
      end;
    end;
  permute_bpc := x;
  end;

procedure invert_bpc(const src:ta_subword; src_k:t_subword_set; var tgt:ta_subword; var tgt_k:t_subword_set);
var
  i: t_subword;
begin
  for i := 0 to ld_bits-1 do begin
    tgt[src[i]] := i;
    end;
  tgt_k := 0;
  for i := 0 to ld_bits-1 do begin
    if (src_k and (1 shl tgt[i])) <> 0 then begin
      tgt_k := tgt_k or (1 shl i);
      end;
    end;
  end;


//////
// Generalized Bit Reversal

function general_reverse_bits(x:t_bits; k:t_int):t_bits;
// Swap all subwords of given levels.
// See Hacker's Delight, 7.1 "Generalized Bit Reversal"
// k: set of t_subword, i.e. one bit per subword size.
var
  i,j: t_int;
  m: t_bits;
begin
  for i := 0 to ld_bits-1 do begin  // UNROLL
    j := 1 shl i;
    if (k and j) <> 0 then begin
      // x := bit_index_complement(x,j);
      m := a_bfly_mask[i];
      x := bit_permute_step_simple(x,m,j);
      end;
    end;
  general_reverse_bits := x;
  end;

function bswap(x:t_bits):t_bits;
// INLINE
// Exchange byte order.
// This can be expressed in assembler:
// bits = 8: n/a
// bits = 16: "xchg al,ah" or "rol ax,16"
// bits = 32: "bswap eax"
// bits = 64: "bswap rax"
// bits = 128: "xchg rax,rdx; bswap rax; bswap rdx"
begin
  bswap := general_reverse_bits(x, not 7);
  end;


//////
// Swap by primitives

function prim_swap(x,m:t_bits):t_bits;
// Swap by primitives.
// Generalization of general_reverse_bits.
// See "Matters Computational" by Joerg Arndt, "A restricted method"
// See Hacker's Delight, 7.1 "Generalized Bit Reversal"
// Bit-parallel implementation: 2011-02 Sigrid/Jasper Neumann
var
  i,s: t_int;
  q: t_bits;
begin
  if (m and hi_bit) <> 0 then begin  // highest bit set?
    // normal operation
    for i := ld_bits-1 downto 0 do begin  // UNROLL
      q := m and a_prim_swap[i];
      s := 1 shl i;
      q := (q shl 1) - (q shr (s-1));  // broadcast bits
      x := bit_permute_step(x,q,s);
      end;
    end
  else begin
    // inverse operation
    // same as above but with reversed loop
    for i := 0 to ld_bits-1 do begin  // UNROLL
      q := m and a_prim_swap[i];
      s := 1 shl i;
      q := (q shl 1) - (q shr (s-1));  // broadcast bits
      x := bit_permute_step(x,q,s);
      end;
    end;
  prim_swap := x;
  end;


//////
// Shuffle and unshuffle

function shuffle(x:t_bits; sw1,sw2:t_subword):t_bits;
// Shuffle/zip/interlace entities.
// See Hacker's Delight, 7.2 "Shuffling Bits"
// sw1: log_2(subword_length): entities to move
// sw2: log_2(word_length): moving area
// 0 <= sw1 < sw2 <= ld_bits
// Example: sw1=2, sw2=5: Shuffle nibbles in dword
// See shuffle_power.
var
  i,j: t_int;
begin
  if sw2 >= 2 then begin
    // same code as ibfly but with special non-conforming masks
    for i := t_int(sw2)-2 downto t_int(sw1) do begin  // UNROLL?
      // x := bit_index_swap(x,i+1,i);
      j := 1 shl i;
      x := bit_permute_step(x, a_shuffle_mask[i], j);
      end;
    end;
  shuffle := x;
  end;

function unshuffle(x:t_bits; sw1,sw2:t_subword):t_bits;
// Unshuffle/unzip/uninterlace entities.
// See Hacker's Delight, 7.2 "Shuffling Bits"
// sw1: log_2(subword_length): entities to move
// sw2: log_2(word_length): moving area
// 0 <= sw1 < sw2 <= ld_bits
// Example: sw1=0, sw2=3: Unshuffle bits in bytes
// See unshuffle_power.
var
  i,j: t_int;
begin
  if sw2 >= 2 then begin
    // same code as bfly but with special non-conforming masks
    for i := t_int(sw1) to t_int(sw2)-2 do begin  // UNROLL?
      // x := bit_index_swap(x,i+1,i);
      j := 1 shl i;
      x := bit_permute_step(x, a_shuffle_mask[i], j);
      end;
    end;
  unshuffle := x;
  end;


//////
// A "class" for butterfly and other operations

type tr_bfly = record
  // This structure is used to hold the configuration of
  // butterfly-based operations as well as compress and expand.

  cfg: array [0..ld_bits-1] of t_bits;  // butterfly configuration
  mask: t_bits;  // saved mask, for compress/expand

  // Here is sketched how to convert this to a class:
  // Include all the generator and usage functions as private methods
  // and replace the parameter self by the implicit object reference.
  // Add the many compound routines.
  // Remove the name suffix  for all methods.
  // If you want to cache the configuration, add here:
  //   kind: the generator kind
  //     enum (initialized, frot, vrot, ce_right, ce_left, cef_right, cef_left)
  //   sw: the used subword size (t_subword)
  // Add an initializer/constructor which sets kind to initialized.
  // The generator routines must set the keys (kind, mask, sw).
  // The compound routines check the cached keys (kind, mask, sw);
  //   if not equal, call the generator routine and update the configuration;
  // finally they call the usage routine.
  end;


//////
// Compress and expand

//////
// Compress and expand: Compress bit masks

function compress_mask_right(m:t_bits; sw:t_subword):t_bits;
// Move all 1 bits of m to the right of their subwords.
// This is essentially the same code as gen_ce_right.
// We only need the final value of m.
// See Hacker's Delight, 7.4 "Compress, or Generalized Extract"
// UNFOLD all cases of sw
// =compress_right(m,m,sw)
var
  mk, mp, mv, mm, m0: t_bits;
  i, j, s: t_int;
begin
  if sw > 0 then begin
    m0 := a_bfly_lo[sw];
    mk := ((not m) shl 1) and not m0;  // We will count 0's to right
    for i := 0 to t_int(sw)-1 do begin  // UNROLL
      mp := mk;  // Parallel suffix
      for j := 0 to t_int(sw)-1 do begin  // UNROLL
        s := 1 shl j;
        mm := not(m0 shl s)+m0;
        mp := mp xor ((mp shl s) and mm);
          // Masking not needed for sw=ld_bits
        end;
      mv := mp and m;  // Bits to move
      m := (m xor mv) or (mv shr (1 shl i));  // Compress m
      mk := mk and not mp;
      end;
    end;
  compress_mask_right := m;
  end;

function compress_mask_left(m:t_bits; sw:t_subword):t_bits;
// Move all 1 bits of m to the left of their subwords.
// This is essentially the same code as gen_ce_left.
// We only need the final value of m.
// See Hacker's Delight, 7.4 "Compress, or Generalized Extract"
// UNFOLD all cases of sw
// =compress_left(m,m,sw)
var
  mk, mp, mv, mm, m0, m1: t_bits;
  i,j,s: t_int;
begin
  if sw > 0 then begin
    m1 := a_bfly_lo[sw];
    m0 := (m1 shr 1) + hi_bit;  // m0 := a_bfly_hi[sw];
    mk := ((not m) shr 1) and not m0;  // We will count 0's to right
    for i := 0 to t_int(sw)-1 do begin  // UNROLL
      mp := mk;  // Parallel suffix
      for j := 0 to t_int(sw)-1 do begin  // UNROLL
        s := 1 shl j;
        mm := (m0 shr (s-1)) - m1;
        mp := mp xor ((mp shr s) and mm);
          // Masking not needed for sw=ld_bits
        end;
      mv := mp and m;  // Bits to move
      m := (m xor mv) or (mv shl (1 shl i));  // Compress m
      mk := mk and not mp;
      end;
    end;
  compress_mask_left := m;
  end;

function compress_mask(m:t_bits; sw:t_subword; d:t_direction):t_bits;
// INLINE
begin
  case d of
    right: compress_mask := compress_mask_right(m,sw);
    left:  compress_mask := compress_mask_left(m,sw);
    else   compress_mask := 0;  // this can't happen
    end;
  end;


//////
// Compress and expand: Generate configuration

procedure gen_ce_right(var self:tr_bfly; m:t_bits; sw:t_subword);
// See Hacker's Delight, 7.4 "Compress, or Generalized Extract"
// See Hacker's Delight, 7.5 "Expand, or Generalized Insert"
// To compress use apply_compress_right.
// To expand use apply_expand_right.
// UNFOLD all cases of sw
var
  mk, mp, mv, mm, m0: t_bits;
  i,j,s: t_int;
begin
  self.mask := m;  // Save original mask
  for i := 0 to ld_bits-1 do begin  // UNROLL
    self.cfg[i] := 0;
    end;
  if sw > 0 then begin
    m0 := a_bfly_lo[sw];
    mk := ((not m) shl 1) and not m0;  // We will count 0's to right
    for i := 0 to t_int(sw)-1 do begin  // UNROLL
      mp := mk;  // Parallel suffix
      for j := 0 to t_int(sw)-1 do begin  // UNROLL
        s := 1 shl j;
        mm := not(m0 shl s)+m0;
        mp := mp xor ((mp shl s) and mm);
          // Masking not needed for sw=ld_bits
        end;
      mv := mp and m;  // Bits to move
      self.cfg[i] := mv;
      m := (m xor mv) or (mv shr (1 shl i));  // Compress m
      mk := mk and not mp;
      end;
    end;
  end;

procedure gen_ce_left(var self:tr_bfly; m:t_bits; sw:t_subword);
// See Hacker's Delight, 7.4 "Compress, or Generalized Extract"
// See Hacker's Delight, 7.5 "Expand, or Generalized Insert"
// To compress use apply_compress_left.
// To expand use apply_expand_left.
// UNFOLD all cases of sw
var
  mk, mp, mv, mm, m0, m1: t_bits;
  i,j,s: t_int;
begin
  self.mask := m;  // Save original mask
  for i := 0 to ld_bits-1 do begin  // UNROLL
    self.cfg[i] := 0;
    end;
  if sw > 0 then begin
    m1 := a_bfly_lo[sw];
    m0 := (m1 shr 1) + hi_bit;  // m0 := a_bfly_hi[sw];
    mk := ((not m) shr 1) and not m0;  // We will count 0's to right
    for i := 0 to t_int(sw)-1 do begin  // UNROLL
      mp := mk;  // Parallel suffix
      for j := 0 to t_int(sw)-1 do begin  // UNROLL
        s := 1 shl j;
        mm := (m0 shr (s-1)) - m1;
        mp := mp xor ((mp shr s) and mm);
          // Masking not needed for sw=ld_bits
        end;
      mv := mp and m;  // Bits to move
      self.cfg[i] := mv;
      m := (m xor mv) or (mv shl (1 shl i));  // Compress m
      mk := mk and not mp;
      end;
    end;
  end;


//////
// Compress and expand: Usage

function apply_compress_right(const self:tr_bfly; x:t_bits):t_bits;
// See Hacker's Delight, 7.4 "Compress, or Generalized Extract"
// self should be configured by gen_ce_right.
var
  t: t_bits;
  i: t_int;
begin
  x := x and self.mask;  // Clear irrelevant bits

  for i := 0 to ld_bits-1 do begin  // UNROLL
    t := x and self.cfg[i];
    x := (x xor t) or (t shr (1 shl i));  // Compress x
    end;

  apply_compress_right := x;
  end;

function apply_compress_left(const self:tr_bfly; x:t_bits):t_bits;
// See Hacker's Delight, 7.4 "Compress, or Generalized Extract"
// self should be configured by gen_ce_left.
var
  t: t_bits;
  i: t_int;
begin
  x := x and self.mask;  // Clear irrelevant bits

  for i := 0 to ld_bits-1 do begin  // UNROLL
    t := x and self.cfg[i];
    x := (x xor t) or (t shl (1 shl i));  // Compress x
    end;

  apply_compress_left := x;
  end;

function apply_expand_right(const self:tr_bfly; x:t_bits):t_bits;
// See Hacker's Delight, 7.4 "Compress, or Generalized Extract"
// See Hacker's Delight, 7.5 "Expand, or Generalized Insert"
// self should be configured by gen_ce_right.
// (a and b) or (not a and c) => ((b xor c) and a) xor c
var
  i: t_int;
begin
  for i := ld_bits-1 downto 0 do begin  // UNROLL
    x := (((x shl (1 shl i)) xor x) and self.cfg[i]) xor x;
    end;

  apply_expand_right := x and self.mask;  // Clear out extraneous bits
  end;

function apply_expand_left(const self:tr_bfly; x:t_bits):t_bits;
// See Hacker's Delight, 7.4 "Compress, or Generalized Extract"
// See Hacker's Delight, 7.5 "Expand, or Generalized Insert"
// self should be configured by gen_ce_left.
// (a and b) or (not a and c) => ((b xor c) and a) xor c
var
  i: t_int;
begin
  for i := ld_bits-1 downto 0 do begin  // UNROLL
    x := (((x shr (1 shl i)) xor x) and self.cfg[i]) xor x;
    end;

  apply_expand_left := x and self.mask;  // Clear out extraneous bits
  end;


//////
// Compress and expand: Compound

function compress_right(x:t_bits; m:t_bits; sw:t_subword):t_bits;
var
  ce: tr_bfly;
begin
  gen_ce_right(ce,m,sw);
  compress_right := apply_compress_right(ce,x);
  end;

function compress_left(x:t_bits; m:t_bits; sw:t_subword):t_bits;
var
  ce: tr_bfly;
begin
  gen_ce_left(ce,m,sw);
  compress_left := apply_compress_left(ce,x);
  end;

function compress(x:t_bits; m:t_bits; sw:t_subword; d:t_direction):t_bits;
// INLINE
begin
  case d of
    right: compress := compress_right(x,m,sw);
    left:  compress := compress_left(x,m,sw);
    else   compress := 0;  // this can't happen
    end;
  end;

function expand_right(x:t_bits; m:t_bits; sw:t_subword):t_bits;
var
  ce: tr_bfly;
begin
  gen_ce_right(ce,m,sw);
  expand_right := apply_expand_right(ce,x);
  end;

function expand_left(x:t_bits; m:t_bits; sw:t_subword):t_bits;
var
  ce: tr_bfly;
begin
  gen_ce_left(ce,m,sw);
  expand_left := apply_expand_left(ce,x);
  end;

function expand(x:t_bits; m:t_bits; sw:t_subword; d:t_direction):t_bits;
// INLINE
begin
  case d of
    right: expand := expand_right(x,m,sw);
    left:  expand := expand_left(x,m,sw);
    else   expand := 0;  // this can't happen
    end;
  end;


//////
// Butterfly network

function butterfly(x:t_bits; m:t_bits; sw:t_subword):t_bits;
// INLINE
// One butterfly step/stage.
// sw: 0..ld_bits-1
// m and a_bfly_mask[sw] should be = m
begin
  butterfly := bit_permute_step(x, m, 1 shl sw);
  end;

function bfly(const self:tr_bfly; x:t_bits):t_bits;
// Apply butterfly network on x configured by
//   - gen_frot
//   - gen_vrot
//   - gen_cef_right
//   - gen_cef_left
var
  stage,j: t_int;
begin
  for stage := ld_bits-1 downto 0 do begin  // UNROLL
    // x := butterfly(x, self.cfg[stage], sw);
    j := 1 shl stage;
    x := bit_permute_step(x, self.cfg[stage], j);
    end;

  bfly := x;
  end;

function ibfly(const self:tr_bfly; x:t_bits):t_bits;
// Apply inverse butterfly network on x configured by
//   - gen_frot
//   - gen_vrot
//   - gen_cef_right
//   - gen_cef_left
var
  stage,j: t_int;
begin
  for stage := 0 to ld_bits-1 do begin  // UNROLL
    // x := butterfly(x, self.cfg[stage], sw);
    j := 1 shl stage;
    x := bit_permute_step(x, self.cfg[stage], j);
    end;

  ibfly := x;
  end;

function bfly_parity(const self:tr_bfly):t_bool;
// Return the parity of a permutation given by a butterfly network.
// This is false for even parity and true for odd parity.
var
  stage: t_int;
  x: t_bits;
begin
  x := 0;
  for stage := 0 to ld_bits-1 do begin  // UNROLL
    x := x xor self.cfg[stage];
    end;

  bfly_parity := odd(nr_1bits(x));
  end;


//////
// Rotate via butterfly

//////
// Rotate via butterfly: Generate configuration

procedure gen_frot(var self:tr_bfly; rot:t_int; sw:t_subword);
// Fixed rotate.
// Generate configuration for [inverse] butterfly network.
// To rotate right use bfly.
// To rotate left use ibfly.
// Bit-parallel implementation: 2011-09-14 Sigrid/Jasper Neumann
var
  i: t_int;
begin
  self.mask := rot;
  for i := 0 to ld_bits-1 do begin  // UNROLL
    self.cfg[i] := 0;
    end;
  if sw > 0 then begin
    for i := 0 to t_int(sw)-1 do begin  // UNROLL
      self.cfg[i] := rolc_lo(0,rot,i) * a_bfly_lo[i+1];
      end;
    end;
  end;

procedure gen_vrot(var self:tr_bfly; rot:t_bits; sw:t_subword);
// Field variable rotate.
// Generate configuration for [inverse] butterfly network.
// To rotate right use bfly.
// To rotate left use ibfly.
// Simulate rolc for every subword.
// Bit-parallel implementation: 2011-09-14 Sigrid/Jasper Neumann
var
  t,x,y,lo: t_bits;
  i,s: t_int;
begin
  self.mask := rot;
  for i := 0 to ld_bits-1 do begin  // UNROLL
    self.cfg[i] := 0;
    end;
  case sw of
    0: begin
      // nothing else to do
      end;
    1: begin
      self.cfg[0] := rot and a_bfly_mask[0];
      end;
    else begin  // UNFOLD all cases of sw
      // this code does not work for sw<1
      // shift a single 1 to the left...
      lo := a_bfly_lo[sw];
      sw := sw-1;
      x := lo;
      for i := 0 to t_int(sw)-1 do begin  // UNROLL
        y := (rot shr i) and lo;  // rot bit to #0
        s := 1 shl i;
        // (lo_bit shl s)-1 = a_sw_base[i]
        // t := x and (y * ((lo_bit shl s) - 1));  // get offending bit(s)
        t := x and ((y shl s) - y);  // get offending bit(s)
        x := (x xor t) xor (t shl s);  // swap subwords if bit set
        end;
      // x is e.g. 1000 here (1 shl 3), we want 3 ones
      x := x-lo;  // sub 1 to yield 1-string, e.g. 1000-1=111
      y := (rot shr sw) and lo;
      s := 1 shl sw;
      // (lo_bit shl s)-1 = a_sw_base[sw]
      // x := x xor (y * ((lo_bit shl s) - 1));  // invert if rot<0
      x := x xor ((y shl s) - y);  // get offending bit(s)
      x := x and a_bfly_mask[sw];  // finalize rolc
      self.cfg[sw] := x;
      // and now for the lower stages...
      for i := t_int(sw)-1 downto 0 do begin  // UNROLL
        // xor 2 columns together to get new rolc for the stage...
        s := 1 shl i;
        x := (x xor (x shr s)) and a_bfly_mask[i];
        x := x or (x shl (s * 2));  // ...and spread into places
        self.cfg[i] := x;
        end;
      end;
    end;
  end;


//////
// Rotate via butterfly: Compound

function fror_bfly(x:t_bits; rot:t_int; sw:t_subword):t_bits;
// Rotate right all subwords by the same amount.
// 2011-09-14 Sigrid/Jasper Neumann
var
  cef: tr_bfly;
begin
  gen_frot(cef,rot,sw);
  fror_bfly := bfly(cef,x);
  end;

function frol_bfly(x:t_bits; rot:t_int; sw:t_subword):t_bits;
// Rotate left all subwords by the same amount.
// 2011-09-14 Sigrid/Jasper Neumann
var
  cef: tr_bfly;
begin
  gen_frot(cef,rot,sw);
  frol_bfly := ibfly(cef,x);
  end;

function frot_bfly(x:t_bits; rot:t_int; sw:t_subword; d:t_direction):t_bits;
// Rotate all subwords by the same amount.
// INLINE
// 2011-09-14 Sigrid/Jasper Neumann
begin
  case d of
    right: frot_bfly := fror_bfly(x,rot,sw);
    left:  frot_bfly := frol_bfly(x,rot,sw);
    else   frot_bfly := 0;  // this can't happen
    end;
  end;

function vror_bfly(x:t_bits; rot:t_bits; sw:t_subword):t_bits;
// Rotate right all subwords by a variable amount.
// 2011-09-14 Sigrid/Jasper Neumann
var
  cef: tr_bfly;
begin
  gen_vrot(cef,rot,sw);
  vror_bfly := bfly(cef,x);
  end;

function vrol_bfly(x:t_bits; rot:t_bits; sw:t_subword):t_bits;
// Rotate left all subwords by a variable amount.
// 2011-09-14 Sigrid/Jasper Neumann
var
  cef: tr_bfly;
begin
  gen_vrot(cef,rot,sw);
  vrol_bfly := ibfly(cef,x);
  end;

function vrot_bfly(x:t_bits; rot:t_bits; sw:t_subword; d:t_direction):t_bits;
// Rotate all subwords by a variable amount.
// INLINE
// 2011-09-14 Sigrid/Jasper Neumann
begin
  case d of
    right: vrot_bfly := vror_bfly(x,rot,sw);
    left:  vrot_bfly := vrol_bfly(x,rot,sw);
    else   vrot_bfly := 0;  // this can't happen
    end;
  end;

function frol(x:t_bits; rot:t_int; sw:t_subword):t_bits;
// Fast alternative to frol_bfly.
// Every (1 shl sw) bits are rotated left.
// x: value
// sw: #bits, must be <=ld_bits
// rot: rotate count
// Bit-parallel implementation: 2011-09-21 Sigrid/Jasper Neumann
// = fror(x,-rot,sw)
var
  b: t_int;  // # affected bits
  r: t_int;  // rot mod b
  m: t_bits;  // mask for affected bits
begin
  b := 1 shl sw;

  r := rot and (b-1);
  if r = 0 then begin
    // Prevent shifting by b-r >= bits.
    frol := x;
    end
  else begin
    m := a_bfly_lo[sw];
    m := (m shl r) - m;

    frol :=
      ((x shl r) and not m) or
      ((x shr (b-r)) and m);
    end;
  end;

function fror(x:t_bits; rot:t_int; sw:t_subword):t_bits;
// Fast alternative to fror_bfly.
// Every (1 shl sw) bits are rotated right.
// x: value
// sw: #bits, must be <=ld_bits
// rot: rotate count
// Bit-parallel implementation: 2011-09-21 Sigrid/Jasper Neumann
// = frol(x,-rot,sw)
var
  b: t_int;  // # affected bits
  r: t_int;  // rot mod b
  m: t_bits;  // mask for affected bits
begin
  b := 1 shl sw;

  r := (b-rot) and (b-1);
  if r = 0 then begin
    // Prevent shifting by b-r >= bits.
    fror := x;
    end
  else begin
    m := a_bfly_lo[sw];
    m := (m shl r) - m;

    fror :=
      ((x shl r) and not m) or
      ((x shr (b-r)) and m);
    end;
  end;

function frot(x:t_bits; rot:t_int; sw:t_subword; d:t_direction):t_bits;
// Fast alternative to frot_bfly.
// INLINE
// 2011-09-21 Sigrid/Jasper Neumann
begin
  case d of
    right: frot := fror(x,rot,sw);
    left:  frot := frol(x,rot,sw);
    else   frot := 0;  // this can't happen
    end;
  end;

function frolc(x:t_bits; rot:t_int; sw:t_subword):t_bits;
// Every (1 shl sw) bits are rotated left.
// x: value
// sw: #bits, must be <=ld_bits
// rot: rotate count
// Bit-parallel implementation: 2011-09-23 Sigrid/Jasper Neumann
var
  b: t_int;   // # affected bits
  r: t_int;   // rot mod b
  m: t_bits;  // mask for affected bits
begin
  b := 1 shl sw;

  r := rot and (b-1);
  if r = 0 then begin
    // Prevent shifting by b-r >= bits.
    end
  else begin
    m := a_bfly_lo[sw];
    m := (m shl r) - m;

    x :=
      ((x shl r) and not m) or
      ((x shr (b-r)) and m);

    // unil here essentially same code as frol
    x := x xor m;
    end;

  if (rot and b)<>0 then
    x := not x;

  frolc := x;
  end;

function frorc(x:t_bits; rot:t_int; sw:t_subword):t_bits;
// Every (1 shl sw) bits are rotated right.
// x: value
// sw: #bits, must be <=ld_bits
// rot: rotate count
// INLINE
begin
  frorc := frolc(x, -rot, sw);
  end;

function frotc(x:t_bits; rot:t_int; sw:t_subword; d:t_direction):t_bits;
// INLINE
begin
  case d of
    right: frotc := frorc(x,rot,sw);
    left:  frotc := frolc(x,rot,sw);
    else   frotc := 0;  // this can't happen
    end;
  end;

function vrol(x:t_bits; rot:t_bits; sw:t_subword):t_bits;
// Field variable variant of frol.
// Fast alternative to vrol_bfly.
// Gives correct results for all values of rot.
// 2014-07-14 Sigrid/Jasper Neumann
// O(ld_bits)
var
  i, s: t_int;
begin
  s := 1;
  for i := 0 to t_int(sw)-1 do begin
    // s := 1 shl i;
    x := blend(simd_odd(rot, sw), frol(x, s, sw), x);
    rot := rot shr 1;
    s := s shl 1;
    end;
  vrol := x;
  end;

function vror(x:t_bits; rot:t_bits; sw:t_subword):t_bits;
// Field variable variant of fror.
// Fast alternative to vror_bfly.
// Gives correct results for all values of rot.
// 2014-07-14 Sigrid/Jasper Neumann
// O(ld_bits)
var
  i, s: t_int;
begin
  s := 1;
  for i := 0 to t_int(sw)-1 do begin
    // s := 1 shl i;
    x := blend(simd_odd(rot, sw), fror(x, s, sw), x);
    rot := rot shr 1;
    s := s shl 1;
    end;
  vror := x;
  end;

function vrot(x:t_bits; rot:t_bits; sw:t_subword; d:t_direction):t_bits;
// Fast alternative to vrot_bfly.
// INLINE
// 2011-09-21 Sigrid/Jasper Neumann
begin
  case d of
    right: vrot := vror(x,rot,sw);
    left:  vrot := vrol(x,rot,sw);
    else   vrot := 0;  // this can't happen
    end;
  end;


//////
// Compress/expand-flip via butterfly

//////
// Compress/expand-flip via butterfly: Generate configuration

procedure gen_cef_right(var self:tr_bfly; m:t_bits; sw:t_subword);
// Scatter/gather-flip, compress/expand+flip.
// Generate configuration for [inverse] butterfly network.
// To compress use ibfly.
// To expand use bfly.
// Bit-parallel implementation: 2011-02 Sigrid/Jasper Neumann
var
  t,mm,m0: t_bits;
  i,j,s: t_int;
begin
  self.mask := m;
  for i := 0 to ld_bits-1 do begin  // UNROLL
    self.cfg[i] := 0;
    end;
  if sw > 0 then begin  // UNFOLD all cases of sw
    m := not m;
    m0 := a_bfly_lo[sw];

    for i := 0 to t_int(sw)-1 do begin  // UNROLL
      t := m;
      for j := i to t_int(sw)-1 do begin  // UNROLL
        s := 1 shl j;  // j ones; j=2: 1+not(1 shl 4): 11101111+1=11110000
        mm := not(m0 shl s)+m0;  // mask to hinder shifting into other subwords
        m := m xor ((m shl s) and mm);
        end;
      s := 1 shl i;
      m := m and a_bfly_mask[i];  // my bfly looks on low bits
      self.cfg[i] := m;
      m := (t xor (t shr s)) and m;  // do a butterfly op
      m := (t xor m) xor (m shl s);
      end;
    end;
  end;

procedure gen_cef_left(var self:tr_bfly; m:t_bits; sw:t_subword);
// Scatter/gather-flip, compress/expand+flip.
// Generate configuration for [inverse] butterfly network.
// To compress use ibfly.
// To expand use bfly.
// Bit-parallel implementation: 2011-02 Sigrid/Jasper Neumann
var
  t,mm,m0,m1: t_bits;
  i,j,s: t_int;
begin
  self.mask := m;
  for i := 0 to ld_bits-1 do begin  // UNROLL
    self.cfg[i] := 0;
    end;
  if sw > 0 then begin  // UNFOLD all cases of sw
    m := not m;
    m1 := a_bfly_lo[sw];
    m0 := (m1 shr 1) + hi_bit;  // m0 := a_bfly_hi[sw];
    for i := 0 to t_int(sw)-1 do begin  // UNROLL
      t := m;
      for j := i to t_int(sw)-1 do begin  // UNROLL
        s := 1 shl j;  // j ones; j=2: 1+not(1 shl 4): 11101111+1=11110000
        mm := (m0 shr (s-1)) - m1;  // mask to hinder shifting into other subwords
        m := m xor ((m shr s) and mm);
        end;
      s := 1 shl i;
      m := (m shr s) and a_bfly_mask[i];  // my bfly looks on low bits
      self.cfg[i] := m;  // so shift into place
      m := (t xor (t shr s)) and m;  // do a butterfly op
      m := (t xor m) xor (m shl s);
      end;
    end;
  end;


//////
// Compress/expand-flip via butterfly: Compound

function compress_flip_right(x:t_bits; m:t_bits; sw:t_subword):t_bits;
// 2011-02 Sigrid/Jasper Neumann
var
  cef: tr_bfly;
begin
  gen_cef_right(cef,m,sw);
  compress_flip_right := ibfly(cef,x);
  end;

function compress_flip_left(x:t_bits; m:t_bits; sw:t_subword):t_bits;
// 2011-02 Sigrid/Jasper Neumann
var
  cef: tr_bfly;
begin
  gen_cef_left(cef,m,sw);
  compress_flip_left := ibfly(cef,x);
  end;

function compress_flip(x:t_bits; m:t_bits; sw:t_subword; d:t_direction):t_bits;
// INLINE
// 2011-02 Sigrid/Jasper Neumann
begin
  case d of
    right: compress_flip := compress_flip_right(x,m,sw);
    left:  compress_flip := compress_flip_left(x,m,sw);
    else   compress_flip := 0;  // this can't happen
    end;
  end;

function expand_flip_right(x:t_bits; m:t_bits; sw:t_subword):t_bits;
// 2011-02 Sigrid/Jasper Neumann
var
  cef: tr_bfly;
begin
  gen_cef_right(cef,m,sw);
  expand_flip_right := bfly(cef,x);
  end;

function expand_flip_left(x:t_bits; m:t_bits; sw:t_subword):t_bits;
// 2011-02 Sigrid/Jasper Neumann
var
  cef: tr_bfly;
begin
  gen_cef_left(cef,m,sw);
  expand_flip_left := bfly(cef,x);
  end;

function expand_flip(x:t_bits; m:t_bits; sw:t_subword; d:t_direction):t_bits;
// INLINE
// 2011-02 Sigrid/Jasper Neumann
begin
  case d of
    right: expand_flip := expand_flip_right(x,m,sw);
    left:  expand_flip := expand_flip_left(x,m,sw);
    else   expand_flip := 0;  // this can't happen
    end;
  end;


//////
// Omega/flip

function omega(x:t_bits; m:t_bits; sw:t_subword):t_bits;
// Simulate one omega step/stage.
// sw: 0..ld_bits-1
// m: low bits used
// m and a_bfly_mask[sw-1] should be = m
// 2011-07-18 Sigrid/Jasper Neumann
begin
  x := butterfly(x, m, sw-1);
  x := shuffle(x,0,sw);

  omega := x;
  end;

function flip(x:t_bits; m:t_bits; sw:t_subword):t_bits;
// Simulate one flip step/stage.
// sw: 0..ld_bits-1
// m: low bits used
// m and a_bfly_mask[sw-1] should be = m
// 2011-07-18 Sigrid/Jasper Neumann
begin
  x := unshuffle(x,0,sw);
  x := butterfly(x, m, sw-1);

  flip := x;
  end;


//////
// Permutations via Benes network

type tr_benes = record
  b1,b2: tr_bfly;
  end;

procedure exchange_bit_index(var a,b:t_bit_index);
// INLINE
var
  q: t_bit_index;
begin
  q := a;
  a := b;
  b := q;
  end;

procedure gen_benes_ex(var self:tr_benes; const c_tgt:ta_index; const a_stage:ta_subword);
// Generate a configuration for the Benes network with variable stage order.
// Use benes_fwd_ex and benes_bwd_ex.
// Algorithm as sketched by Donal E. Knuth,
//   The art of computer programming, vol. 4, pre-fascicle 1a
// Implemented 2011-12-02 Sigrid/Jasper Neumann
// Modified 2012-08-31 to allow for "don't care" entries.
var
  src, inv_src: ta_index;
  tgt, inv_tgt: ta_index;
  stage: t_int;
  mask: t_int;
  cfg_src, cfg_tgt: t_bits;
  src_set: t_bits;
  main_idx, aux_idx, src_idx, tgt_idx, idx2: t_int;
  stage_idx: t_int;
  s: t_int;
begin
  for s := 0 to bits-1 do begin
    src[s] := no_index;
    tgt[s] := no_index;
    end;
  for s := 0 to bits-1 do begin
    if c_tgt[s] <> no_index then begin
      tgt[s] := s;
      src[c_tgt[s]] := s;
      end;
    end;

  invert_perm(src,inv_src);
  invert_perm(tgt,inv_tgt);
  for stage_idx := 0 to ld_bits-1 do begin
    stage := a_stage[stage_idx];
    src_set := 0;
    mask := lo_bit shl stage;
    cfg_src := 0;
    cfg_tgt := 0;
    for main_idx := 0 to bits-1 do begin  // This order to meet Waksman test
      if (main_idx and mask) = 0 then begin  // low only
        for aux_idx := 0 to 1 do begin
          src_idx := main_idx+(aux_idx shl stage);
          if ((lo_bit shl src_idx) and src_set) = 0 then begin  // yet unhandled
            if src[src_idx] <> no_index then begin  // not open

              repeat
                src_set := src_set or (lo_bit shl src_idx);
                tgt_idx := inv_tgt[src[src_idx]];
                if tgt[tgt_idx] = no_index then begin
                  BREAK;  // open end
                  end;

                if ((src_idx xor tgt_idx) and mask)=0 then begin
                  // straight
                  tgt_idx := tgt_idx xor mask;
                  end
                else begin
                  // cross
                  cfg_tgt := cfg_tgt or (lo_bit shl (tgt_idx and not mask));
                  idx2 := tgt_idx xor mask;
                  exchange_bit_index(tgt[tgt_idx],tgt[idx2]);
                  inv_tgt[tgt[idx2]] := idx2;
                  if tgt[tgt_idx] <> no_index then begin
                    inv_tgt[tgt[tgt_idx]] := tgt_idx;
                    end;
                  end;

                if tgt[tgt_idx] = no_index then begin
                  BREAK;  // open end
                  end;
                src_idx := inv_src[tgt[tgt_idx]];

                if ((src_idx xor tgt_idx) and mask)=0 then begin
                  // straight
                  src_set := src_set or (lo_bit shl src_idx);
                  src_idx := src_idx xor mask;
                  end
                else begin
                  // cross
                  cfg_src := cfg_src or (lo_bit shl (src_idx and not mask));
                  idx2 := src_idx xor mask;
                  src_set := src_set or (lo_bit shl idx2);
                  exchange_bit_index(src[src_idx],src[idx2]);
                  inv_src[src[idx2]] := idx2;
                  if src[src_idx] <> no_index then begin
                    inv_src[src[src_idx]] := src_idx;
                    end;
                  end;
                if src[src_idx] = no_index then begin
                  BREAK;  // open end
                  end;
                if ((lo_bit shl src_idx) and src_set) <> 0 then begin
                  BREAK;  // already handled
                  end;
                until false;

              end;
            end;
          end;
        end;
      end;
    self.b1.cfg[stage] := cfg_src;
    self.b2.cfg[stage] := cfg_tgt;
    end;
  // Reduce inner stages to one (not needed)
  // self.b2.cfg[0] := self.b2.cfg[0] xor self.b1.cfg[0];
  // self.b1.cfg[0] := 0;
  end;

procedure gen_benes(var self:tr_benes; const c_tgt:ta_index);
// INLINE
// Generate a configuration for the standard Benes network.
begin
  gen_benes_ex(self,c_tgt,a_stage_bwd);  // standard Benes order
  end;

function benes_fwd(const self:tr_benes; x:t_bits):t_bits;
// Apply Benes network.
// c_tgt of gen_benes selected source indexes.
begin
  benes_fwd := ibfly(self.b2,bfly(self.b1,x));
  end;

function benes_bwd(const self:tr_benes; x:t_bits):t_bits;
// Apply Benes network.
// c_tgt of gen_benes selected target indexes.
begin
  benes_bwd := ibfly(self.b1,bfly(self.b2,x));
  end;

function benes_fwd_ex(const self:tr_benes; x:t_bits; const a_stage:ta_subword):t_bits;
// Apply reordered Benes network.
// c_tgt of gen_benes_ex selected source indexes.
var
  stage_idx: t_int;
  stage: t_int;
  j: t_int;
begin
  //  benes_fwd := ibfly(self.b2,bfly(self.b1,x));
  for stage_idx := 0 to ld_bits-1 do begin
    stage := a_stage[stage_idx];
    // x := butterfly(x, self.b1.cfg[stage], sw);
    j := 1 shl stage;
    x := bit_permute_step(x, self.b1.cfg[stage], j);
    end;
  for stage_idx := ld_bits-1 downto 0 do begin
    stage := a_stage[stage_idx];
    // x := butterfly(x, self.b2.cfg[stage], sw);
    j := 1 shl stage;
    x := bit_permute_step(x, self.b2.cfg[stage], j);
    end;

  benes_fwd_ex := x;
  end;

function benes_bwd_ex(const self:tr_benes; x:t_bits; const a_stage:ta_subword):t_bits;
// Apply reordered Benes network.
// c_tgt of gen_benes_ex selected target indexes.
var
  stage_idx: t_int;
  stage: t_int;
  j: t_int;
begin
  //  benes_bwd := ibfly(self.b1,bfly(self.b2,x));
  for stage_idx := 0 to ld_bits-1 do begin
    stage := a_stage[stage_idx];
    // x := butterfly(x, self.b2.cfg[stage], sw);
    j := 1 shl stage;
    x := bit_permute_step(x, self.b2.cfg[stage], j);
    end;
  for stage_idx := ld_bits-1 downto 0 do begin
    stage := a_stage[stage_idx];
    // x := butterfly(x, self.b1.cfg[stage], sw);
    j := 1 shl stage;
    x := bit_permute_step(x, self.b1.cfg[stage], j);
    end;

  benes_bwd_ex := x;
  end;

function benes_parity(const self:tr_benes):t_bool;
// Return the parity of a permutation given by a Benes network.
// This is false for even parity and true for odd parity.
var
  stage: t_int;
  x: t_bits;
begin
  x := 0;
  for stage := 0 to ld_bits-1 do begin  // UNROLL
    x := x xor self.b1.cfg[stage] xor self.b2.cfg[stage];
    end;

  benes_parity := odd(nr_1bits(x));
  end;

// eof.
