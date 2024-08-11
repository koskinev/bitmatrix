//////
// Intro

// Some bit hacks and permutations

// (c) 2011..2020 Sigrid/Jasper Neumann
// www.sirrida.de / programming.sirrida.de
// E-Mail: info@sirrida.de

// Granted to the public domain
// First version: 2011-02
// Last change: 2014-09-16

// Compile with
// Gnu C: gcc testperm.c

// Test program for the bit fiddling procedures.

// Test stuff
//   - test_basic
//   - test_permute_step
//   - test_general_reverse
//   - test_prim_swap
//   - test_frot_bfly
//   - test_frot_bfly2
//   - test_vrot_bfly
//   - test_vrot_bfly2
//   - test_frot
//   - test_frotc
//   - test_cef_ce_right
//   - test_cef_ce_left
//   - test_compress_mask
//   - test_ce_right_sub
//   - test_ce_left_sub
//   - test_shuffle_cycles
//   - test_shuffle_by_ce
//   - test_bit_index
//   - test_shuffle_power
//   - test_bpc
//   - test_omega_flip
//   - test_benes
//   - test_parity

#include "general.c"
// Choose a working size by including the needed perm_b*.c here:
#include "perm_b32.h"
#include "perm_bas.c"

#define test_count 100000
  // # tests performed


//////
// Testing, auxiliary functions

t_bool error;

void set_error()
{
  error = true;
  // exit(1);
  }

mycall void odometer(t_longint loop) {
  if ((loop & 0x1fff) == 0) {
    printf("%i    \x0d",loop);
    fflush(0);
    }
  }

mycall void check_ok(t_bool x, const t_char* s) {

  if (!x) {
    printf("%s: Error\x0d", s);
    set_error();
    }
  }

mycall t_bool test_idx(const tr_bfly* cef, const t_int* a) {

  t_bits x,y;
  t_int n;

  x = random_bits();
  y = ibfly(cef,x);
  if (bfly(cef,y) != x) {  // bfly must be inverse of ibfly; error
    printf("random: bfly must be inverse of ibfly; error\n");
    set_error();
    return false;
    }

  for (n = 0; n <= bits-1; ++n) {
    if ( (a[n] < 0) ||
         (a[n] >= bits) ) {
      printf("gen: bit index out of bounds; error\n");
      set_error();
      return false;
      }
    x = lo_bit << n;
    y = ibfly(cef,x);
    if (bfly(cef,y) != x) {  // bfly must be inverse of ibfly; error
      printf("bits: bfly must be inverse of ibfly; error\n");
      set_error();
      return false;
      }
    if (y != lo_bit << a[n]) {  // wrong result; error
      printf("wrong result\n");
      set_error();
      return false;
      }
    }

  return true;  // all OK
  }

mycall void test_benes1(const tr_benes* self, const ta_index c_tgt) {

  t_int q;
  t_bits mask;
  t_bits x;
  ta_index c_inv_tgt;

  invert_perm(c_tgt,c_inv_tgt);

  mask = 1;
  for (q=0; q<=bits-1; ++q) {
    x = benes_fwd(self,mask);
    if (x != lo_bit << c_inv_tgt[q]) {
      printf("Error: test_benes1\n");
      set_error();
      }
    mask = mask << 1;
    }
  }

mycall void test_benes1a(const tr_benes* self, const ta_index c_tgt) {

  t_int q;
  t_bits mask;
  t_bits x;

  mask=1;
  for (q=0; q<=bits-1; ++q) {
    x=benes_fwd(self,lo_bit << c_tgt[q]);
    if (x != mask) {
      printf("Error: test_benes1a\n");
      set_error();
      }
    mask = mask << 1;
    }
  }

mycall void test_benes2(const tr_benes* self, const ta_index c_tgt) {

  t_int q;
  t_bits mask;
  t_bits x;

  mask = 1;
  for (q=0; q<=bits-1; ++q) {
    x=benes_bwd(self,mask);
    if (x != lo_bit << c_tgt[q]) {
      printf("Error: test_benes2\n");
      set_error();
      }
    mask = mask << 1;
    }
  }

mycall void test_benes3(const tr_benes* self) {

  t_int q;
  t_bits mask;
  t_bits x;

  mask = 1;
  for (q=0; q<=bits-1; ++q) {
    x = benes_fwd(self,benes_bwd(self,mask));
    if (x != mask) {
      printf("Error: test_benes3\n");
      set_error();
      }
    mask = mask << 1;
    }
  }


//////
// Testing, main functions

mycall void test_basic() {

  t_longint loop;
  t_bits x,y,q;
  t_int mode;
  t_int i;
  t_longint f;

  printf("basic\n");
  f = 1;
  for (i = 1; i <= ld_bits; ++i) {
    f = f * i;
    }
  check_ok(ld_bits_factorial == f, "ld_bits_factorial");
  for (i = 0; i <= ld_bits-1; ++i) {
    check_ok(a_stage_fwd[i] == (t_subword)(i), "a_stage_fwd");
    check_ok(a_stage_bwd[i] == (t_subword)(ld_bits-i-1), "a_stage_bwd");
    }
  check_ok(lo_bit == 1, "lo_bit");
  check_ok(rol(lo_bit, 1) == 2, "rol(1,1)");
  check_ok(rol(hi_bit, 1) == 1, "rol(hi_bit,1)");
  check_ok(rol(lo_bit, -1) == hi_bit, "rol(1,-1)");
  check_ok(rol(hi_bit, -1) == hi_bit >> 1, "rol(hi_bit,-1)");
  check_ok(nr_1bits(0) == 0, "nr_1bits(0)");
  check_ok(nr_1bits(lo_bit) == 1, "nr_1bits(lo_bit)");
  check_ok(nr_1bits(hi_bit) == 1, "nr_1bits(hi_bit)");
  check_ok(nr_1bits(all_bits) == bits, "nr_1bits(all_bits)");
  check_ok(nr_leading_0bits(0) == bits, "nr_leading_0bits(0)");
  check_ok(nr_leading_0bits(1) == bits - 1, "nr_leading_0bits(1)");
  check_ok(nr_leading_0bits(2) == bits - 2, "nr_leading_0bits(2)");
  check_ok(nr_leading_0bits(3) == bits - 2, "nr_leading_0bits(3)");
  check_ok(nr_leading_0bits(4) == bits - 3, "nr_leading_0bits(4)");
  check_ok(nr_leading_0bits(lo_bit) == bits - 1, "nr_leading_0bits(lo_bit)");
  check_ok(nr_leading_0bits(hi_bit) == 0, "nr_leading_0bits(hi_bit)");
  check_ok(nr_leading_0bits(all_bits) == 0, "nr_leading_0bits(all_bits)");
  check_ok(nr_trailing_0bits(0) == bits, "nr_trailing_0bits(0)");
  check_ok(nr_trailing_0bits(1) == 0, "nr_trailing_0bits(1)");
  check_ok(nr_trailing_0bits(2) == 1, "nr_trailing_0bits(2)");
  check_ok(nr_trailing_0bits(3) == 0, "nr_trailing_0bits(3)");
  check_ok(nr_trailing_0bits(4) == 2, "nr_trailing_0bits(4)");
  check_ok(nr_trailing_0bits(lo_bit) == 0, "nr_trailing_0bits(lo_bit)");
  check_ok(nr_trailing_0bits(hi_bit) == bits - 1, "nr_trailing_0bits(hi_bit)");
  check_ok(nr_trailing_0bits(all_bits) == 0, "nr_trailing_0bits(all_bits)");
  check_ok(is_contiguous_1bits(0), "is_contiguous_1bits(0)");
  check_ok(is_contiguous_1bits(lo_bit), "is_contiguous_1bits(lo_bit)");
  check_ok(is_contiguous_1bits(hi_bit), "is_contiguous_1bits(hi_bit)");
  check_ok(is_contiguous_1bits(all_bits), "is_contiguous_1bits(all_bits)");
  check_ok(!is_contiguous_1bits(5), "is_contiguous_1bits(5)");
  check_ok(!is_contiguous_1bits(9), "is_contiguous_1bits(9)");
  for (loop = 1; loop <= test_count; ++loop) {
    x = random_bits();
    if ((x&1)!=0) {
      check_ok((t_bits)(x*mul_inv(x)) == 1, "mul_inv");
      }
    if (gray_code(inv_gray_code(x)) != x) {
      printf("general_reverse_bits: gray_code\n");
      set_error();
      }
    if (inv_gray_code(gray_code(x)) != x) {
      printf("general_reverse_bits: gray_code\n");
      set_error();
      }
    mode = random_int(32);
    y = tbm(x, mode);
    check_ok(y == (
      tbm(x, mode & 0x11) |
      tbm(x, mode & 0x12) |
      tbm(x, mode & 0x1c)), "tbm 0");
    if ((mode & 0x08) == 0) {
      // => 0??, 1??
      // GCC might emit a wrong warning for the code below:
      // "warning: comparison of promoted ~unsigned with unsigned",
      // see https://gcc.gnu.org/bugzilla/show_bug.cgi?id=38341 .
      check_ok(y == (t_bits)(~tbm(x, mode ^ 0x07)), "tbm 1");
      check_ok(y == tbm(~x, mode ^ 0x10), "tbm 2");
      }
    else {
      // => x??, ~??
      if ((mode & 0x10) == 0)
        q = 1;
      else
        q = all_bits;  // -1
      // GCC warning: See above.
      check_ok(y == (t_bits)(~tbm(~x, mode ^ 0x13)), "tbm 3");
      check_ok(y == tbm(~x, mode ^ 0x14), "tbm 4");
      check_ok(y == tbm(x+q, mode ^ 0x10), "tbm 5");
      }
    }
  }

mycall void test_permute_step() {
// This test is best performed with ld_bits <= 16
// since otherwise not much is tested.

  t_longint loop;
  t_bits x,m,n;
  t_int s;
  t_subword k;

  printf("bit_permute_step\n");
  for (loop = test_count; loop > 0; --loop) {
    odometer(loop);
    m = random_bits();
    s = random_int(ld_bits);
    if ( (((m << s) & m) == 0) &&
         (((m << s) >> s) == m) ) {
      n = random_bits();
      if (nr_1bits(bit_permute_step_simple(n,m,s)) == nr_1bits(n)) {
        for (k = 0; k <= ld_bits-1; ++k) {
          x = lo_bit << k;
          if ((x & n) != 0) {
            if (bit_permute_step_simple(x,m,s) !=
                bit_permute_step(x,m,s)) {
              printf("permute_step: Error\n");
              set_error();
              }
            }
          }
        }
      }
    }
  }

mycall void test_general_reverse() {

  t_subword k;
  t_longint loop;
  t_int i;
  tr_bfly ce;
  t_bits x,y;

  printf("general_reverse_bits\n");
  for (loop = test_count; loop > 0; --loop) {
    odometer(loop);
    k = random_int(bits);  // 0..bits-1
    x = random_bits();

    for (i = 0; i <= ld_bits-1; ++i) {
      if ((k & (1 << i)) != 0) {
        ce.cfg[i] = a_bfly_mask[i];
        }
      else {
        ce.cfg[i] = 0;
        }
      }

    y = ibfly(&ce, x);
    if (general_reverse_bits(x,k) != y) {  // wrong result; error
      printf("general_reverse_bits: Error\n");
      set_error();
      }

    y = bfly(&ce, x);
    if (general_reverse_bits(x,k) != y) {  // wrong result; error
      printf("general_reverse_bits: Error\n");
      set_error();
      }

    if (general_reverse_bits(general_reverse_bits(x,k),k) != x) {
      printf("general_reverse_bits: Error\n");
      set_error();
      }
    }
  }

mycall void test_prim_swap() {

  t_bits m;
  t_longint loop;
  t_int i,j,k;
  tr_bfly ce;
  t_bits x,y,bit;

  printf("prim_swap\n");
  for (loop = test_count; loop > 0; --loop) {
    odometer(loop);
    m = random_bits();
    x = random_bits();

    for (i = 0; i <= ld_bits-1; ++i) {
      ce.cfg[i] = 0;
      }

    for (i = 0; i <= ld_bits-1; ++i) {
      k = 0;
      for (j = 0; j <= bits-1; ++j) {
        bit = lo_bit << j;
        if ((bit & a_prim_swap[i]) != 0) {
          if ((m & bit) != 0) {
            ce.cfg[i] = ce.cfg[i] | ((bit << 1) - (lo_bit << k));
            }
          k = k + (1 << (i+1));
          }
        }
      }

    if ((m & hi_bit) == 0) {
      y = ibfly(&ce, x);
      if (prim_swap(x,m) != y) {  // wrong result; error
        printf("prim_swap: Error\n");
        set_error();
        }
      }
    else {
      y = bfly(&ce, x);
      if (prim_swap(x,m) != y) {  // wrong result; error
        printf("prim_swap: Error\n");
        set_error();
        }
      }

    if (prim_swap(prim_swap(x,m ^ hi_bit),m) != x) {
      printf("general_reverse_bits: Error\n");
      set_error();
      }
    }
  }

mycall void test_frot_bfly() {

  tr_bfly cef;
  t_int a[bits];  // result index

  t_int rot;
  t_longint loop;
  t_int s,i;
  t_subword sw;

  printf("gen_frot\n");
  for (loop = test_count; loop > 0; --loop) {
    odometer(loop);
    sw = random_int(ld_bits+1);  // 0..ld_bits
    rot = random_int(32767);  // 0..anything fitting in t_int

    gen_frot(&cef,rot,sw);

    s = (1 << sw)-1;
    // Loop over subwords
    for (i = 0; i <= bits-1; ++i) {
      a[i] = (i & ~s) | ((i+rot) & s);
      }

    if (!test_idx(&cef,a)) {
      printf("gen_frot: Error in level %i\n",sw);
      set_error();
      }
    }
  }

mycall void test_frot_bfly2() {

  t_longint loop;
  t_bits x;
  t_int rot;
  t_subword sw;
  t_direction d;

  printf("gen_frot 2\n");
  for (loop = test_count; loop > 0; --loop) {
    odometer(loop);
    x = random_bits();
    sw = random_int(ld_bits+1);  // 0..ld_bits
    rot = random_int(32767);  // 0..anything fitting in t_int
    if (random_int(2) == 0) {
      d = right;
      }
    else {
      d = left;
      }

    if (frot_bfly(x,rot,sw,d) != frot(x,rot,sw,d)) {
      printf("frot: Error\n");
      set_error();
      }
    }
  }

mycall void test_vrot_bfly() {

  tr_bfly cef;
  t_int a[bits];  // result index

  t_bits m;
  t_longint loop;
  t_int s,i;
  t_subword sw;

  printf("gen_vrot\n");
  for (loop = test_count; loop > 0; --loop) {
    odometer(loop);
    sw = random_int(ld_bits+1);  // 0..ld_bits
    m = random_bits();

    gen_vrot(&cef,m,sw);

    s = (1 << sw)-1;
    // Loop over subwords
    for (i = 0; i <= bits-1; ++i) {
      a[i] = (i & ~s) | ((i+((t_int)(m >> (i & ~s)))) & s);
      }

    if (!test_idx(&cef,a)) {
      printf("gen_vrot: Error in level %i\n",sw);
      set_error();
      }
    }
  }

mycall void test_vrot_bfly2() {

  t_longint loop;
  t_bits x;
  t_bits rot;
  t_subword sw;
  t_direction d;

  printf("gen_vrot 2\n");
  for (loop = test_count; loop > 0; --loop) {
    odometer(loop);
    x = random_bits();
    sw = random_int(ld_bits+1);  // 0..ld_bits
    rot = random_bits();
    if (random_int(2) == 0) {
      d = right;
      }
    else {
      d = left;
      }

    if (vrot_bfly(x,rot,sw,d) != vrot(x,rot,sw,d)) {
      printf("vrot: Error\n");
      set_error();
      }
    }
  }

mycall void test_frot() {

  t_longint loop;
  t_int k;
  t_int j,b,s;
  t_subword sw;
  t_bits x,z,m;
  t_bits x1,x2;

  printf("frot\n");
  for (loop = test_count; loop > 0; --loop) {
    odometer(loop);
    x = random_bits();
    k = random_int(32767)-16384;  // anything fitting in t_int
    sw = random_int(ld_bits+1);
    z = frol(x,k,sw);
    if (z != fror(x,-k,sw)) {
      printf("fror: Error\n");
      set_error();
      }
    b = 1 << sw;
    m = a_sw_base[sw];
    for (j = (bits / (1 << sw))-1; j >= 0; j--) {
      s = b*j;
      x1 = (z >> s) & m;
      x2 = rol_lo(x >> s, k, sw);
      if (x1 != x2) {
        printf("frol: Error\n");
        set_error();
        }
      }
    }
  }

mycall void test_frotc() {

  t_longint loop;
  t_int k;
  t_int j,b,s;
  t_subword sw;
  t_bits x,z,m;
  t_bits x1,x2;

  printf("frotc\n");
  for (loop = test_count; loop > 0; --loop) {
    odometer(loop);
    x = random_bits();
    k = random_int(32767)-16384;  // anything fitting in t_int
    sw = random_int(ld_bits+1);
    z = frolc(x,k,sw);
    if (z != frorc(x,-k,sw)) {
      printf("frorc: Error\n");
      set_error();
      }
    b = 1 << sw;
    m = a_sw_base[sw];
    for (j = (bits / (1 << sw))-1; j >= 0; j--) {
      s = b*j;
      x1 = (z >> s) & m;
      x2 = rolc_lo(x >> s, k, sw);
      if (x1 != x2) {
        printf("frolc: Error\n");
        set_error();
        }
      }
    }
  }

mycall void test_cef_ce_right() {

  tr_bfly cef;
  t_int a[bits];  // result index

  t_bits m;
  t_longint loop;
  t_int s,o,i,j,k;
  t_subword sw;

  tr_bfly ce;
  t_bits x,y;

  printf("gen_cef_right\n");
  for (loop = test_count; loop > 0; --loop) {
    odometer(loop);
    sw = random_int(ld_bits+1);  // 0..ld_bits
    m = random_bits();  // my_random_a(m,sizeof(m));

    gen_cef_right(&cef,m,sw);

    // Enumerate bit positions
    s = 1 << sw;
    o = 0;
    // Loop over subwords
    for (k = 0; k <= (1 << (ld_bits-sw))-1; ++k) {
      j = 0;
      // Enumerate ones from LSB to MSB
      for (i = o; i <= o+s-1; ++i) {
        if ((m & (lo_bit << i)) != 0) {
          a[i] = j+o;
          ++j;
          }
        }
      // Enumerate zeroes from MSB to LSB
      for (i = o+s-1; i >= o; --i) {
        if ((m & (lo_bit << i)) == 0) {
          a[i] = j+o;
          ++j;
          }
        }
      o = o+s;
      }

    if (!test_idx(&cef,a)) {
      printf("gen_cef_right: Error in level %i\n",sw);
      set_error();
      }

    gen_ce_right(&ce,m,sw);
    x = random_bits();

    y = ibfly(&cef, x & m);
    if (apply_compress_right(&ce,x) != y) {  // wrong result; error
      printf("apply_compress_right: Error in level %i\n",sw);
      set_error();
      }

    y = bfly(&cef, x) & m;
    if (apply_expand_right(&ce,x) != y) {  // wrong result; error
      printf("apply_expand_right: Error in level %i\n",sw);
      set_error();
      }
    }
  }

mycall void test_cef_ce_left() {

  tr_bfly cef;
  t_int a[bits];  // result index

  t_bits m;
  t_longint loop;
  t_int s,o,i,j,k;
  t_subword sw;

  tr_bfly ce;
  t_bits x,y;

  printf("gen_cef_left\n");
  for (loop = test_count; loop > 0; --loop) {
    odometer(loop);
    sw = random_int(ld_bits+1);  // 0..ld_bits
    m = random_bits();  // my_random_a(m,sizeof(m));

    gen_cef_left(&cef,m,sw);

    // Enumerate bit positions
    s = 1 << sw;
    o = 0;
    // Loop over subwords
    for (k = 0; k <= (1 << (ld_bits-sw))-1; ++k) {
      j = 0;
      // Enumerate zeroes from MSB to LSB
      for (i = o+s-1; i >= o; --i) {
        if ((m & (lo_bit << i)) == 0) {
          a[i] = j+o;
          ++j;
          }
        }
      // Enumerate ones from LSB to MSB
      for (i = o; i <= o+s-1; ++i) {
        if ((m & (lo_bit << i)) != 0) {
          a[i] = j+o;
          ++j;
          }
        }
      o = o+s;
      }

    if (!test_idx(&cef,a)) {
      printf("gen_cef_left: Error in level %i\n",sw);
      set_error();
      }

    gen_ce_left(&ce,m,sw);
    x = random_bits();

    y = ibfly(&cef, x & m);
    if (apply_compress_left(&ce,x) != y) {  // wrong result; error
      printf("apply_compress_left: Error in level %i\n",sw);
      set_error();
      }

    y = bfly(&cef, x) & m;
    if (apply_expand_left(&ce,x) != y) {  // wrong result; error
      printf("apply_expand_left: Error in level %i\n",sw);
      set_error();
      }
    }
  }

mycall void test_compress_mask() {

  t_bits m;
  t_longint loop;
  t_subword sw;
  t_bits y,z;

  printf("compress_mask_*\n");
  for (loop = test_count; loop > 0; --loop) {
    odometer(loop);
    m = random_bits();
    sw = random_int(ld_bits+1);  // 0..ld_bits

    y = compress_mask_right(m,sw);
    z = compress_right(m,m,sw);
    if (z != y) {
      printf("compress_mask_right: Error");
      set_error();
      }

    y = compress_mask_left(m,sw);
    z = compress_left(m,m,sw);
    if (z != y) {
      printf("compress_mask_left: Error");
      set_error();
      }
    }
  }

mycall void test_ce_right_sub() {

  t_bits m;
  t_longint loop;
  t_subword sw;
  t_subword sw1;
  t_bits x,y,z,cm;

  printf("compress/expand via compress_mask_right\n");
  for (loop = test_count; loop > 0; --loop) {
    odometer(loop);
    x = random_bits();
    m = random_bits();
    do {
      sw = random_int(ld_bits+1);  // 0..ld_bits
      sw1 = random_int(ld_bits+1);  // 0..ld_bits
      } while (!(sw <= sw1));
    cm = compress_mask_right(m,sw);

    y = compress_right(x,m,sw);
    z = expand_right(compress_right(x,m,sw1),cm,sw1);
    if (z != y) {
      set_error();
      printf("compress_right via mask: Error");
      }

    y = compress_right(x,m,sw);
    z = expand_left(compress_left(x,m,sw1),cm,sw1);
    if (z != y) {
      set_error();
      printf("compress_right via mask: Error");
      }

    y = expand_right(x,m,sw);
    z = expand_right(compress_right(x,cm,sw1),m,sw1);
    if (z != y) {
      set_error();
      printf("expand_right via mask: Error");
      }

    y = expand_right(x,m,sw);
    z = expand_left(compress_left(x,cm,sw1),m,sw1);
    if (z != y) {
      set_error();
      printf("expand_right via mask: Error");
      }
    }
  }

mycall void test_ce_left_sub() {

  t_bits m;
  t_longint loop;
  t_subword sw;
  t_subword sw1;
  t_bits x,y,z,cm;

  printf("compress/expand via compress_mask_left\n");
  for (loop = test_count; loop > 0; --loop) {
    odometer(loop);
    x = random_bits();
    m = random_bits();
    do {
      sw = random_int(ld_bits+1);  // 0..ld_bits
      sw1 = random_int(ld_bits+1);  // 0..ld_bits
      } while (!(sw <= sw1));
    cm = compress_mask_left(m,sw);

    y = compress_left(x,m,sw);
    z = expand_right(compress_right(x,m,sw1),cm,sw1);
    if (z != y) {
      printf("compress_left via mask: Error");
      set_error();
      }

    y = compress_left(x,m,sw);
    z = expand_left(compress_left(x,m,sw1),cm,sw1);
    if (z != y) {
      printf("compress_left via mask: Error");
      set_error();
      }

    y = expand_left(x,m,sw);
    z = expand_right(compress_right(x,cm,sw1),m,sw1);
    if (z != y) {
      printf("expand_left via mask: Error");
      set_error();
      }

    y = expand_left(x,m,sw);
    z = expand_left(compress_left(x,cm,sw1),m,sw1);
    if (z != y) {
      printf("expand_left via mask: Error");
      set_error();
      }
    }
  }

mycall void test_shuffle_cycles() {

  t_longint loop;
  t_int j;
  t_bits x,m;
  t_subword sw1,sw2;

  printf("shuffle / unshuffle\n");
  for (loop = test_count; loop > 0; --loop) {
    odometer(loop);
    x = random_bits();
    m = x;
    do {
      sw1 = random_int(ld_bits+1);  // 0..ld_bits
      sw2 = random_int(ld_bits+1);  // 0..ld_bits
      } while (!(sw1 <= sw2));
    for (j = 1; j <= (t_int)(sw2-sw1); ++j) {
      x = shuffle(x,sw1,sw2);
      }
    if (x != m) {
      printf("shuffle: Error\n");
      set_error();
      }
    for (j = 1; j <= (t_int)(sw2-sw1); ++j) {
      x = unshuffle(x,sw1,sw2);
      }
    if (x != m) {
      printf("shuffle: Error\n");
      set_error();
      }
    }
  }

mycall void test_shuffle_by_ce() {

  t_longint loop;
  t_bits x,y1,y2;
  t_subword sw1,sw2,sw3;

  printf("[un]shuffle by CE\n");
  for (loop = test_count; loop > 0; --loop) {
    odometer(loop);
    x = random_bits();
    do {
      sw1 = random_int(ld_bits+1);  // 0..ld_bits
      sw2 = random_int(ld_bits+1);  // 0..ld_bits
      sw3 = random_int(ld_bits+1);  // 0..ld_bits
      } while (!((sw1 < sw2) && (sw2 <= sw3)));

    y1 = shuffle(x,sw1,sw2);
    y2
      = expand_right(compress_right(x,
          a_bfly_mask[sw2-1],sw3),
          a_bfly_mask[sw1],sw3)
      | expand_right(compress_right(x,
          ~a_bfly_mask[sw2-1],sw3),
          ~a_bfly_mask[sw1],sw3);
    if (y1 != y2) {
      printf("shuffle: Error\n");
      set_error();
      }
    y2
      = expand_right(compress_right(x,
          a_bfly_mask[sw2-1],sw3),
          a_bfly_mask[sw1],sw3)
      | expand_left(compress_left(x,
          ~a_bfly_mask[sw2-1],sw3),
          ~a_bfly_mask[sw1],sw3);
    if (y1 != y2) {
      printf("shuffle: Error\n");
      set_error();
      }
    y2
      = expand_left(compress_left(x,
          a_bfly_mask[sw2-1],sw3),
          a_bfly_mask[sw1],sw3)
      | expand_left(compress_left(x,
          ~a_bfly_mask[sw2-1],sw3),
          ~a_bfly_mask[sw1],sw3);
    if (y1 != y2) {
      printf("shuffle: Error\n");
      set_error();
      }

    y1 = unshuffle(x,sw1,sw2);
    y2
      = expand_right(compress_right(x,
          a_bfly_mask[sw1],sw3),
          a_bfly_mask[sw2-1],sw3)
      | expand_right(compress_right(x,
          ~a_bfly_mask[sw1],sw3),
          ~a_bfly_mask[sw2-1],sw3);
    if (y1 != y2) {
      printf("unshuffle: Error\n");
      set_error();
      }
    y2
      = expand_right(compress_right(x,
          a_bfly_mask[sw1],sw3),
          a_bfly_mask[sw2-1],sw3)
      | expand_left(compress_left(x,
          ~a_bfly_mask[sw1],sw3),
          ~a_bfly_mask[sw2-1],sw3);
    if (y1 != y2) {
      printf("unshuffle: Error\n");
      set_error();
      }
    y2
      = expand_left(compress_left(x,
          a_bfly_mask[sw1],sw3),
          a_bfly_mask[sw2-1],sw3)
      | expand_left(compress_left(x,
          ~a_bfly_mask[sw1],sw3),
          ~a_bfly_mask[sw2-1],sw3);
    if (y1 != y2) {
      printf("unshuffle: Error\n");
      set_error();
      }
    }
  }

mycall void test_bit_index() {

  t_longint loop;
  t_subword j,k;
  t_bits x,y;

  printf("Bit index functions\n");
  for (loop = test_count; loop > 0; --loop) {
    odometer(loop);
    x = random_bits();
    y = x;
    for (j = 0; j<=ld_bits-1; ++j) {
      x = bit_index_complement(x,j);
      }
    if (x != general_reverse_bits(y,bits-1)) {
      printf("bit_index_complement: Error\n");
      set_error();
      }

    y = x;
    for (j = ld_bits-1; j>=1; --j) {
      x = bit_index_swap(x,j,j-1);
      }
    if (x != shuffle(y,0,ld_bits)) {
      printf("bit_index_swap: Error\n");
      set_error();
      }

    y = x;
    j = random_int(ld_bits);  // 0..ld_bits-1
    k = random_int(ld_bits);  // 0..ld_bits-1
    x = bit_index_swap_complement(x,j,k);
    if (x != bit_index_complement(
            bit_index_complement(
              bit_index_swap(y,j,k),
            j),
          k)) {
      printf("bit_index_complement_swap: Error\n");
      set_error();
      }
    }
  }

mycall void test_bpc() {

  ta_subword perm,inv_perm;
  t_subword i, j, q;
  t_subword_set k,inv_k;
  t_longint loop;
  t_bits x,y;

  printf("BPC permutations\n");
  for (i = 0; i<=ld_bits-1; ++i) {
    perm[i] = i;
    }
  for (loop = test_count; loop > 0; --loop) {
    odometer(loop);
    for (i = 0; i<=ld_bits-1; ++i) {
      j = (t_int)(random_int(ld_bits-i))+i;
      if (j != i) {
        // swap perm[i], perm[j]
        q = perm[i];
        perm[i] = perm[j];
        perm[j] = q;
        }
      }
    k = random_int(bits);
    x = random_bits();
    y = permute_bpc(x,perm,k);
    if (y != general_reverse_bits(permute_bpc(x,perm,0),k)) {
      printf("permute_bpc: Error\n");
      set_error();
      }
    invert_bpc(perm,k,inv_perm,&inv_k);
    if (x != permute_bpc(y,inv_perm,inv_k)) {
      printf("invert_bpc: Error\n");
      set_error();
      }
    }
  }

mycall void test_shuffle_power() {

  t_longint loop;
  t_int i;
  t_bits x,y,z;
  t_subword sw_entities,ld_col,ld_row;

  printf("[un]shuffle_power\n");
  for (loop = test_count; loop > 0; --loop) {
    odometer(loop);
    x = random_bits();
    do {
      ld_row = random_int(ld_bits);  // 0..ld_bits-1
      ld_col = random_int(ld_bits);  // 0..ld_bits-1
      sw_entities = random_int(ld_bits);  // 0..ld_bits-1
      } while (!(sw_entities+ld_row+ld_col <= ld_bits));
    y = x;
    for (i = 1; i<=(t_int)(ld_col); ++i) {
      y = shuffle(y, sw_entities,ld_row+ld_col+sw_entities);
      }
    z = transpose(x, sw_entities,ld_row,ld_col);
    if (y != z) {
      printf("transpose: Error\n");
      set_error();
      }
    z = shuffle_power(x,sw_entities,ld_row+ld_col+sw_entities,ld_col);
    if (y != z) {
      printf("shuffle_power: Error\n");
      set_error();
      }
    z = unshuffle_power(z,sw_entities,ld_row+ld_col+sw_entities,ld_col);
    if (x != z) {
      printf("unshuffle_power: Error\n");
      set_error();
      }
    }
  }

mycall void test_omega_flip() {

  t_longint loop;
  t_int j,k;
  t_bits x,m1,m2,x1,x2,x3,x4;
  t_subword sw;
  t_bits am[ld_bits];  // 0..ld_bits-1

  printf("omega / flip\n");
  for (loop = test_count; loop > 0; --loop) {
    odometer(loop);
    sw = random_int(ld_bits-1)+2;  // 2..ld_bits
    for (j = 0; j <= (t_int)(sw)-1; ++j) {
      am[j] = random_bits();
      }
    x = random_bits();
    x1 = x;
    x2 = x;
    for (j = 0; j <= (t_int)(sw)-1; ++j) {
      m1 = expand_right(am[j], a_bfly_mask[sw-1], sw);
      for (k = 1; k <= j; ++k) {
        m1 = unshuffle(m1, 0, sw-1);
        }
      x3 = x1;
      x1 = flip(x1,m1,sw);
      x4 = omega(x1,m1,sw);  // omega should invert flip
      if (x4 != x3) {
        printf("omega: Error\n");
        set_error();
        }
      m2 = expand_right(am[j], a_bfly_mask[j], sw);
      x2 = butterfly(x2, m2, j);
      }
    if (x1 != x2) {
      printf("flip: Error\n");
      set_error();
      }
    }
  }

mycall void test_benes() {

  t_longint loop;
  tr_benes benes;
  ta_index c_tgt;
  t_int i;

  printf("Permutations via Benes network\n");
  for (loop = test_count/4; loop > 0; --loop) {
    odometer(loop);
    random_perm(c_tgt);
    gen_benes(&benes,c_tgt);
    if (benes.b1.cfg[0] != 0) {  // superfluous stage
      printf("test_benes: Error (stage 0)\n");
      set_error();
      }
    for (i=0; i<=ld_bits-1; ++i) {
      if ((benes.b1.cfg[i] &
          ~(a_bfly_mask[i] & ~a_bfly_lo[i+1])) != 0) {
        printf("test_benes: Error (Waksman)\n");
        set_error();
        }
      }
    test_benes1(&benes,c_tgt);
    test_benes1a(&benes,c_tgt);
    test_benes2(&benes,c_tgt);
    test_benes3(&benes);
    }
  }

mycall void test_parity() {

  t_longint loop;
  tr_benes benes;
  ta_index c_tgt;
  t_bool p1, p2;
  t_int i,j;
  t_int x;

  printf("Parity of a Benes network\n");
  identity_perm(c_tgt);
  p1 = false;
  for (loop = test_count; loop > 0; --loop) {
    odometer(loop);
    gen_benes(&benes,c_tgt);  // need target indexes for each c_tgt
    p2 = benes_parity(&benes);
    if (p1 != p2) {
      printf("test_parity: Error\n");
      set_error();
      }
    // exchange two different entries
    do {
      i = random_int(bits);  // 0..bits-1
      j = random_int(bits);  // 0..bits-1
      } while (!(i != j));
    x = c_tgt[i];
    c_tgt[i] = c_tgt[j];
    c_tgt[j] = x;
    p1 = ! p1;
    }
  }

int main(void) {

  if (!init_general()) {
    return 1;
    }

  printf("Testing (in C, bits=%i)...\n", bits);
  if (sizeof(t_bits)*8 != bits) {
    printf("Sizes wrong! sizeof(t_bits)=%i\n", (int)sizeof(t_bits));
    return 1;
    }

  my_randseed = (t_bits)(rand());

  // TODO: Test for rolc_lo
  error = false;
  test_basic();
  test_permute_step();
  test_general_reverse();
  test_prim_swap();
  test_frot_bfly();
  test_frot_bfly2();
  test_vrot_bfly();
  test_vrot_bfly2();
  test_frot();
  test_frotc();
  test_cef_ce_right();
  test_cef_ce_left();
  test_compress_mask();
  test_ce_right_sub();
  test_ce_left_sub();
  test_shuffle_cycles();
  test_shuffle_by_ce();
  test_bit_index();
  test_shuffle_power();
  test_bpc();
  test_omega_flip();
  test_benes();
  test_parity();
  printf("Program ended ");
  if (error) {
    printf("with errors.\n");
    }
  else {
    printf("successfully.\n");
    }
  return error ? 1 : 0;
  }

// eof.
