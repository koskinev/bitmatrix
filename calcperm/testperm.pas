program testperm;


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
// Delphi:      dcc32 /cc calcperm.pas
// Free Pascal: fpc -Mtp calcperm.pas
// Gnu Pascal:  gpc calcperm.pas

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


//////
// Head

(*$r-*)  // range check off
(*$q-*)  // overflow check off

const test_count = 100000;  // # tests performed

{$i general.pas }
// Choose a working size by including the needed perm_b*.pas here:
{$i perm_b32.pas }
{$i perm_bas.pas }


//////
// Testing, auxiliary functions

var
  error: t_bool;

procedure set_error;
begin
  error := true;
  // HALT(1);
  end;

procedure odometer(loop:t_longint);
begin
  if (loop and $1fff) = 0 then begin
    write(loop,'    '#$0d);
    // flush;
    end;
  end;

procedure check_ok(x:t_bool; const s:string);
begin
  if NOT x then begin
    writeln(s,': Error');
    set_error;
    end;
  end;

function test_idx(const cef:tr_bfly; const a:ta_index):t_bool;
var
  x,y: t_bits;
  n: t_int;
begin
  x := random_bits;
  y := ibfly(cef,x);
  if bfly(cef,y) <> x then begin  // bfly must be inverse of ibfly; error
    writeln('random: bfly must be inverse of ibfly; error');
    set_error;
    test_idx := false;
    EXIT;
    end;

  for n := 0 to bits-1 do begin
    if (a[n] < 0) OR (a[n] >= bits) then begin
      writeln('gen: bit index out of bounds; error');
      set_error;
      test_idx := false;
      EXIT;
      end;
    x := lo_bit shl n;
    y := ibfly(cef,x);
    if bfly(cef,y) <> x then begin  // bfly must be inverse of ibfly; error
      writeln('bits: bfly must be inverse of ibfly; error');
      set_error;
      test_idx := false;
      EXIT;
      end;
    if y <> lo_bit shl a[n] then begin  // wrong result; error
      writeln('wrong result');
      set_error;
      test_idx := false;
      EXIT;
      end;
    end;

  test_idx := true;  // all OK
  end;

procedure test_benes1(const self:tr_benes; const c_tgt:ta_index);
var
  q: t_int;
  mask: t_bits;
  x: t_bits;
  c_inv_tgt: ta_index;
begin
  invert_perm(c_tgt,c_inv_tgt);

  mask := 1;
  for q := 0 to bits-1 do begin
    x := benes_fwd(self,mask);
    if x <> lo_bit shl c_inv_tgt[q] then begin
      writeln('Error: test_benes1');
      set_error;
      end;
    mask := mask shl 1;
    end;
  end;

procedure test_benes1a(const self:tr_benes; const c_tgt:ta_index);
var
  q: t_int;
  mask: t_bits;
  x: t_bits;
begin
  mask := 1;
  for q := 0 to bits-1 do begin
    x := benes_fwd(self,lo_bit shl c_tgt[q]);
    if x <> mask then begin
      writeln('Error: test_benes1a');
      set_error;
      end;
    mask := mask shl 1;
    end;
  end;

procedure test_benes2(const self:tr_benes; const c_tgt:ta_index);
var
  q: t_int;
  mask: t_bits;
  x: t_bits;
begin
  mask := 1;
  for q := 0 to bits-1 do begin
    x := benes_bwd(self,mask);
    if x <> lo_bit shl c_tgt[q] then begin
      writeln('Error: test_benes2');
      set_error;
      end;
    mask := mask shl 1;
    end;
  end;

procedure test_benes3(const self:tr_benes);
var
  q: t_int;
  mask: t_bits;
  x: t_bits;
begin
  mask := 1;
  for q := 0 to bits-1 do begin
    x := benes_fwd(self,benes_bwd(self,mask));
    if x <> mask then begin
      writeln('Error: test_benes3');
      set_error;
      end;
    mask := mask shl 1;
    end;
  end;


//////
// Testing, main functions

procedure test_basic;
var
  loop: t_longint;
  x,y,q: t_bits;
  mode: t_int;
  i: t_int;
  f: t_longint;
begin
  writeln('basic');
  f := 1;
  for i := 1 to ld_bits do begin
    f := f * i;
    end;
  check_ok(ld_bits_factorial = f, 'ld_bits_factorial');
  for i := 0 to ld_bits-1 do begin
    check_ok(a_stage_fwd[i] = i, 'a_stage_fwd');
    check_ok(a_stage_bwd[i] = ld_bits-i-1, 'a_stage_bwd');
    end;
  check_ok(lo_bit = 1, 'lo_bit');
  check_ok(rol(lo_bit, 1) = 2, 'rol(1,1)');
  check_ok(rol(hi_bit, 1) = 1, 'rol(hi_bit,1)');
  check_ok(rol(lo_bit, -1) = hi_bit, 'rol(1,-1)');
  check_ok(rol(hi_bit, -1) = hi_bit shr 1, 'rol(hi_bit,-1)');
  check_ok(nr_1bits(0) = 0, 'nr_1bits(0)');
  check_ok(nr_1bits(lo_bit) = 1, 'nr_1bits(lo_bit)');
  check_ok(nr_1bits(hi_bit) = 1, 'nr_1bits(hi_bit)');
  check_ok(nr_1bits(all_bits) = bits, 'nr_1bits(all_bits)');
  check_ok(nr_leading_0bits(0) = bits, 'nr_leading_0bits(0)');
  check_ok(nr_leading_0bits(1) = bits - 1, 'nr_leading_0bits(1)');
  check_ok(nr_leading_0bits(2) = bits - 2, 'nr_leading_0bits(2)');
  check_ok(nr_leading_0bits(3) = bits - 2, 'nr_leading_0bits(3)');
  check_ok(nr_leading_0bits(4) = bits - 3, 'nr_leading_0bits(4)');
  check_ok(nr_leading_0bits(lo_bit) = bits - 1, 'nr_leading_0bits(lo_bit)');
  check_ok(nr_leading_0bits(hi_bit) = 0, 'nr_leading_0bits(hi_bit)');
  check_ok(nr_leading_0bits(all_bits) = 0, 'nr_leading_0bits(all_bits)');
  check_ok(nr_trailing_0bits(0) = bits, 'nr_trailing_0bits(0)');
  check_ok(nr_trailing_0bits(1) = 0, 'nr_trailing_0bits(1)');
  check_ok(nr_trailing_0bits(2) = 1, 'nr_trailing_0bits(2)');
  check_ok(nr_trailing_0bits(3) = 0, 'nr_trailing_0bits(3)');
  check_ok(nr_trailing_0bits(4) = 2, 'nr_trailing_0bits(4)');
  check_ok(nr_trailing_0bits(lo_bit) = 0, 'nr_trailing_0bits(lo_bit)');
  check_ok(nr_trailing_0bits(hi_bit) = bits - 1, 'nr_trailing_0bits(hi_bit)');
  check_ok(nr_trailing_0bits(all_bits) = 0, 'nr_trailing_0bits(all_bits)');
  check_ok(is_contiguous_1bits(0), 'is_contiguous_1bits(0)');
  check_ok(is_contiguous_1bits(lo_bit), 'is_contiguous_1bits(lo_bit)');
  check_ok(is_contiguous_1bits(hi_bit), 'is_contiguous_1bits(hi_bit)');
  check_ok(is_contiguous_1bits(all_bits), 'is_contiguous_1bits(all_bits)');
  check_ok(NOT is_contiguous_1bits(5), 'is_contiguous_1bits(5)');
  check_ok(NOT is_contiguous_1bits(9), 'is_contiguous_1bits(9)');
  for loop := 1 to test_count do begin
    x := random_bits;
    if odd(x) then begin
      check_ok(t_bits(x*mul_inv(x)) = 1, 'mul_inv');
      end;
    if gray_code(inv_gray_code(x)) <> x then begin
      writeln('general_reverse_bits: gray_code');
      set_error;
      end;
    if inv_gray_code(gray_code(x)) <> x then begin
      writeln('general_reverse_bits: gray_code');
      set_error;
      end;
    mode := random_int(32);
    y := tbm(x, mode);
    check_ok(y = (
      tbm(x, mode and $11) or
      tbm(x, mode and $12) or
      tbm(x, mode and $1c)), 'tbm 0');
    if (mode and $08) = 0 then begin
      // => 0??, 1??
      check_ok(y = t_bits(not tbm(x, mode xor $07)), 'tbm 1');
      check_ok(y = tbm(not x, mode xor $10), 'tbm 2');
      end
    else begin
      // => x??, ~??
      if (mode and $10) = 0 then
        q := 1
      else
        q := all_bits;  // -1
      check_ok(y = t_bits(not tbm(not x, mode xor $13)), 'tbm 3');
      check_ok(y = tbm(not x, mode xor $14), 'tbm 4');
      check_ok(y = tbm(x+q, mode xor $10), 'tbm 5');
      end;
    end;
  end;

procedure test_permute_step;
// This test is best performed with ld_bits <= 16
// since otherwise not much is tested.
var
  loop: t_longint;
  x,m,n: t_bits;
  s: t_int;
  k: t_subword;
begin
  writeln('bit_permute_step');
  for loop := test_count downto 1 do begin
    odometer(loop);
    m := random_bits;
    s := random_int(ld_bits);
    if (((m shl s) and m) = 0) AND
       (((m shl s) shr s) = m) then begin
      n := random_bits;
      if nr_1bits(bit_permute_step_simple(n,m,s)) = nr_1bits(n) then begin
        for k := 0 to ld_bits-1 do begin
          x := lo_bit shl k;
          if (x and n) <> 0 then begin
            if bit_permute_step_simple(x,m,s) <>
               bit_permute_step(x,m,s) then begin
              writeln('permute_step: Error');
              set_error;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

procedure test_general_reverse;
var
  k: t_subword;
  loop: t_longint;
  i: t_int;
  ce: tr_bfly;
  x,y: t_bits;
begin
  writeln('general_reverse_bits');
  for loop := test_count downto 1 do begin
    odometer(loop);
    k := random_int(bits);  // 0..bits-1
    x := random_bits;

    for i := 0 to ld_bits-1 do begin
      if (k and (1 shl i)) <> 0 then begin
        ce.cfg[i] := a_bfly_mask[i];
        end
      else begin
        ce.cfg[i] := 0;
        end;
      end;

    y := ibfly(ce, x);
    if general_reverse_bits(x,k) <> y then begin  // wrong result; error
      writeln('general_reverse_bits: Error');
      set_error;
      end;

    y := bfly(ce, x);
    if general_reverse_bits(x,k) <> y then begin  // wrong result; error
      writeln('general_reverse_bits: Error');
      set_error;
      end;

    if general_reverse_bits(general_reverse_bits(x,k),k) <> x then begin
      writeln('general_reverse_bits: Error');
      set_error;
      end;
    end;
  end;

procedure test_prim_swap;
var
  m: t_bits;
  loop: t_longint;
  i,j,k: t_int;
  ce: tr_bfly;
  x,y,bit: t_bits;
begin
  writeln('prim_swap');
  for loop := test_count downto 1 do begin
    odometer(loop);
    m := random_bits;
    x := random_bits;

    for i := 0 to ld_bits-1 do begin
      ce.cfg[i] := 0;
      end;

    for i := 0 to ld_bits-1 do begin
      k := 0;
      for j := 0 to bits-1 do begin
        bit := lo_bit shl j;
        if (bit and a_prim_swap[i]) <> 0 then begin
          if (m and bit) <> 0 then begin
            ce.cfg[i] := ce.cfg[i] or ((bit shl 1) - (lo_bit shl k));
            end;
          k := k + (1 shl (i+1));
          end;
        end;
      end;

    if (m and hi_bit) = 0 then begin
      y := ibfly(ce, x);
      if prim_swap(x,m) <> y then begin  // wrong result; error
        writeln('prim_swap: Error');
        set_error;
        end;
      end
    else begin
      y := bfly(ce, x);
      if prim_swap(x,m) <> y then begin  // wrong result; error
        writeln('prim_swap: Error');
        set_error;
        end;
      end;

    if prim_swap(prim_swap(x,m xor hi_bit),m) <> x then begin
      writeln('general_reverse_bits: Error');
      set_error;
      end;
    end;
  end;

procedure test_frot_bfly;
var
  cef: tr_bfly;
  a: ta_index;  // result index

  rot: t_int;
  loop: t_longint;
  s,i: t_int;
  sw: t_subword;
begin
  writeln('gen_frot');
  for loop := test_count downto 1 do begin
    odometer(loop);
    sw := random_int(ld_bits+1);  // 0..ld_bits
    rot := random_int(32767);  // 0..anything fitting in t_int

    gen_frot(cef,rot,sw);

    s := (1 shl sw)-1;
    // Loop over subwords
    for i := 0 to bits-1 do begin
      a[i] := (i and not s) or ((i+rot) and s);
      end;

    if NOT test_idx(cef,a) then begin
      writeln('gen_frot: Error in level ',sw);
      set_error;
      end;
    end;
  end;

procedure test_frot_bfly2;
var
  loop: t_longint;
  x: t_bits;
  rot: t_int;
  sw: t_subword;
  d: t_direction;
begin
  writeln('gen_frot 2');
  for loop := test_count downto 1 do begin
    odometer(loop);
    x := random_bits;
    sw := random_int(ld_bits+1);  // 0..ld_bits
    rot := random_int(32767)-16384;  // anything fitting in t_int
    if random_int(2)=0 then begin
      d := right;
      end
    else begin
      d := left;
      end;

    if frot_bfly(x,rot,sw,d) <> frot(x,rot,sw,d) then begin
      writeln('frot: Error');
      set_error;
      end;
    end;
  end;

procedure test_vrot_bfly;
var
  cef: tr_bfly;
  a: ta_index;  // result index

  m: t_bits;
  loop: t_longint;
  s,i: t_int;
  sw: t_subword;
begin
  writeln('gen_vrot');
  for loop := test_count downto 1 do begin
    odometer(loop);
    sw := random_int(ld_bits+1);  // 0..ld_bits
    m := random_bits;

    gen_vrot(cef,m,sw);

    s := (1 shl sw)-1;
    // Loop over subwords
    for i := 0 to bits-1 do begin
      a[i] := (i and not s) or ((i+t_int(m shr (i and not s))) and s);
      end;

    if NOT test_idx(cef,a) then begin
      writeln('gen_vrot: Error in level ',sw);
      set_error;
      end;
    end;
  end;

procedure test_vrot_bfly2;
var
  loop: t_longint;
  x: t_bits;
  rot: t_int;
  sw: t_subword;
  d: t_direction;
begin
  writeln('gen_vrot 2');
  for loop := test_count downto 1 do begin
    odometer(loop);
    x := random_bits;
    sw := random_int(ld_bits+1);  // 0..ld_bits
    rot := random_int(32767)-16384;  // anything fitting in t_int
    if random_int(2)=0 then begin
      d := right;
      end
    else begin
      d := left;
      end;

    if vrot_bfly(x,rot,sw,d) <> vrot(x,rot,sw,d) then begin
      writeln('vrot: Error');
      set_error;
      end;
    end;
  end;

procedure test_frot;
var
  loop: t_longint;
  k: t_int;
  j,b,s: t_int;
  sw: t_subword;
  x,z,m: t_bits;
  x1,x2: t_bits;
begin
  writeln('frot');
  for loop := test_count downto 1 do begin
    odometer(loop);
    x := random_bits;
    k := random_int(32767)-16384;  // anything fitting in t_int
    sw := random_int(ld_bits+1);
    z := frol(x,k,sw);
    if z <> fror(x,-k,sw) then begin
      writeln('fror: Error');
      set_error;
      end;
    b := 1 shl sw;
    m := a_sw_base[sw];
    for j := (bits div (1 shl sw))-1 downto 0 do begin
      s := b*j;
      x1 := (z shr s) and m;
      x2 := rol_lo(x shr s,k,sw);
      if x1 <> x2 then begin
        writeln('frol: Error');
        set_error;
        end;
      end;
    end;
  end;

procedure test_frotc;
var
  loop: t_longint;
  k: t_int;
  j,b,s: t_int;
  sw: t_subword;
  x,z,m: t_bits;
  x1,x2: t_bits;
begin
  writeln('frotc');
  for loop := test_count downto 1 do begin
    odometer(loop);
    x := random_bits;
    k := random_int(32767)-16384;  // anything fitting in t_int
    sw := random_int(ld_bits+1);
    z := frolc(x,k,sw);
    if z <> frorc(x,-k,sw) then begin
      writeln('frorc: Error');
      set_error;
      end;
    b := 1 shl sw;
    m := a_sw_base[sw];
    for j := (bits div (1 shl sw))-1 downto 0 do begin
      s := b*j;
      x1 := (z shr s) and m;
      x2 := rolc_lo(x shr s,k,sw);
      if x1 <> x2 then begin
        writeln('frolc: Error');
        set_error;
        end;
      end;
    end;
  end;

procedure test_cef_ce_right;
var
  cef: tr_bfly;
  a: ta_index;  // result index

  m: t_bits;
  loop: t_longint;
  s,o,i,j,k: t_int;
  sw: t_subword;

  ce: tr_bfly;
  x,y: t_bits;
begin
  writeln('gen_cef_right');
  for loop := test_count downto 1 do begin
    odometer(loop);
    sw := random_int(ld_bits+1);  // 0..ld_bits
    m := random_bits;  // my_random_a(m,sizeof(m));

    gen_cef_right(cef,m,sw);

    // Enumerate bit positions
    s := 1 shl sw;
    o := 0;
    // Loop over subwords
    for k := 0 to (1 shl (ld_bits-sw))-1 do begin
      j := 0;
      // Enumerate ones from LSB to MSB
      for i := o to o+s-1 do begin
        if (m and (lo_bit shl i)) <> 0 then begin
          a[i] := j+o;
          j := j+1;
          end;
        end;
      // Enumerate zeroes from MSB to LSB
      for i := o+s-1 downto o do begin
        if (m and (lo_bit shl i)) = 0 then begin
          a[i] := j+o;
          j := j+1;
          end;
        end;
      o := o+s;
      end;

    if NOT test_idx(cef,a) then begin
      writeln('gen_cef_right: Error in level ',sw);
      set_error;
      end;

    gen_ce_right(ce,m,sw);
    x := random_bits;

    y := ibfly(cef, x and m);
    if apply_compress_right(ce,x) <> y then begin  // wrong result; error
      writeln('apply_compress_right: Error in level ',sw);
      set_error;
      end;

    y := bfly(cef, x) and m;
    if apply_expand_right(ce,x) <> y then begin  // wrong result; error
      writeln('apply_expand_right: Error in level ',sw);
      set_error;
      end;
    end;
  end;

procedure test_cef_ce_left;
var
  cef: tr_bfly;
  a: ta_index;  // result index

  m: t_bits;
  loop: t_longint;
  s,o,i,j,k: t_int;
  sw: t_subword;

  ce: tr_bfly;
  x,y: t_bits;
begin
  writeln('gen_cef_left');
  for loop := test_count downto 1 do begin
    odometer(loop);
    sw := random_int(ld_bits+1);  // 0..ld_bits
    m := random_bits;  // my_random_a(m,sizeof(m));

    gen_cef_left(cef,m,sw);

    // Enumerate bit positions
    s := 1 shl sw;
    o := 0;
    // Loop over subwords
    for k := 0 to (1 shl (ld_bits-sw))-1 do begin
      j := 0;
      // Enumerate zeroes from MSB to LSB
      for i := o+s-1 downto o do begin
        if (m and (lo_bit shl i)) = 0 then begin
          a[i] := j+o;
          j := j+1;
          end;
        end;
      // Enumerate ones from LSB to MSB
      for i := o to o+s-1 do begin
        if (m and (lo_bit shl i)) <> 0 then begin
          a[i] := j+o;
          j := j+1;
          end;
        end;
      o := o+s;
      end;

    if NOT test_idx(cef,a) then begin
      writeln('gen_cef_left: Error in level ',sw);
      set_error;
      end;

    gen_ce_left(ce,m,sw);
    x := random_bits;

    y := ibfly(cef, x and m);
    if apply_compress_left(ce,x) <> y then begin  // wrong result; error
      writeln('apply_compress_left: Error in level ',sw);
      set_error;
      end;

    y := bfly(cef, x) and m;
    if apply_expand_left(ce,x) <> y then begin  // wrong result; error
      writeln('apply_expand_left: Error in level ',sw);
      set_error;
      end;
    end;
  end;

procedure test_compress_mask;
var
  m: t_bits;
  loop: t_longint;
  sw: t_subword;
  y,z: t_bits;
begin
  writeln('compress_mask_*');
  for loop := test_count downto 1 do begin
    odometer(loop);
    m := random_bits;
    sw := random_int(ld_bits+1);  // 0..ld_bits

    y := compress_mask_right(m,sw);
    z := compress_right(m,m,sw);
    if z <> y then begin
      writeln('compress_mask_right: Error');
      set_error;
      end;

    y := compress_mask_left(m,sw);
    z := compress_left(m,m,sw);
    if z <> y then begin
      writeln('compress_mask_left: Error');
      set_error;
      end;
    end;
  end;

procedure test_ce_right_sub;
var
  m: t_bits;
  loop: t_longint;
  sw: t_subword;
  sw1: t_subword;
  x,y,z,cm: t_bits;
begin
  writeln('compress/expand via compress_mask_right');
  for loop := test_count downto 1 do begin
    odometer(loop);
    x := random_bits;
    m := random_bits;
    repeat
      sw := random_int(ld_bits+1);  // 0..ld_bits
      sw1 := random_int(ld_bits+1);  // 0..ld_bits
      until sw <= sw1;
    cm := compress_mask_right(m,sw);

    y := compress_right(x,m,sw);
    z := expand_right(compress_right(x,m,sw1),cm,sw1);
    if z <> y then begin
      writeln('compress_right via mask: Error');
      set_error;
      end;

    y := compress_right(x,m,sw);
    z := expand_left(compress_left(x,m,sw1),cm,sw1);
    if z <> y then begin
      writeln('compress_right via mask: Error');
      set_error;
      end;

    y := expand_right(x,m,sw);
    z := expand_right(compress_right(x,cm,sw1),m,sw1);
    if z <> y then begin
      writeln('expand_right via mask: Error');
      set_error;
      end;

    y := expand_right(x,m,sw);
    z := expand_left(compress_left(x,cm,sw1),m,sw1);
    if z <> y then begin
      writeln('expand_right via mask: Error');
      set_error;
      end;
    end;
  end;

procedure test_ce_left_sub;
var
  m: t_bits;
  loop: t_longint;
  sw: t_subword;
  sw1: t_subword;
  x,y,z,cm: t_bits;
begin
  writeln('compress/expand via compress_mask_left');
  for loop := test_count downto 1 do begin
    odometer(loop);
    x := random_bits;
    m := random_bits;
    repeat
      sw := random_int(ld_bits+1);  // 0..ld_bits
      sw1 := random_int(ld_bits+1);  // 0..ld_bits
      until sw <= sw1;
    cm := compress_mask_left(m,sw);

    y := compress_left(x,m,sw);
    z := expand_right(compress_right(x,m,sw1),cm,sw1);
    if z <> y then begin
      writeln('compress_left via mask: Error');
      set_error;
      end;

    y := compress_left(x,m,sw);
    z := expand_left(compress_left(x,m,sw1),cm,sw1);
    if z <> y then begin
      writeln('compress_left via mask: Error');
      set_error;
      end;

    y := expand_left(x,m,sw);
    z := expand_right(compress_right(x,cm,sw1),m,sw1);
    if z <> y then begin
      writeln('expand_left via mask: Error');
      set_error;
      end;

    y := expand_left(x,m,sw);
    z := expand_left(compress_left(x,cm,sw1),m,sw1);
    if z <> y then begin
      writeln('expand_left via mask: Error');
      set_error;
      end;
    end;
  end;

procedure test_shuffle_cycles;
var
  loop: t_longint;
  j: t_int;
  x,m: t_bits;
  sw1,sw2: t_subword;
begin
  writeln('shuffle / unshuffle');
  for loop := test_count downto 1 do begin
    odometer(loop);
    x := random_bits;
    m := x;
    repeat
      sw1 := random_int(ld_bits+1);  // 0..ld_bits
      sw2 := random_int(ld_bits+1);  // 0..ld_bits
      until sw1 <= sw2;
    for j := 1 to t_int(sw2)-t_int(sw1) do begin
      x := shuffle(x,sw1,sw2);
      end;
    if x <> m then begin
      writeln('shuffle: Error');
      set_error;
      end;
    for j := 1 to t_int(sw2)-t_int(sw1) do begin
      x := unshuffle(x,sw1,sw2);
      end;
    if x <> m then begin
      writeln('unshuffle: Error');
      set_error;
      end;
    end;
  end;

procedure test_shuffle_by_ce;
var
  loop: t_longint;
  x,y1,y2: t_bits;
  sw1,sw2,sw3: t_subword;
begin
  writeln('[un]shuffle by CE');
  for loop := test_count downto 1 do begin
    odometer(loop);
    x := random_bits;
    repeat
      sw1 := random_int(ld_bits+1);  // 0..ld_bits
      sw2 := random_int(ld_bits+1);  // 0..ld_bits
      sw3 := random_int(ld_bits+1);  // 0..ld_bits
      until ((sw1 < sw2) AND (sw2 <= sw3));

    y1 := shuffle(x,sw1,sw2);
    y2
      := expand_right(compress_right(x,
        a_bfly_mask[sw2-1],sw3),
        a_bfly_mask[sw1],sw3)
      or expand_right(compress_right(x,
        not a_bfly_mask[sw2-1],sw3),
        not a_bfly_mask[sw1],sw3);
    if y1 <> y2 then begin
      writeln('shuffle: Error');
      set_error;
      end;
    y2
      := expand_right(compress_right(x,
        a_bfly_mask[sw2-1],sw3),
        a_bfly_mask[sw1],sw3)
      or expand_left(compress_left(x,
        not a_bfly_mask[sw2-1],sw3),
        not a_bfly_mask[sw1],sw3);
    if y1 <> y2 then begin
      writeln('shuffle: Error');
      set_error;
      end;
    y2
      := expand_left(compress_left(x,
        a_bfly_mask[sw2-1],sw3),
        a_bfly_mask[sw1],sw3)
      or expand_left(compress_left(x,
        not a_bfly_mask[sw2-1],sw3),
        not a_bfly_mask[sw1],sw3);
    if y1 <> y2 then begin
      writeln('shuffle: Error');
      set_error;
      end;

    y1 := unshuffle(x,sw1,sw2);
    y2
      := expand_right(compress_right(x,
        a_bfly_mask[sw1],sw3),
        a_bfly_mask[sw2-1],sw3)
      or expand_right(compress_right(x,
        not a_bfly_mask[sw1],sw3),
        not a_bfly_mask[sw2-1],sw3);
    if y1 <> y2 then begin
      writeln('unshuffle: Error');
      set_error;
      end;
    y2
      := expand_right(compress_right(x,
        a_bfly_mask[sw1],sw3),
        a_bfly_mask[sw2-1],sw3)
      or expand_left(compress_left(x,
        not a_bfly_mask[sw1],sw3),
        not a_bfly_mask[sw2-1],sw3);
    if y1 <> y2 then begin
      writeln('unshuffle: Error');
      set_error;
      end;
    y2
      := expand_left(compress_left(x,
        a_bfly_mask[sw1],sw3),
        a_bfly_mask[sw2-1],sw3)
      or expand_left(compress_left(x,
        not a_bfly_mask[sw1],sw3),
        not a_bfly_mask[sw2-1],sw3);
    if y1 <> y2 then begin
      writeln('unshuffle: Error');
      set_error;
      end;
    end;
  end;

procedure test_bit_index;
var
  loop: t_longint;
  j,k: t_subword;
  x,y: t_bits;
begin
  writeln('Bit index functions');
  for loop := test_count downto 1 do begin
    odometer(loop);
    x := random_bits;
    y := x;
    for j := 0 to ld_bits-1 do begin
      x := bit_index_complement(x,j);
      end;
    if x <> general_reverse_bits(y,bits-1) then begin
      writeln('bit_index_complement: Error');
      set_error;
      end;

    y := x;
    for j := ld_bits-1 downto 1 do begin
      x := bit_index_swap(x,j,j-1);
      end;
    if x <> shuffle(y,0,ld_bits) then begin
      writeln('bit_index_swap: Error');
      set_error;
      end;

    y := x;
    j := random_int(ld_bits);  // 0..ld_bits-1
    k := random_int(ld_bits);  // 0..ld_bits-1
    x := bit_index_swap_complement(x,j,k);
    if x <> bit_index_complement(
            bit_index_complement(
              bit_index_swap(y,j,k),
            j),
          k) then begin
      writeln('bit_index_complement_swap: Error');
      set_error;
      end;
    end;
  end;

procedure test_bpc;
var
  perm,inv_perm: ta_subword;
  i, j, q: t_subword;
  k, inv_k: t_subword_set;
  loop: t_longint;
  x,y: t_bits;
begin
  writeln('BPC permutations');
  for i := 0 to ld_bits-1 do begin
    perm[i] := i;
    end;
  for loop := test_count downto 1 do begin
    odometer(loop);
    for i := 0 to ld_bits-1 do begin
      j := t_int(random_int(ld_bits-i))+i;
      if j <> i then begin
        // swap perm[i], perm[j]
        q := perm[i];
        perm[i] := perm[j];
        perm[j] := q;
        end;
      end;
    k := random_int(bits);
    x := random_bits;
    y := permute_bpc(x,perm,k);
    if y <> general_reverse_bits(permute_bpc(x,perm,0),k) then begin
      writeln('permute_bpc: Error');
      set_error;
      end;
    invert_bpc(perm,k,inv_perm,inv_k);
    if x <> permute_bpc(y,inv_perm,inv_k) then begin
      writeln('invert_bpc: Error');
      set_error;
      end;
    end;
  end;

procedure test_shuffle_power;
var
  loop: t_longint;
  i: t_int;
  x,y,z: t_bits;
  sw_entities,ld_col,ld_row: t_subword;
begin
  writeln('[un]shuffle_power');
  for loop := test_count downto 1 do begin
    odometer(loop);
    x := random_bits;
    repeat
      ld_row := random_int(ld_bits);  // 0..ld_bits-1
      ld_col := random_int(ld_bits);  // 0..ld_bits-1
      sw_entities := random_int(ld_bits);  // 0..ld_bits-1
      until sw_entities+ld_row+ld_col <= ld_bits;
    y := x;
    for i := 1 to ld_col do begin
      y := shuffle(y, sw_entities,ld_row+ld_col+sw_entities);
      end;
    z := transpose(x, sw_entities,ld_row,ld_col);
    if y <> z then begin
      writeln('transpose: Error');
      set_error;
      end;
    z := shuffle_power(x,sw_entities,ld_row+ld_col+sw_entities,ld_col);
    if y <> z then begin
      writeln('shuffle_power: Error');
      set_error;
      end;
    z := unshuffle_power(z,sw_entities,ld_row+ld_col+sw_entities,ld_col);
    if x <> z then begin
      writeln('unshuffle_power: Error');
      set_error;
      end;
    end;
  end;

procedure test_omega_flip;
var
  loop: t_longint;
  i,j: t_int;
  x,m1,m2,x1,x2,x3,x4: t_bits;
  sw: t_subword;
  am: array [0..ld_bits-1] of t_bits;
begin
  writeln('omega / flip');
  for loop := test_count downto 1 do begin
    odometer(loop);
    sw := random_int(ld_bits-1)+2;  // 2..ld_bits
    for i := 0 to t_int(sw)-1 do begin
      am[i] := random_bits;
      end;
    x := random_bits;
    x1 := x;
    x2 := x;
    for i := 0 to t_int(sw)-1 do begin
      m1 := expand_right(am[i], a_bfly_mask[sw-1], sw);
      for j := 1 to i do begin
        m1 := unshuffle(m1, 0, sw-1);
        end;
      x3 := x1;
      x1 := flip(x1,m1,sw);
      x4 := omega(x1,m1,sw);  // omega should invert flip
      if x4 <> x3 then begin
        writeln('omega: Error');
        set_error;
        end;
      m2 := expand_right(am[i], a_bfly_mask[i], sw);
      x2 := butterfly(x2, m2, i);
      end;
    if x1 <> x2 then begin
      writeln('flip: Error');
      set_error;
      end;
    end;
  end;

procedure test_benes;
var
  loop: t_longint;
  benes: tr_benes;
  c_tgt: ta_index;
  i: t_int;
begin
  writeln('Permutations via Benes network');
  for loop := test_count div 4 downto 1 do begin
    odometer(loop);
    random_perm(c_tgt);
    gen_benes(benes,c_tgt);  // need target indexes for each c_tgt
    if benes.b1.cfg[0] <> 0 then begin  // superfluous stage
      writeln('test_benes: Error (stage 0)');
      set_error;
      end;
    for i := 0 to ld_bits-1 do begin
      if (benes.b1.cfg[i] and
          not(a_bfly_mask[i] and not a_bfly_lo[i+1])) <> 0 then begin
        writeln('test_benes: Error (Waksman)');
        set_error;
        end;
      end;
    test_benes1(benes,c_tgt);
    test_benes1a(benes,c_tgt);
    test_benes2(benes,c_tgt);
    test_benes3(benes);
    end;
  end;

procedure test_parity;
var
  loop: t_longint;
  benes: tr_benes;
  c_tgt: ta_index;
  p1, p2: t_bool;
  i,j: t_int;
  x: t_int;
begin
  writeln('Parity of a Benes network');
  identity_perm(c_tgt);
  p1 := false;
  for loop := test_count downto 1 do begin
    odometer(loop);
    gen_benes(benes,c_tgt);  // need target indexes for each c_tgt
    p2 := benes_parity(benes);
    if p1 <> p2 then begin
      writeln('test_parity: Error');
      set_error;
      end;
    // exchange two different entries
    repeat
      i := random_int(bits);  // 0..bits-1
      j := random_int(bits);  // 0..bits-1
      until i <> j;
    x := c_tgt[i];
    c_tgt[i] := c_tgt[j];
    c_tgt[j] := x;
    p1 := NOT p1;
    end;
  end;


//////
// Main program

begin
  if NOT init_general then begin
    HALT(1);
    end;

  writeln('Testing (in Pascal, bits=',bits,')...');
  if sizeof(t_bits)*8 <> bits then begin
    writeln('Sizes wrong! sizeof(t_bits)=', sizeof(t_bits));
    HALT(1);
    end;

  my_randseed := random(32767);

  // TODO: Test for rolc_lo
  error := false;
  test_basic;
  test_permute_step;
  test_general_reverse;
  test_prim_swap;
  test_frot_bfly;
  test_frot_bfly2;
  test_vrot_bfly;
  test_vrot_bfly2;
  test_frot;
  test_frotc;
  test_cef_ce_right;
  test_cef_ce_left;
  test_compress_mask;
  test_ce_right_sub;
  test_ce_left_sub;
  test_shuffle_cycles;
  test_shuffle_by_ce;
  test_bit_index;
  test_shuffle_power;
  test_bpc;
  test_omega_flip;
  test_benes;
  test_parity;
  write('Program ended ');
  if error then begin
    writeln('with errors.');
    HALT(1);
    end
  else begin
    writeln('successfully.');
    HALT(0);
    end;
  end.

// eof.
