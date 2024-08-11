program calcperm;


//////
// Intro

// Bit permutation code generator

// (c) 2011..2020 Sigrid/Jasper Neumann
// www.sirrida.de / programming.sirrida.de
// E-Mail: info@sirrida.de

// Granted to the public domain
// First version: 2012-08-31
// Last change: 2013-03-26

// Compile with
// Delphi:      dcc32 /cc calcperm.pas
// Free Pascal: fpc -Mtp calcperm.pas
// Gnu Pascal:  gpc calcperm.pas


//////
// Head

(*$r-*)  // range check off
(*$q-*)  // overflow check off

{$i general.pas }
// Choose a working size by including the needed perm_b*.pas here:
{$i perm_b32.pas }
{$i perm_bas.pas }

const
  nr_hex_digits = bits shr 2;  // each hexdigit encodes 4 bits
type
  t_string = string;


//////
// Configuration parameter variables

var
  options: record
    // Output options
    dump_input: t_bool;
    dump_inverse: t_bool;
    brief: t_bool;
    verbose: t_bool;

    // Language dependence
    comment_prefix: t_string;
    comment_postfix: t_string;
    hex_prefix: t_string;
    hex_postfix: t_string;
    op_assign: t_string;
    op_and: t_string;
    op_or: t_string;
    op_xor: t_string;
    op_shl: t_string;
    op_shr: t_string;

    // Function names
    op_pstep: t_string;
    op_pstep_simple: t_string;
    op_rol: t_string;
    op_gather: t_string;
    op_scatter: t_string;
    op_bswap: t_string;

    // Input options
    in_origin: t_int;  // input index origin
    in_base: t_int;  // input number base
    in_indexes_are_target: t_bool;  // /in_indexes=source or target

    // General costs
    cost_rotate_shift: t_int;
    cost_bool: t_int;
    cost_bswap: t_int;
    cost_mul: t_int;
    cost_gs: t_int;
    cost_mask: t_int;

    // Special costs
    cost_rotate: t_int;
    cost_shift: t_int;
    cost_and: t_int;
    cost_or: t_int;
    cost_xor: t_int;
    cost_scatter: t_int;
    cost_gather: t_int;
    cost_bit_permute_step: t_int;
    cost_bit_permute_step_simple: t_int;

    // Superscalar boni
    bonus_bit_permute_step: t_int;
    bonus_bit_permute_step_simple: t_int;
    bonus_gs: t_int;
    bonus_mask_rol: t_int;
    bonus_gs_rol: t_int;

    // Calculation options
    allow_bswap: t_bool;
    allow_bmi: t_bool;
    test_bpc: t_bool;
    test_bfly: t_bool;
    test_ibfly: t_bool;
    test_benes: t_bool;
    test_bit_groups: t_bool;
    test_mul: t_bool;
    test_gather_scatter: t_bool;
    test_gather_shift: t_bool;
    test_gather_shift_sloppy: t_bool;
    test_shift_scatter: t_bool;
    test_shift_scatter_sloppy: t_bool;
    test_sag: t_bool;
    test_merge: t_bool;
    opt_gs: t_bool;
    opt_rol: t_bool;
    opt_rol_ex: t_bool;
    opt_bswap: t_bool;

    self_test: t_bool;
    end;

procedure error_abort;
begin
  writeln('ERROR');
  HALT(1);
  end;


//////
// Auxiliary routines

function min(a,b:t_longint):t_longint;
begin
  if a < b then begin
    min := a;
    end
  else begin
    min := b;
    end;
  end;

function max(a,b:t_longint):t_longint;
begin
  if a > b then begin
    max := a;
    end
  else begin
    max := b;
    end;
  end;

function cl_mul(x,y:t_bits):t_bits;
var
  res: t_bits;
begin
  res := 0;
  while x <> 0 do begin
    if odd(x) then begin
      res:=res xor y;
      end;
    y := y shl 1;
    x := x shr 1;
    end;
  cl_mul := res;
  end;

function mul_is_carryless(x,y:t_bits):t_bool;
var
  res: t_bool;
  q: t_bits;
begin
  res := true;
  q := 0;
  while x <> 0 do begin
    if odd(x) then begin
      if (q and y) <> 0 then begin
        res := false;
        BREAK;
        end;
      q := q or y;
      end;
    y := y shl 1;
    x := x shr 1;
    end;
  mul_is_carryless := res;
  end;

function carries_of_mul(x,y:t_bits):t_bits;
var
  res: t_bits;
  q: t_bits;
begin
  res := 0;
  q := 0;
  while x <> 0 do begin
    if odd(x) then begin
      res := res or (q and y);
      q := q or y;
      end;
    y := y shl 1;
    x := x shr 1;
    end;
  carries_of_mul := res;
  end;

function mask_hi(x:t_bits):t_bits;
// Mask all but highest 0 bit.
var
  res: t_bits;
begin
  repeat
    res := x;
    x := x or (x+1);  // turn on rightmost bit
    until x = all_bits;
  mask_hi := res;
  end;

function pdep(x,m:t_bits):t_bits;
// scatter
begin
  pdep := expand_right(x,m,ld_bits);
  end;

function pext(x,m:t_bits):t_bits;
// gather
begin
  pext := compress_right(x,m,ld_bits);
  end;

function locase(x:t_char):t_char;
begin
  if ('A'<=x) AND
     (x<='Z') then begin
    x := chr(ord(x)-(ord('a')-ord('A')));
    end;
  locase := x;
  end;

function lostr(const x:t_string):t_string;
var
  i: t_int;
  res: t_string;
begin
  res := '';
  for i:=1 to length(x) do begin
    res := res+locase(x[i]);
    end;
  lostr := res;
  end;

function upcase(x:t_char):t_char;
begin
  if ('a'<=x) AND
     (x<='z') then begin
    x := chr(ord(x)-(ord('A')-ord('a')));
    end;
  upcase := x;
  end;

function upstr(const x:t_string):t_string;
var
  i: t_int;
  res: t_string;
begin
  res := '';
  for i:=1 to length(x) do begin
    res := res+upcase(x[i]);
    end;
  upstr := res;
  end;

function num(x:t_int):t_string;
// Convert an integer to a string containing the decimal representation.
var
  res: t_string;
  neg: t_bool;
begin
  // num := inttostr(x);
  if x = 0 then begin
    res := '0';
    end
  else begin
    neg := x < 0;
    if neg then begin
      x := -x;
      end;
    res := '';
    while x <> 0 do begin
      res := chr((x mod 10)+ord('0'))+res;
      x := x div 10;
      end;
    if neg then begin
      res := '-'+res;
      end;
    end;
  num := res;
  end;

function hex(x:t_bits):t_string;
// Convert a positive integer to a string
// containing the hexadecimal representation.
const
  digits: array [0..15] of char = '0123456789abcdef';
var
  res: t_string;
  i: t_int;
begin
  // hex := hex_prefix+inttohex(x,nr_hex_digits)+hex_postfix;
  res := '';
  for i := 1 to nr_hex_digits do begin
    res := digits[x and 15]+res;
    x := x shr 4;
    end;
  hex := options.hex_prefix+res;
  end;

function q2s(x:t_bool):t_string;
begin
  if x then begin
    q2s:='true';
    end
  else begin
    q2s:='false';
    end;
  end;

function s2i(const s:t_string):t_longint;
// Convert a string to an integer.
// Simply halts the program in case of an error
// since we use no exception mechanism for simplicity.
var
  len: t_int;
  i: t_int;
  q: t_int;
  v: t_longint;
  neg: t_bool;
begin
  len := length(s);
  // Trim trailing spaces
  while (len > 0) AND
        (s[len] = ' ') do begin
    len := len - 1;
    end;
  q := 1;
  // Trim leading spaces
  while (q <= len) AND
        (s[q] = ' ') do begin
    q := q + 1;
    end;
  neg := false;
  // Accept one "-" char
  if (q <= len) AND
     (s[q] = '-') then begin
    neg := true;
    q := q + 1;
    // Trim more spaces
    while (q <= len) AND
          (s[q] = ' ') do begin
      q := q + 1;
      end;
    end;
  v := 0;
  for i := q to len do begin
    if s[i] = '_' then begin
      // Skip "_" chars
      end
    else if (s[i] >= '0') AND
            (s[i] <= '9') then begin
      v := v*10 + (ord(s[i]) - ord('0'));
      end
    else begin
      writeln('ERROR: Number expected in '+s);
      error_abort;
      end;
    end;
  if neg then begin
    v := -v;
    end;
  s2i := v;
  end;

const
  max_pprim = bits;
  max_pstep = ld_bits * 2 + 3;  // one each extra rol/bswap, one spare
type

  tq_prim_kind = (
    pk_none,
    pk_permute,
    pk_permute_simple,
    pk_mask,
    pk_mask_rol,
    pk_mask_shift,
    pk_rol,
    pk_mul,
    pk_gather_scatter,
    pk_gather,
    pk_gather_shift,
    pk_scatter,
    pk_shift_scatter,
    pk_bswap);

  tr_pprim = record
    // one permutation substep, these are ORed together
    kind: tq_prim_kind;
    mask: t_bits;   // permute, masked_rol, gather
    mask2: t_bits;  // scatter, mul
    mul: t_bits;  // mul
    rol: t_int;  // rotate/shift count
    rol2: t_int;  // mul (not yet used)
    end;

  tr_pstep = record
    // one permutation step
    description: t_string;
    needed_src_bits: t_bits;
    resulting_src_bits: t_bits;
    nr_pprim: t_int;
    a_pprim: array [0..max_pprim-1] of tr_pprim;
    end;

  tr_imp = record
    // complete permutation
    perm: ta_index;
    description: t_string;
    nr_step: t_int;
    a_step: array [0..max_pstep-1] of tr_pstep;
    needed_src_bits: t_bits;
    is_bfly: t_bool;  // used in route_benes
    end;

  tr_gather_scatter = record
    // for routing gather/scatter and derivatives
    mask_src,mask_tgt: t_bits;
    end;

  tar_gather_scatter = array [0..bits-1] of tr_gather_scatter;

  tr_performance = record
    cost: t_int;
    cmask: t_int;
    end;

var
  pre_imp: tr_imp;
  post_imp: tr_imp;

procedure imp_dump(const self:tr_imp);  forward;
procedure dump_perm(const perm:ta_index);  forward;


//////
// Evaluating routines

function pprim_eval(const self:tr_pprim; x:t_bits):t_bits;
var
  res: t_bits;
begin
  case self.kind of
    pk_none: res := x;
    pk_permute: res := bit_permute_step(x, self.mask, self.rol);
    pk_permute_simple: res := bit_permute_step_simple(x, self.mask, self.rol);
    pk_rol: res := rol(x, self.rol);
    pk_mask: res := x and self.mask;
    pk_mask_rol: res := rol(x and self.mask, self.rol);
    pk_mask_shift: begin
      if self.rol >= 0 then begin
        res := (x and self.mask) shl self.rol;
        end
      else begin
        res := (x and self.mask) shr (-self.rol);
        end;
      end;
    pk_mul: res := rol(t_bits((rol(x, self.rol) and self.mask)*self.mul) and self.mask2, self.rol2);
    pk_gather_scatter: res := pdep(pext(x, self.mask), self.mask2);
    pk_gather: res := pext(x, self.mask);
    pk_gather_shift: res := (pext(x, self.mask) shl self.rol);
    pk_scatter: res := pdep(x, self.mask2);
    pk_shift_scatter: res := pdep(x shr ((-self.rol) and (bits-1)), self.mask2);
    pk_bswap: res := bswap(x);
    else begin
      res := 0;
      writeln('ERROR: pprim_eval: unknown kind '+num(ord(self.kind)));
      error_abort;
      end;
    end;
  pprim_eval := res;
  end;

function pstep_eval(const self:tr_pstep; x:t_bits):t_bits;
var
  i: t_int;
  res: t_bits;
begin
  if self.nr_pprim = 0 then begin
    res := x;
    end
  else begin
    res := pprim_eval(self.a_pprim[0], x);
    for i := 1 to self.nr_pprim-1 do begin
      res := res or pprim_eval(self.a_pprim[i], x);
      end;
    end;
  pstep_eval := res;
  end;

function imp_eval(const self:tr_imp; x:t_bits):t_bits;
var
  i: t_int;
  res: t_bits;
begin
  res := x;
  for i := 0 to self.nr_step-1 do begin
    res := pstep_eval(self.a_step[i], res);
    end;
  imp_eval := res;
  end;

procedure imp_check(const imp:tr_imp);
var
  q: t_int;
  mask: t_bits;
  x: t_bits;
  error: t_bool;
  inv_perm: ta_index;
begin
  error := false;
  mask := 1;
  for q := 0 to bits-1 do begin
    if imp.perm[q] <> no_index then begin
      x := lo_bit shl imp.perm[q];
      x := imp_eval(imp, x);
      if x <> mask then begin
        error := true;
        BREAK;
        end;
      end;
    mask := mask shl 1;
    end;
  if error then begin
    writeln('ERROR (mask)');
    dump_perm(imp.perm);
    imp_dump(imp);
    invert_perm(imp.perm,inv_perm);
    mask := 1;
    for q := 0 to bits-1 do begin
      x := imp_eval(imp, mask);
      write(hex(mask)+' => '+hex(x));
      if (inv_perm[q] <> no_index) AND
         (lo_bit shl inv_perm[q] <> x) then begin
        writeln(' ERROR, expected: '+hex(lo_bit shl inv_perm[q]));
        end
      else begin
        writeln(' OK');
        end;
      mask := mask shl 1;
      end;
    error_abort;
    end;
  if imp.nr_step <> 0 then begin
    if imp_eval(imp, imp.needed_src_bits) <>
       imp.a_step[imp.nr_step-1].resulting_src_bits then begin
      writeln('ERROR (needed bits defective)');
      dump_perm(imp.perm);
      imp_dump(imp);
      error_abort;
      end;
    end;
  end;


//////
// Init routines

procedure init_gather_scatter(var self:tr_gather_scatter);
begin
  self.mask_src := 0;
  self.mask_tgt := 0;
  end;

procedure init_pprim(var self:tr_pprim);
begin
  self.kind := pk_none;
  self.mask := 0;
  self.mask2 := 0;
  self.mul := 0;
  self.rol := 0;
  self.rol2 := 0;
  end;

procedure init_pstep(var self:tr_pstep);
var
  i: t_int;
begin
  self.description := '';
  self.needed_src_bits := 0;
  self.resulting_src_bits := 0;
  self.nr_pprim := 0;
  for i := 0 to max_pprim-1 do begin
    init_pprim(self.a_pprim[i]);
    end;
  end;

procedure init_imp0(var self:tr_imp; const perm:ta_index);
var
  needed: t_bits;
  i: t_int;
begin
  needed := used_source_bits(perm);
  self.perm := perm;
  self.description := '';
  self.nr_step := 0;
  for i := 0 to max_pstep-1 do begin
    init_pstep(self.a_step[i]);
    end;
  self.needed_src_bits := needed;
  self.is_bfly := false;
  self.a_step[0].needed_src_bits := needed;
  end;

procedure init_imp(var self:tr_imp; var perm:ta_index);
var
  perm1: ta_index;
  q: t_int;
  x: t_bits;
begin
  self := pre_imp;
  for q := 0 to bits-1 do begin
    perm[q] := no_index;
    perm1[q] := no_index;
    end;
  for q := 0 to bits-1 do begin
    if pre_imp.perm[q] <> no_index then begin
      x := lo_bit shl pre_imp.perm[q];
      x := imp_eval(pre_imp, x);
      perm1[q] := nr_trailing_0bits(x);
      assert(x = lo_bit shl perm1[q]);
      end;
    end;
  for q := 0 to bits-1 do begin
    x := lo_bit shl q;
    x := imp_eval(post_imp, x);
    perm[q] := perm1[nr_trailing_0bits(x)];
    end;
  end;


//////
// Count mask routines

function pprim_cmask(const self:tr_pprim):t_int;
var
  res: t_int;
begin
  case self.kind of
    pk_none:           res := 0;
    pk_permute:        res := 1;
    pk_permute_simple: res := 1;
    pk_rol:            res := 0;
    pk_mask:           res := 1;
    pk_mask_rol:       res := 1;
    pk_mask_shift:     res := 1;
    pk_mul:            res := 3;
    pk_gather_scatter: res := 2;
    pk_gather:         res := 1;
    pk_gather_shift:   res := 1;
    pk_scatter:        res := 1;
    pk_shift_scatter:  res := 1;
    pk_bswap:          res := 0;
    else begin
      writeln('ERROR: pprim_cmask: unknown kind');
      res := 0;
      error_abort;
      end;
    end;
  pprim_cmask := res;
  end;

function pstep_cmask(const self:tr_pstep):t_int;
var
  i: t_int;
  res: t_int;
begin
  res := 0;
  for i := 0 to self.nr_pprim-1 do begin
    res := res + pprim_cmask(self.a_pprim[i]);
    end;
  pstep_cmask := res;
  end;

function imp_cmask(const self:tr_imp):t_int;
var
  i: t_int;
  res: t_int;
begin
  res := 0;
  for i := 0 to self.nr_step-1 do begin
    res := res + pstep_cmask(self.a_step[i]);
    end;
  imp_cmask := res;
  end;


//////
// Cost routines

function pprim_cost(const self:tr_pprim):t_int;
var
  res: t_int;
begin
  case self.kind of
    pk_none:           res := 0;
    pk_permute:        res := options.cost_bit_permute_step;
    pk_permute_simple: res := options.cost_bit_permute_step_simple;
    pk_rol:            res := options.cost_rotate;
    pk_mask:           res := options.cost_and;
    pk_mask_rol:       res := options.cost_and+options.cost_rotate;
    pk_mask_shift:     res := options.cost_and+options.cost_shift;
    pk_mul: begin
      res := options.cost_mul + options.cost_and * 2;
      if self.rol<>0 then begin
        res := res + options.cost_rotate;
        end;
      if self.rol2<>0 then begin
        res := res + options.cost_rotate;
        end;
      end;
    pk_gather_scatter: res := options.cost_scatter+options.cost_gather;
    pk_gather:         res := options.cost_gather;
    pk_gather_shift:   res := options.cost_gather+options.cost_shift;
    pk_scatter:        res := options.cost_scatter;
    pk_shift_scatter:  res := options.cost_shift+options.cost_scatter;
    pk_bswap:          res := options.cost_shift+options.cost_bswap;
    else begin
      writeln('ERROR: pprim_cost: unknown kind');
      res := 0;
      error_abort;
      end;
    end;
  pprim_cost := res + pprim_cmask(self) * options.cost_mask;
  end;

function pstep_cost(const self:tr_pstep):t_int;
const
  s_gs = [
    pk_gather_scatter,
    pk_gather,
    pk_gather_shift,
    pk_scatter,
    pk_shift_scatter];
  s_mr = [
    pk_rol,
    pk_mask,
    pk_mask_rol,
    pk_mask_shift,
    pk_mul];
var
  i: t_int;
  res: t_int;
begin
  if self.nr_pprim = 0 then begin
    res := 0;
    end
  else begin
    res := pprim_cost(self.a_pprim[0]);
    for i := 1 to self.nr_pprim-1 do begin
      res := res + options.cost_or + pprim_cost(self.a_pprim[i]);
      if (self.a_pprim[i-1].kind in s_gs) AND
         (self.a_pprim[i].kind in s_gs) then begin
        res := res - options.bonus_gs;
        end
      else if (self.a_pprim[i-1].kind in s_mr) AND
              (self.a_pprim[i].kind in s_mr) then begin
        res := res - options.bonus_mask_rol;
        end
      else if (self.a_pprim[i-1].kind in s_mr) AND
              (self.a_pprim[i].kind in s_gs) then begin
        res := res - options.bonus_gs_rol;
        end
      else if (self.a_pprim[i-1].kind in s_gs) AND
              (self.a_pprim[i].kind in s_mr) then begin
        res := res - options.bonus_gs_rol;
        end;
      end;
    case self.a_pprim[0].kind of
      pk_permute:        res := res - options.bonus_bit_permute_step;
      pk_permute_simple: res := res - options.bonus_bit_permute_step_simple;
      else ;
      end;
    end;
  pstep_cost := res;
  end;

function imp_cost(const self:tr_imp):t_int;
var
  i: t_int;
  res: t_int;
begin
  res := 0;
  for i := 0 to self.nr_step-1 do begin
    res := res + pstep_cost(self.a_step[i]);
    end;
  imp_cost := res;
  end;


//////
// Performance routines

procedure init_performance(var self:tr_performance);
begin
  self.cost := maxint;
  self.cmask := maxint;
  end;

function cmp_performance(const a,b:tr_performance):t_int;
// -1: a<b
// 0: a=b
// 1: a>b
var
  res: t_int;
begin
  if a.cost < b.cost then begin
    res := -1;
    end
  else if a.cost > b.cost then begin
    res := 1;
    end
  else if a.cmask < b.cmask then begin
    res := -1;
    end
  else if a.cmask > b.cmask then begin
    res := 1;
    end
  else begin
    res := 0;
    end;
  cmp_performance := res;
  end;

procedure imp_performance(const imp:tr_imp; var perf:tr_performance);
begin
  perf.cost := imp_cost(imp);
  perf.cmask := imp_cmask(imp);
  end;


//////
// Dumping routines

var
  progress_pos: t_int;

procedure progress;
const
  chars: array [0..3] of t_char='-\|/';
begin
  progress_pos := progress_pos+1;
  write(chars[progress_pos and 3]);
  write(#$0d);
  end;

procedure dump_perm(const perm:ta_index);
var
  i: t_int;
begin
  for i := 0 to bits-1 do begin
    if perm[i] = no_index then begin
      write('* ');
      end
    else begin
      write(num(perm[i])+' ');
      end;
    end;
  writeln;
  end;

function dump_performance(const perf:tr_performance):t_string;
begin
  dump_performance :=
    num(perf.cost)+
    ' cycles, '+
    num(perf.cmask)+
    ' masks';
  end;

function pprim_dump(const self:tr_pprim):t_string;
var
  res: t_string;
begin
  case self.kind of
    pk_none: begin
      res := '';
      end;
    pk_permute: begin
      res := options.op_pstep+'(x, '+hex(self.mask)+', '+num(self.rol)+')';
      end;
    pk_permute_simple: begin
      res := options.op_pstep_simple+
        '(x, '+hex(self.mask)+', '+num(self.rol)+')';
      end;
    pk_rol: begin
      res := options.op_rol+'(x, '+num(self.rol)+')';
      end;
    pk_mask: begin
      res := '(x '+options.op_and+' '+hex(self.mask)+')';
      end;
    pk_mask_rol: begin
      res := options.op_rol+
        '(x '+options.op_and+' '+hex(self.mask)+', '+num(self.rol)+')';
      end;
    pk_mask_shift: begin
      if self.rol >= 0 then begin
        res := '((x '+options.op_and+' '+hex(self.mask)+') '+
          options.op_shl+' '+num(self.rol)+')';
        end
      else begin
        res := '((x '+options.op_and+' '+hex(self.mask)+') '+
          options.op_shr+' '+num(-self.rol)+')';
        end;
      end;
    pk_mul: begin
      if self.rol=0 then begin
        if self.rol2=0 then begin
          res := '(((x '+options.op_and+' '+hex(self.mask)+') * '+
            hex(self.mul)+') '+options.op_and+' '+hex(self.mask2)+')';
          end
        else begin
          res := options.op_rol+
            '(((x '+options.op_and+' '+hex(self.mask)+') * '+
            hex(self.mul)+') '+options.op_and+' '+hex(self.mask2)+', '+
            num(self.rol2 and (bits-1))+')';
          end;
        end
      else begin
        if self.rol2=0 then begin
          res := '((('+options.op_rol+'(x, '+num(self.rol)+') '+
            options.op_and+' '+hex(self.mask)+') * '+hex(self.mul)+') '+
            options.op_and+' '+hex(self.mask2)+')';
          end
        else begin
          res := options.op_rol+
            '((('+options.op_rol+'(x, '+num(self.rol)+') '+
            options.op_and+' '+hex(self.mask)+') * '+hex(self.mul)+') '+
            options.op_and+' '+hex(self.mask2)+', '+
            num(self.rol2 and (bits-1))+')';
          end;
        end;
      end;
    pk_gather_scatter: begin
      res := options.op_scatter+
        '('+options.op_gather+'(x, '+hex(self.mask)+'), '+hex(self.mask2)+')';
      end;
    pk_gather: begin
      res := options.op_gather+'(x, '+hex(self.mask)+')';
      end;
    pk_gather_shift: begin
      res := '('+options.op_gather+
        '(x, '+hex(self.mask)+') '+options.op_shl+' '+num(self.rol)+')';
      end;
    pk_scatter: begin
      res := options.op_scatter+'(x, '+hex(self.mask2)+')';
      end;
    pk_shift_scatter: begin
      res := options.op_scatter+'(x '+options.op_shr+' '+
        num((-self.rol) and (bits-1))+', '+hex(self.mask2)+')';
      end;
    pk_bswap: begin
      res := options.op_bswap+'(x)';
      end;
    else begin
      res := '';
      writeln('ERROR: pprim_dump: unknown kind');
      error_abort;
      end;
    end;
  pprim_dump := res;
  end;

procedure pstep_dump(const self:tr_pstep);
var
  i: t_int;
begin
  if self.nr_pprim <> 0 then begin
    write('x '+options.op_assign+' '+pprim_dump(self.a_pprim[0]));
    for i := 1 to self.nr_pprim-1 do begin
      writeln;
      write('  '+options.op_or+' '+pprim_dump(self.a_pprim[i]));
      end;
    write(';');
    end;
  end;

procedure imp_dump_simple(const self:tr_imp);
var
  perf: tr_performance;
begin
  imp_performance(self, perf);
  writeln(
    self.description+
    ': '+
    dump_performance(perf));
  end;

procedure imp_dump(const self:tr_imp);
var
  i: t_int;
begin
  imp_dump_simple(self);
  for i := 0 to self.nr_step-1 do begin
    pstep_dump(self.a_step[i]);
    if self.a_step[i].description <> '' then begin
      write('  '+options.comment_prefix+self.a_step[i].description+options.comment_postfix);
      end;
    writeln;
    end;
  end;


//////
// Code construction routines

procedure concat_description(var tgt:t_string; const src:t_string);
begin
  if tgt = '' then begin
    tgt := src;
    end
  else if src <> '' then begin
    tgt := tgt + ' + ' + src;
    end;
  end;

procedure finish_step(var imp:tr_imp);
var
  needed,resulting: t_bits;
begin
  if imp.nr_step = 0 then begin
    needed := imp.needed_src_bits;
    end
  else begin
    needed := imp.a_step[imp.nr_step-1].resulting_src_bits;
    end;
  resulting := pstep_eval(imp.a_step[imp.nr_step], needed);
  imp.a_step[imp.nr_step].resulting_src_bits := resulting;
  imp.nr_step := imp.nr_step+1;
  imp.a_step[imp.nr_step].needed_src_bits := resulting;
  end;

procedure finish_perm(var imp:tr_imp);
var
  i: t_int;
begin
  for i := 0 to post_imp.nr_step-1 do begin
    imp.a_step[imp.nr_step] := post_imp.a_step[i];
    finish_step(imp);
    end;
  concat_description(imp.description, post_imp.description);
  end;

procedure add_permute(var imp:tr_imp; mask:t_bits; rol:t_int);
// Add a permute step
var
  n: t_bits;
begin
  if mask <> 0 then begin
    rol := rol and (bits-1);
    imp.a_step[imp.nr_step].a_pprim[0].rol := rol;
    imp.a_step[imp.nr_step].a_pprim[0].mask := mask;
    n := imp.a_step[imp.nr_step].needed_src_bits;
    if nr_1bits(bit_permute_step_simple(n,mask,rol)) = nr_1bits(n) then begin
      // bit_permute_step_simple does not kill needed bits
      imp.a_step[imp.nr_step].a_pprim[0].kind := pk_permute_simple;
      end
    else begin
      // we need a full permute step
      imp.a_step[imp.nr_step].a_pprim[0].kind := pk_permute;
      end;
    imp.a_step[imp.nr_step].nr_pprim := 1;
    finish_step(imp);
    end;
  end;

procedure make_rol_step(var pstep:tr_pstep; mask:t_bits; rol:t_int);
// Add a mask/rol step
begin
  rol := rol and (bits-1);
  pstep.a_pprim[pstep.nr_pprim].rol := rol;
  pstep.a_pprim[pstep.nr_pprim].mask := mask;

  if mask = all_bits then begin
    // nothing to mask
    pstep.a_pprim[pstep.nr_pprim].kind := pk_rol;
    end
  else if rol = 0 then begin
    // nothing to rotate
    pstep.a_pprim[pstep.nr_pprim].kind := pk_mask;
    end
  else if (t_bits(mask shl rol) shr rol) = mask then begin
    // shift left is sufficient
    pstep.a_pprim[pstep.nr_pprim].kind := pk_mask_shift;
    end
  else if (t_bits(mask shr (bits-rol)) shl (bits-rol)) = mask then begin
    // shift right is sufficient
    pstep.a_pprim[pstep.nr_pprim].kind := pk_mask_shift;
    pstep.a_pprim[pstep.nr_pprim].rol := rol-bits;  // < 0
    end
  else begin
    // we need a rotate; a shift is not enough
    pstep.a_pprim[pstep.nr_pprim].kind := pk_mask_rol;
    end;
  end;

procedure add_rol_step(var pstep:tr_pstep; mask:t_bits; rol:t_int);
begin
  rol := rol and (bits-1);
  if (mask <> 0) AND
     ( (mask <> all_bits) OR
       (rol <> 0) ) then begin
    make_rol_step(pstep, mask, rol);
    pstep.nr_pprim := pstep.nr_pprim+1;
    end;
  end;

procedure add_rol(var imp:tr_imp; mask:t_bits; rol:t_int);
begin
  add_rol_step(imp.a_step[imp.nr_step], mask, rol);
  end;

procedure add_bswap_step(var pstep:tr_pstep);
begin
  pstep.a_pprim[pstep.nr_pprim].kind := pk_bswap;
  pstep.nr_pprim := pstep.nr_pprim+1;
  end;

procedure add_bswap(var imp:tr_imp);
begin
  add_bswap_step(imp.a_step[imp.nr_step]);
  end;

procedure add_gs_step(var pstep:tr_pstep; mask,mask2:t_bits);
// Add a gather/scatter step
var
  t1,t2: t_int;
begin
  if mask <> 0 then begin
    pstep.a_pprim[pstep.nr_pprim].rol := 0;
    pstep.a_pprim[pstep.nr_pprim].mask := mask;
    pstep.a_pprim[pstep.nr_pprim].mask2 := mask2;
    t1 := nr_trailing_0bits(mask);
    t2 := nr_trailing_0bits(mask2);
    if mask = mask2 then begin
      // we scatter what we gather, only masking needed
      pstep.a_pprim[pstep.nr_pprim].kind := pk_mask;
      end
    else if (mask shr t1) = (mask2 shr t2) then begin
      // the masks differ by a shift, so mask and rotate
      make_rol_step(pstep, mask, t2-t1);
      end
    else if is_contiguous_1bits(mask2) then begin
      // the target bits are contiguous
      if t2 = 0 then begin
        // single gather, no shift necessary
        pstep.a_pprim[pstep.nr_pprim].kind := pk_gather;
        end
      else begin
        // gather and shift
        pstep.a_pprim[pstep.nr_pprim].kind := pk_gather_shift;
        pstep.a_pprim[pstep.nr_pprim].rol := t2;
        end;
      end
    else if is_contiguous_1bits(mask) then begin
      // the source bits are contiguous
      if t1 = 0 then begin
        // single scatter, no shift necessary
        pstep.a_pprim[pstep.nr_pprim].kind := pk_scatter;
        end
      else begin
        // shift to 0 position and scatter
        pstep.a_pprim[pstep.nr_pprim].kind := pk_shift_scatter;
        pstep.a_pprim[pstep.nr_pprim].rol := (-t1) and (bits-1);
        end;
      end
    else begin
      // last resort: gather and scatter
      pstep.a_pprim[pstep.nr_pprim].kind := pk_gather_scatter;
      end;
    pstep.nr_pprim := pstep.nr_pprim+1;
    end;
  end;

procedure add_gs(var imp:tr_imp; mask,mask2:t_bits);
begin
  add_gs_step(imp.a_step[imp.nr_step], mask, mask2);
  end;

procedure add_sag_step(var pstep:tr_pstep; mask:t_bits);
// Add a sheep and goats step
var
  lo_mask: t_bits;
  rol: t_int;
begin
  rol := bits-nr_1bits(mask);
  if (rol<>0) AND (rol<>bits) then begin
    lo_mask := (lo_bit shl rol)-1;
    add_gs_step(pstep, mask, not lo_mask);
    add_gs_step(pstep, not mask, lo_mask);
    end;
  end;

procedure add_sag(var imp:tr_imp; mask:t_bits);
begin
  add_sag_step(imp.a_step[imp.nr_step], mask);
  finish_step(imp);
  end;

procedure add_inv_sag_step(var pstep:tr_pstep; mask:t_bits);
// Add a sheep and goats step
var
  lo_mask: t_bits;
  rol: t_int;
begin
  rol := bits-nr_1bits(mask);
  if (rol<>0) AND (rol<>bits) then begin
    lo_mask := (lo_bit shl rol)-1;
    add_gs_step(pstep, not lo_mask, mask);
    add_gs_step(pstep, lo_mask, not mask);
    end;
  end;

procedure add_inv_sag(var imp:tr_imp; mask:t_bits);
begin
  add_inv_sag_step(imp.a_step[imp.nr_step], mask);
  finish_step(imp);
  end;

procedure add_mul_step(var pstep:tr_pstep; mask,mul,mask2:t_bits; rol,rol2:t_int);
// Add a mul step
begin
  if mask <> 0 then begin
    pstep.a_pprim[pstep.nr_pprim].kind := pk_mul;
    pstep.a_pprim[pstep.nr_pprim].rol := rol and (bits-1);
    pstep.a_pprim[pstep.nr_pprim].rol2 := rol2 and (bits-1);
    pstep.a_pprim[pstep.nr_pprim].mask := mask;
    pstep.a_pprim[pstep.nr_pprim].mask2 := mask2;
    pstep.a_pprim[pstep.nr_pprim].mul := mul;
    pstep.nr_pprim := pstep.nr_pprim+1;
    end;
  end;


//////
// Routing routines

function is_identity:t_bool;
var
  imp: tr_imp;
  perm: ta_index;
  i: t_int;
  res: t_bool;
begin
  res := true;
  init_imp(imp,perm);
  for i := 0 to bits-1 do begin
    if perm[i] <> no_index then begin
      if i<>perm[i] then begin
        res := false;
        end;
      end;
    end;
  is_identity := res;
  end;

procedure route_benes(var imp:tr_imp; const a_stage:ta_subword);
var
  perm: ta_index;
  benes: tr_benes;
  stage_idx,stage: t_int;
begin
  init_imp(imp,perm);
  gen_benes_ex(benes,perm,a_stage);
  concat_description(imp.description, 'Benes ');
  imp.is_bfly := true;
  for stage_idx := 0 to ld_bits-1 do begin
    stage := a_stage[stage_idx];
    imp.description := imp.description+num(stage);
    if benes.b1.cfg[stage] <> 0 then begin
      imp.is_bfly := false;
      add_permute(imp, benes.b1.cfg[stage], 1 shl stage);
      end;
    end;
  for stage_idx := ld_bits-1 downto 0 do begin
    stage := a_stage[stage_idx];
    if benes.b2.cfg[stage] <> 0 then begin
      add_permute(imp, benes.b2.cfg[stage], 1 shl stage);
      end;
    end;
  finish_perm(imp);
  end;

procedure route_bit_groups(var imp:tr_imp);
var
  perm: ta_index;
  i,j,k,count,min_pos: t_int;
  s: t_bits;  // set of differences
  masks: array [0..bits-1] of t_bits;
begin
  init_imp(imp,perm);
  concat_description(imp.description, 'Bit group moving');

  s := 0;
  for i := 0 to bits-1 do begin
    if perm[i] <> no_index then begin
      s := s or (lo_bit shl ((i-perm[i]) and (bits-1)));
      end;
    end;

  count := 0;
  min_pos := 0;
  for i := bits-1 downto 0 do begin
    if ((lo_bit shl i) and s) <> 0 then begin
      count := count+1;
      min_pos := i;
      end;
    end;

  case count of
    0: begin
      finish_perm(imp);
      EXIT;
      end;
    1: begin
      if min_pos = 0 then begin
        finish_perm(imp);
        EXIT;
        end;
      add_rol(imp, all_bits, min_pos);
      finish_step(imp);
      finish_perm(imp);
      EXIT;
      end;
    else ;
    end;

  for i := 0 to bits-1 do begin
    masks[i] := 0;
    end;
  for i := 0 to bits-1 do begin
    if perm[i] <> no_index then begin
      j := (i-perm[i]) and (bits-1);
      k := (i-j) and (bits-1);
      masks[j] := masks[j] or (lo_bit shl k);
      end;
    end;

  for i := 0 to bits-1 do begin
    add_rol(imp, masks[i], i);
    end;

  finish_step(imp);
  finish_perm(imp);
  end;

procedure route_mul1(var imp:tr_imp);
// 2013-03-19  Sigrid/Jasper Neumann

var
  perm: ta_index;
  i: t_int;
  idx: t_int;
  best_idx: t_int;
  diff: t_int;
  rotate: t_int;
  u,use,m,mul,tgt: t_bits;
  empty: t_bool;
  pop, best_pop: t_int;
  save_perm: ta_index;
  nr_pprim: t_int;
  redo: t_bool;
  my_cost_mul, my_cost_rol: t_int;

begin
  init_imp(imp,perm);
  concat_description(imp.description, 'Shift by mul (1)');

  nr_pprim := imp.a_step[imp.nr_step].nr_pprim;
  save_perm := perm;

  repeat
    redo := false;
    rotate := 0;
    while true do begin

      empty := true;
      for i := 0 to bits-1 do begin
        if perm[i] <> no_index then begin
          empty := false;
          BREAK;
          end;
        end;
      if empty then begin
        BREAK;
        end;

      best_idx := 0;
      best_pop := 0;
      for idx := 0 to bits-1 do begin

        // Pseudo-calc
        mul := 0;
        use := 0;
        for i := 0 to bits-1 do begin
          if perm[i] <> no_index then begin
            diff := i-((perm[i]-idx) and (bits-1));
            if diff < 0 then begin
              CONTINUE;  // Can not be reached by mul
              end;
            u := lo_bit shl ((perm[i]-idx) and (bits-1));
            m := lo_bit shl diff;
            if (t_bits(mul*use) and t_bits(mul*u)) <> 0 then begin
              CONTINUE;  // Conflict
              end;
            if (m and mul) <> 0 then begin
              // already handled
              end
            else begin
              // new entry
              if (t_bits(mul*(use or u)) and t_bits(m*(use or u))) <> 0 then begin
                CONTINUE;  // Conflict
                end;
              mul := mul or m;
              end;
            use := use or u;
            end;
          end;

        pop := nr_1bits(mul) * 10 + nr_1bits(use);
        if ((idx+rotate) and (bits-1)) = 0 then begin
          pop := pop + 5;  // prefer not to rotate
          end;
        if pop > best_pop then begin
          best_idx := idx;
          best_pop := pop;
          end;
        end;

      for i := 0 to bits-1 do begin
        if perm[i] <> no_index then begin
          perm[i] := (perm[i]-best_idx) and (bits-1);
          end;
        end;
      rotate := rotate + best_idx;

      mul := 0;
      use := 0;
      tgt := 0;
      for i := 0 to bits-1 do begin
        if perm[i] <> no_index then begin
          diff := i-perm[i];
          if diff < 0 then begin
            CONTINUE;  // Can not be reached by mul
            end;
          u := lo_bit shl perm[i];
          m := lo_bit shl diff;
          if (t_bits(mul*use) and t_bits(mul*u)) <> 0 then begin
            CONTINUE;  // Conflict
            end;
          if (m and mul) <> 0 then begin
            // already handled
            end
          else begin
            // new entry
            if (t_bits(mul*(use or u)) and t_bits(m*(use or u))) <> 0 then begin
              CONTINUE;  // Conflict
              end;
            mul := mul or m;
            end;
          use := use or u;
          tgt := tgt or (lo_bit shl i);
          perm[i] := no_index;
          end;
        end;

      my_cost_mul :=
          // +options.cost_rotate*ord(rotate<>0)
          +options.cost_rotate
          +options.cost_mul
          +options.cost_and*2
          +options.cost_mask*2
          +options.cost_or
          -options.bonus_mask_rol;
      my_cost_rol :=
          // -ord((mul and 1)<>0)*options.cost_rotate
          +nr_1bits(mul)*(
            +options.cost_and
            +options.cost_rotate
            +options.cost_or
            +options.cost_mask
            -options.bonus_mask_rol);
      if my_cost_mul <= my_cost_rol then begin
        add_mul_step(imp.a_step[imp.nr_step], use,mul,tgt,-rotate,0);
        end
      else begin

        diff := (rotate-nr_trailing_0bits(mul)) and (bits-1);
        use := 0;
        for i := 0 to bits-1 do begin
          if save_perm[i] <> no_index then begin
            if ((save_perm[i]-i) and (bits-1)) = diff then begin
              use := use or (lo_bit shl ((i+diff) and (bits-1)));
              save_perm[i] := no_index;
              end;
            end;
          end;
        imp.a_step[imp.nr_step].nr_pprim := nr_pprim;
        add_rol(imp, use, -diff);
        nr_pprim := imp.a_step[imp.nr_step].nr_pprim;
        perm := save_perm;

        redo := true;
        BREAK;
        end;
      end;
    until NOT redo;
  finish_step(imp);
  finish_perm(imp);
  end;

procedure route_mul2(var imp:tr_imp);
// 2013-03-22  Sigrid/Jasper Neumann

var
  perm: ta_index;
  i,j,best_count: t_int;
  rotate: t_int;
  mask,u,use,m,mul,used,tgt,use0,u1: t_bits;
  found: t_bool;
  my_cost_mul, my_cost_rol: t_int;
  masks: array [0..bits-1] of t_bits;
  counts: array [0..bits-1] of t_int;  // nr_1bits(masks[i])

begin
  init_imp(imp,perm);
  concat_description(imp.description, 'Shift by mul (2)');

  mask := 0;
  for i := 0 to bits-1 do begin
    masks[i] := 0;
    counts[i] := 0;
    end;
  for i := 0 to bits-1 do begin
    if perm[i] <> no_index then begin
      j := (i-perm[i]) and (bits-1);
      u := lo_bit shl ((i-j) and (bits-1));
      mask := mask or u;
      masks[j] := masks[j] or u;
      counts[j] := counts[j] + 1;
      end;
    end;

  while mask<>0 do begin

    best_count := 0;
    i := 0;

    for j := 0 to bits-1 do begin
      if masks[j]<>0 then begin
        if counts[j]>best_count then begin
          best_count := counts[j];
          i := j;
          end;
        end;
      end;

    use := rol(masks[i],i);  // used bits
    rotate := i;
    used := lo_bit shl i;  // set of indexes which masks are used

    mul := 1;
    tgt := use;
    use0 := use;

    my_cost_rol := 1;

    repeat
      found := false;
      for i := 0 to bits-1 do begin
        if (masks[i] <> 0) AND
           ((used and (lo_bit shl i)) = 0) then begin
          u := rol(masks[i],rotate);
          j := ((i-rotate) and (bits-1));
          m := lo_bit shl j;
          if nr_1bits(m*u) = nr_1bits(u) then begin
            // if carries_of_mul(mul or m, use or u) = 0 then begin
            if (carries_of_mul(mul or m, use or u) and
                mask_hi(tgt or (u shl j))) = 0 then begin
              my_cost_rol := my_cost_rol + 1;
              use := use or u;
              mul := mul or m;
              used := used or (lo_bit shl i);
              tgt := tgt or (u shl j);
              found := true;
              end;
            end;
          end;
        end;
      until NOT found;

    my_cost_mul :=
        +options.cost_rotate*ord(rotate<>0)
        +options.cost_mul
        +options.cost_and*2
        +options.cost_mask*2
        +options.cost_or
        -options.bonus_mask_rol;
    my_cost_rol :=
         my_cost_rol*(
          +options.cost_and
          +options.cost_rotate
          +options.cost_or
          +options.cost_mask
          -options.bonus_mask_rol);
    if my_cost_mul >= my_cost_rol then begin
      mask := mask and not masks[rotate];
      masks[rotate] := 0;
      add_rol(imp, rol(use0,-rotate), rotate);
      end
    else begin
      for i := 0 to bits-1 do begin
        if (used and (lo_bit shl i)) <> 0 then begin
          mask := mask and not masks[i];
          masks[i] := 0;
          counts[i] := 0;
          end;
        end;

      repeat
        found := false;
        for i := bits-1 downto 0 do begin
          if masks[i] <> 0 then begin
            u := rol(masks[i],rotate);
            j := ((i-rotate) and (bits-1));
            m := lo_bit shl j;
            while u<>0 do begin
              u1 := u and (-u);
              if nr_1bits(m*u1) = nr_1bits(u1) then begin
                if (carries_of_mul(mul or m, use or u1) and
                    mask_hi(tgt or (u1 shl j))) = 0 then begin
                  // my_cost_rol := my_cost_rol + 1;
                  use := use or u1;
                  mul := mul or m;
                  // used := used or (lo_bit shl i);
                  tgt := tgt or (u1 shl j);
                  mask := mask and not rol(u1,-rotate);
                  masks[i] := masks[i] and not rol(u1,-rotate);
                  counts[i] := counts[i] - 1;
                  found := true;
                  end;
                end;
              u := u and not u1;
              end;
            end;
          end;
        until NOT found;

      add_mul_step(imp.a_step[imp.nr_step], use,mul,tgt,rotate,0);
      end;
    end;
  finish_step(imp);
  finish_perm(imp);
  end;

procedure route_mul3(var imp:tr_imp);
// 2013-03-19  Sigrid/Jasper Neumann

var
  perm: ta_index;
  i,j: t_int;
  idx: t_int;
  best_idx: t_int;
  diff: t_int;
  rotate: t_int;
  u,use,m,mul,tgt: t_bits;
  empty: t_bool;
  pop, best_pop: t_int;
  save_perm: ta_index;
  nr_pprim: t_int;
  redo: t_bool;
  hibit: t_int;
  himask: t_bits;
  my_cost_mul, my_cost_rol: t_int;

begin
  init_imp(imp,perm);
  concat_description(imp.description, 'Shift by mul (3)');

  nr_pprim := imp.a_step[imp.nr_step].nr_pprim;
  save_perm := perm;

  repeat
    redo := false;
    rotate := 0;
    while true do begin

      empty := true;
      for i := 0 to bits-1 do begin
        if perm[i] <> no_index then begin
          empty := false;
          BREAK;
          end;
        end;
      if empty then begin
        BREAK;
        end;

      best_idx := 0;
      best_pop := 0;
      for idx := 0 to bits-1 do begin

        // Pseudo-calc
        mul := 0;
        use := 0;
        hibit := 0;
        for i := bits-1 downto 0 do begin
          if perm[i] <> no_index then begin
            j := ((perm[i]-idx) and (bits-1));
            diff := i-j;
            if diff < 0 then begin
              CONTINUE;  // Can not be reached by mul
              end;
            u := lo_bit shl j;
            m := lo_bit shl diff;
            if i > hibit then begin
              j := i;
              end
            else begin
              j := hibit;
              end;
            himask := (lo_bit shl j)*2-1;
            if (t_bits(mul*use) and t_bits(mul*u) and himask) <> 0 then begin
              CONTINUE;  // Conflict
              end;
            if (m and mul) <> 0 then begin
              // already handled
              end
            else begin
              // new entry
              if (t_bits(mul*(use or u)) and t_bits(m*(use or u)) and himask) <> 0 then begin
                CONTINUE;  // Conflict
                end;
              mul := mul or m;
              end;
            use := use or u;
            if i > hibit then begin
              hibit := i;
              end;
            end;
          end;

        pop := nr_1bits(mul) + nr_1bits(use) * 10;
        if ((idx+rotate) and (bits-1)) = 0 then begin
          pop := pop + 1;  // prefer not to rotate
          end;
        if pop > best_pop then begin
          best_idx := idx;
          best_pop := pop;
          end;
        end;

      for i := 0 to bits-1 do begin
        if perm[i] <> no_index then begin
          perm[i] := (perm[i]-best_idx) and (bits-1);
          end;
        end;
      rotate := rotate + best_idx;

      mul := 0;
      use := 0;
      tgt := 0;
      hibit := 0;

      for i := bits-1 downto 0 do begin
        if perm[i] <> no_index then begin
          diff := i-perm[i];
          if diff < 0 then begin
            CONTINUE;  // Can not be reached by mul
            end;
          u := lo_bit shl perm[i];
          m := lo_bit shl diff;
          if i > hibit then begin
            j := i;
            end
          else begin
            j := hibit;
            end;
          himask := (lo_bit shl j)*2-1;
          if (t_bits(mul*use) and t_bits(mul*u) and himask) <> 0 then begin
            CONTINUE;  // Conflict
            end;
          if (m and mul) <> 0 then begin
            // already handled
            end
          else begin
            // new entry
            if (t_bits(mul*(use or u)) and t_bits(m*(use or u)) and himask) <> 0 then begin
              CONTINUE;  // Conflict
              end;
            mul := mul or m;
            end;
          use := use or u;
          tgt := tgt or (lo_bit shl i);
          if i > hibit then begin
            hibit := i;
            end;
          perm[i] := no_index;
          end;
        end;

      my_cost_mul :=
          +options.cost_rotate*ord(rotate<>0)
          +options.cost_mul
          +options.cost_and*2
          +options.cost_mask*2
          +options.cost_or
          -options.bonus_mask_rol;
      my_cost_rol :=
          +nr_1bits(mul)*(
            +options.cost_and
            +options.cost_rotate
            +options.cost_or
            +options.cost_mask
            -options.bonus_mask_rol);
      if my_cost_mul<=my_cost_rol then begin
        add_mul_step(imp.a_step[imp.nr_step], use,mul,tgt,-rotate,0);
        end
      else begin

        best_idx := 0;
        best_pop := 0;
        for idx := 0 to bits-1 do begin
          if ((lo_bit shl idx) and mul) <> 0 then begin
            diff := (rotate-idx) and (bits-1);

            // Pseudo-calc
            pop := 0;
            for i := 0 to bits-1 do begin
              if save_perm[i] <> no_index then begin
                if ((save_perm[i]-i) and (bits-1)) = diff then begin
                  pop := pop + 1;
                  end;
                end;
              end;

            if pop > best_pop then begin
              best_idx := idx;
              best_pop := pop;
              end;
            end;
          end;

        diff := (rotate-best_idx) and (bits-1);
        use := 0;
        for i := 0 to bits-1 do begin
          if save_perm[i] <> no_index then begin
            if ((save_perm[i]-i) and (bits-1)) = diff then begin
              use := use or (lo_bit shl ((i+diff) and (bits-1)));
              save_perm[i] := no_index;
              end;
            end;
          end;

        imp.a_step[imp.nr_step].nr_pprim := nr_pprim;
        add_rol(imp, use, -diff);
        nr_pprim := imp.a_step[imp.nr_step].nr_pprim;
        perm := save_perm;

        redo := true;
        BREAK;
        end;
      end;
    until NOT redo;
  finish_step(imp);
  finish_perm(imp);
  end;

procedure route_sag(var imp:tr_imp);
// 2012-09-14 (Knuth)

var
  perm: ta_index;
  a: array [0..ld_bits-1] of record
    mask: t_bits;
    // needed: t_bits;
    end;
  i,j: t_int;
  mask_src: t_bits;
  max_src,max_tgt: t_int;
  my_ld_bits: t_int;
  my_bits: t_int;

  needed_src: t_bits;
  inv_perm: ta_index;
  perm1: ta_index;
  perm2: ta_index;

begin
  init_imp(imp,perm);
  concat_description(imp.description, 'Sheep-and-goats method');

  for i := 0 to ld_bits-1 do begin
    a[i].mask := 0;
    // a[i].needed := 0;
    end;
  invert_perm(perm,inv_perm);

  needed_src := 0;  // set of needed source bits
  mask_src := 1;
  max_src := -1;
  max_tgt := -1;
  for i := 0 to bits-1 do begin
    if perm[i] > max_tgt then begin
      max_tgt := perm[i];
      end;
    if inv_perm[i] <> no_index then begin
      needed_src := needed_src or mask_src;
      if inv_perm[i] > max_src then begin
        max_src := inv_perm[i];
        end;
      end;
    mask_src := mask_src shl 1;
    end;

  // fill don't care entries ascending with hole indexes
  j := 0;
  for i := 0 to bits-1 do begin
    if perm[i] <> no_index then begin
      perm1[i] := perm[i];
      end
    else begin
      while ((lo_bit shl j) and needed_src) <> 0 do begin
        j := j+1;
        end;
      perm1[i] := j;
      j := j+1;
      end;
    end;

  if max_tgt < 0 then begin
    finish_perm(imp);
    EXIT;  // nothing to do
    end;
  // if max_src > max_tgt then begin
  //   max_tgt := max_src;
  //   end;
  // my_ld_bits := 0;
  // while max_tgt <> 0 do begin
  //   my_ld_bits := my_ld_bits+1;
  //   max_tgt := max_tgt shr 1;
  //   end;
  my_ld_bits := ld_bits;
  my_bits := 1 shl my_ld_bits;

  invert_perm(perm1,perm2);
  for i := 0 to my_ld_bits-1 do begin
    for j := 0 to my_bits-1 do begin
      a[i].mask := a[i].mask or (t_bits(((perm2[j] shr i) and 1)) shl j);
      end;
    // a[i].needed := needed_src;
    end;

  for i := 0 to my_ld_bits-1 do begin
    for j := i+1 to my_ld_bits-1 do begin
      a[j].mask :=
        compress_right(a[j].mask,not a[i].mask,my_ld_bits) or
        (compress_right(a[j].mask,a[i].mask,my_ld_bits) shl (my_bits shr 1));
      // a[j].needed :=
      //   compress_right(a[j].needed,not a[i].mask,my_ld_bits) or
      //   (compress_right(a[j].needed,a[i].mask,my_ld_bits) shl (my_bits shr 1));
      end;
    add_sag(imp, a[i].mask);
    end;

  finish_perm(imp);
  end;

procedure route_merge(var imp:tr_imp; inv:boolean);
// Original algorithm:
// "Efficient permutation instructions for fast software cryptography"
// by Ruby B. Lee, Zhijie Shi, Xiao Yang
// Idea to use it here (incl. first sample source):
// Donatas Radomskis <radomskis.donatas@gmail.com>
// Implementation: 2020-01-12 Sigrid/Jasper Neumann

type
  tpr_l=^tr_l;
  tr_l=record
    // One sublist
    count: t_int;
    idx: ta_index;
    end;
  tpr_ll=^tr_ll;
  tr_ll=record
    mask: t_bits;
    // A list of sublists
    count: t_int;
    l: array [0..bits+1-1] of tpr_l;
    end;
var
  perm: ta_index;
  count: t_int;
  ll: array [0..ld_bits-1+1] of tr_ll;
  i,i1,i2,j,j1,j2,k,idx: t_int;
  mask_src: t_bits;
  max_src,max_tgt: t_int;
  p_ll,p_ll_new: tpr_ll;
  p_l,p_l1,p_l2: tpr_l;

  needed_src: t_bits;
  inv_perm: ta_index;
  perm1: ta_index;

begin
  init_imp(imp,perm);
  if inv then begin
    invert_perm(perm,inv_perm);
    perm:=inv_perm;
    end;
  concat_description(imp.description, 'Merge sublists method');

  for i := 0 to ld_bits-1 do begin
    ll[i].count := 0;
    for j := 0 to bits+1-1 do begin
      ll[i].l[j]:=nil;
      end;
    end;

  invert_perm(perm,inv_perm);

  needed_src := 0;  // set of needed source bits
  mask_src := 1;
  max_src := -1;
  max_tgt := -1;
  for i := 0 to bits-1 do begin
    if perm[i] > max_tgt then begin
      max_tgt := perm[i];
      end;
    if inv_perm[i] <> no_index then begin
      needed_src := needed_src or mask_src;
      if inv_perm[i] > max_src then begin
        max_src := inv_perm[i];
        end;
      end;
    mask_src := mask_src shl 1;
    end;

  // fill don't care entries ascending with hole indexes
  j := 0;
  for i := 0 to bits-1 do begin
    if perm[i] <> no_index then begin
      perm1[i] := perm[i];
      end
    else begin
      while ((lo_bit shl j) and needed_src) <> 0 do begin
        j := j+1;
        end;
      perm1[i] := j;
      j := j+1;
      end;
    end;

  if max_tgt < 0 then begin
    finish_perm(imp);
    EXIT;  // nothing to do
    end;

  idx := bits;  // last processed index, must hit first condition
  p_ll := @ll[0];
  p_ll^.count := 0;
  p_l := nil;
  for i := 0 to bits-1 do begin
    if perm1[i] < idx then begin
      // Start new sublist
      p_ll^.count := p_ll^.count+1;
      new(p_ll^.l[p_ll^.count-1]);
      p_l := p_ll^.l[p_ll^.count-1];
      p_l^.count := 0;
      end;
    p_l^.count := p_l^.count+1;
    p_l^.idx[p_l^.count-1] := perm1[i];
    idx := perm1[i];
    end;
  new(p_ll^.l[p_ll^.count]);
  p_ll^.l[p_ll^.count]^.count := 0;

  k:=0;
  count:=0;
  while p_ll^.count>1 do begin

    // Merge sublists
    // system.write(p_ll^.count,': ');
    count:=count+1;
    // Source
    i1:=0;
    i2:=(p_ll^.count+1) div 2;  // odd count => use empty list at end
    // Target
    p_ll_new := @ll[k+1];
    p_ll_new^.count:=i2;
    for i:=0 to i2 do begin  // yes, one more; <ld_bits
      new(p_ll_new^.l[i]);
      p_ll_new^.l[i]^.count:=0;
      end;
    p_ll^.mask:=0;
    mask_src:=1;
    i:=0;
    while i<p_ll_new^.count do begin
      // system.write(i1,'+',i2,'  ');
      p_l1 := p_ll^.l[i1];
      p_l2 := p_ll^.l[i2];
      new(p_ll_new^.l[i]);
      p_l := p_ll_new^.l[i];
      p_l^.count:=0;
      j1:=0;
      j2:=0;
      j:=0;

      // Merge one pair of sublists
      while true do begin
        if (j1<p_l1^.count) AND (j2<p_l2^.count) then begin
          if p_l1^.idx[j1] < p_l2^.idx[j2] then begin
            p_l^.idx[j]:=p_l1^.idx[j1];
            j:=j+1;
            j1:=j1+1;
            end
          else begin
            p_l^.idx[j]:=p_l2^.idx[j2];
            j:=j+1;
            j2:=j2+1;
            p_ll^.mask := p_ll^.mask or mask_src;
            end;
          end
        else if j1<p_l1^.count then begin
          p_l^.idx[j]:=p_l1^.idx[j1];
          j:=j+1;
          j1:=j1+1;
          end
        else if j2<p_l2^.count then begin
          p_l^.idx[j]:=p_l2^.idx[j2];
          j:=j+1;
          j2:=j2+1;
          p_ll^.mask := p_ll^.mask or mask_src;
          end
        else begin
          BREAK;
          end;
        mask_src:=mask_src shl 1;
        end;

      p_l^.count:=p_l1^.count+p_l2^.count;
      i1:=i1+1;
      i2:=i2+1;
      i:=i+1;

      end;
    p_ll:=p_ll_new;
    k:=k+1;
    // system.writeln;

    end;

  if inv then begin

    // Create permutation entry
    for i:=0 to count-1 do begin
      add_inv_sag(imp, ll[i].mask);
      // for j:=0 to ll[i].count-1 do begin
      //   for k:=0 to ll[i].l[j].count-1 do begin
      //     system.write(' ',ll[i].l[j].idx[k]);
      //     end;
      //   system.write(';');
      //   end;
      // system.writeln;
      end;

    end
  else begin

    // Create permutation entry
    for i:=count-1 downto 0 do begin
      add_sag(imp, ll[i].mask);
      // for j:=0 to ll[i].count-1 do begin
      //   for k:=0 to ll[i].l[j].count-1 do begin
      //     system.write(' ',ll[i].l[j].idx[k]);
      //     end;
      //   system.write(';');
      //   end;
      // system.writeln;
      end;

    end;

  // Clean up
  for i := 0 to ld_bits-1 do begin
    for j := 0 to bits+1-1 do begin
      if ll[i].l[j]<>nil then begin
        dispose(ll[i].l[j]);
        end;
      end;
    end;

  finish_perm(imp);
  end;

type
  tf_aux_route_gs = function (
    var a: tar_gather_scatter;
    const perm: ta_index;
    needed_src: t_bits;
    needed_tgt: t_bits): t_int;

function aux_route_gather_scatter(
  var a: tar_gather_scatter;
  const perm: ta_index;
  needed_src: t_bits;
  needed_tgt: t_bits): t_int;
// FAR;
// 2012-09-27  Sigrid/Jasper Neumann
var
  a_count: t_int;
  s,t: t_int;
  mask_src: t_bits;
  mask_tgt: t_bits;
begin
  a_count := 0;
  while needed_src <> 0 do begin
    s := 0;
    t := -1;
    while true do begin
      t := t+1;
      while (t < bits) AND
            (((lo_bit shl t) and needed_tgt) = 0) do begin
        t := t+1;
        end;
      if t >= bits then begin
        BREAK;
        end;

      if perm[t] < s then begin
        CONTINUE;
        end;
      s := perm[t];
      // assert(s <> no_index);

      mask_src := lo_bit shl s;
      mask_tgt := lo_bit shl t;
      a[a_count].mask_src := a[a_count].mask_src or mask_src;
      a[a_count].mask_tgt := a[a_count].mask_tgt or mask_tgt;
      needed_src := needed_src and not mask_src;
      needed_tgt := needed_tgt and not mask_tgt;
      end;
    a_count := a_count+1;
    end;
  aux_route_gather_scatter := a_count;
  end;

function aux_route_gather_shift_sloppy(
  var a: tar_gather_scatter;
  const perm: ta_index;
  needed_src: t_bits;
  needed_tgt: t_bits): t_int;
// FAR;
// 2012-09-25  Sigrid/Jasper Neumann
var
  a_count: t_int;
  s,t: t_int;
  mask_src: t_bits;
  mask_tgt: t_bits;
begin
  a_count := 0;
  while needed_src <> 0 do begin
    s := 0;  // -bits;
    t := -1;
    while true do begin
      t := t+1;
      while (t < bits) AND
            (((lo_bit shl t) and needed_tgt) = 0) do begin
        t := t+1;
        // s := s+1;
        end;
      if t >= bits then begin
        BREAK;
        end;

      if perm[t] < s then begin
        BREAK;
        end;
      s := perm[t];
      // assert(s <> no_index);

      mask_src := lo_bit shl s;
      mask_tgt := lo_bit shl t;
      a[a_count].mask_src := a[a_count].mask_src or mask_src;
      a[a_count].mask_tgt := a[a_count].mask_tgt or mask_tgt;
      needed_src := needed_src and not mask_src;
      needed_tgt := needed_tgt and not mask_tgt;
      end;
    a_count := a_count+1;
    end;
  aux_route_gather_shift_sloppy := a_count;
  end;

function aux_route_gather_shift(
  var a: tar_gather_scatter;
  const perm: ta_index;
  needed_src: t_bits;
  needed_tgt: t_bits): t_int;
// FAR;
// 2012-09-26  Sigrid/Jasper Neumann
var
  a_count: t_int;
  s,t: t_int;
  mask_src: t_bits;
  mask_tgt: t_bits;
begin
  a_count := 0;
  while needed_src <> 0 do begin
    s := 0;  // -bits;
    t := 0;
    while (t < bits) AND
          (((lo_bit shl t) and needed_tgt) = 0) do begin
      t := t+1;
      end;
    t := t-1;
    while true do begin
      t := t+1;
      if t >= bits then begin
        BREAK;
        end;

      if perm[t] = no_index then begin
        BREAK;
        end;
      if perm[t] < s then begin
        BREAK;
        end;
      s := perm[t];

      mask_src := lo_bit shl s;
      mask_tgt := lo_bit shl t;
      a[a_count].mask_src := a[a_count].mask_src or mask_src;
      a[a_count].mask_tgt := a[a_count].mask_tgt or mask_tgt;
      needed_src := needed_src and not mask_src;
      needed_tgt := needed_tgt and not mask_tgt;
      end;
    a_count := a_count+1;
    end;
  aux_route_gather_shift := a_count;
  end;

function aux_route_shift_scatter_sloppy(
  var a: tar_gather_scatter;
  const perm: ta_index;
  needed_src: t_bits;
  needed_tgt: t_bits): t_int;
// FAR;
// 2012-09-25  Sigrid/Jasper Neumann
var
  a_count: t_int;
  s,t: t_int;
  mask_src: t_bits;
  mask_tgt: t_bits;
  inv_perm: ta_index;
begin
  invert_perm(perm,inv_perm);
  a_count := 0;
  while needed_tgt <> 0 do begin
    t := 0;  // -bits;
    s := -1;
    while true do begin
      s := s+1;
      while (s < bits) AND
            (((lo_bit shl s) and needed_src) = 0) do begin
        s := s+1;
        // t := t+1;
        end;
      if s >= bits then begin
        BREAK;
        end;

      if inv_perm[s] < t then begin
        BREAK;
        end;
      t := inv_perm[s];
      // assert(t <> no_index);

      mask_tgt := lo_bit shl t;
      mask_src := lo_bit shl s;
      a[a_count].mask_tgt := a[a_count].mask_tgt or mask_tgt;
      a[a_count].mask_src := a[a_count].mask_src or mask_src;
      needed_tgt := needed_tgt and not mask_tgt;
      needed_src := needed_src and not mask_src;
      end;
    a_count := a_count+1;
    end;
  aux_route_shift_scatter_sloppy := a_count;
  end;

function aux_route_shift_scatter(
  var a: tar_gather_scatter;
  const perm: ta_index;
  needed_src: t_bits;
  needed_tgt: t_bits): t_int;
// FAR;
// 2012-09-26  Sigrid/Jasper Neumann
var
  a_count: t_int;
  s,t: t_int;
  mask_src: t_bits;

  mask_tgt: t_bits;
  inv_perm: ta_index;
begin
  invert_perm(perm,inv_perm);
  a_count := 0;
  while needed_tgt <> 0 do begin
    t := 0;  // -bits;
    s := 0;
    while (s < bits) AND
          (((lo_bit shl s) and needed_src) = 0) do begin
      s := s+1;
      end;
    s := s-1;
    while true do begin
      s := s+1;
      if s >= bits then begin
        BREAK;
        end;

      if inv_perm[s] = no_index then begin
        BREAK;
        end;
      if inv_perm[s] < t then begin
        BREAK;
        end;
      t := inv_perm[s];

      mask_tgt := lo_bit shl t;
      mask_src := lo_bit shl s;
      a[a_count].mask_tgt := a[a_count].mask_tgt or mask_tgt;
      a[a_count].mask_src := a[a_count].mask_src or mask_src;
      needed_tgt := needed_tgt and not mask_tgt;
      needed_src := needed_src and not mask_src;
      end;
    a_count := a_count+1;
    end;
  aux_route_shift_scatter := a_count;
  end;

procedure decorate_route_gs(var imp:tr_imp; opt:t_bool);
begin
  if opt then begin
    imp.description := imp.description+' opt';
    end
  else begin
    imp.description := imp.description+' pure';
    end;
  end;

procedure frame_route_gs(var imp:tr_imp; opt:t_bool; fn:tf_aux_route_gs; const des:t_string);
// 2012-09-27  Sigrid/Jasper Neumann
var
  perm: ta_index;
  a: tar_gather_scatter;
  a_count: t_int;
  i,j,s,t: t_int;
  mask_src: t_bits;
  mask_tgt: t_bits;
  skip: t_int;
  diff: t_int;
  ok: t_bool;
begin
  init_imp(imp,perm);
  concat_description(imp.description, des);
  decorate_route_gs(imp,opt);
  skip := 0;
  repeat
    ok := true;
    imp.a_step[imp.nr_step].nr_pprim := skip;
    for i := 0 to bits-1 do begin
      init_gather_scatter(a[i]);
      end;
    a_count := fn(a,perm,used_source_bits(perm),used_target_bits(perm));
    for i := 0 to a_count-1 do begin
      mask_src := a[i].mask_src;
      mask_tgt := a[i].mask_tgt;

      if opt then begin
        s := nr_trailing_0bits(mask_src);
        t := nr_trailing_0bits(mask_tgt);
        if mask_src shr s = mask_tgt shr t then begin
          imp.a_step[imp.nr_step].nr_pprim := skip;
          skip := skip+1;
          diff := (t-s) and (bits-1);
          mask_src := 0;
          for j := 0 to bits-1 do begin
            if (perm[j] <> no_index) AND
               (((j-perm[j]) and (bits-1)) = diff) then begin
              mask_src := mask_src or (lo_bit shl j);
              perm[j] := no_index;
              end;
            end;
          add_rol(imp, rol(mask_src,-diff), diff);
          ok := false;
          BREAK;
          end;
        end;
      add_gs(imp,mask_src,mask_tgt);
      end;
    until ok;
  finish_step(imp);
  finish_perm(imp);
  end;

procedure route_gather_scatter(var imp:tr_imp; opt:t_bool);
begin
  frame_route_gs(imp,opt,aux_route_gather_scatter, 'Gather/scatter');
  end;

procedure route_gather_shift_sloppy(var imp:tr_imp; opt:t_bool);
begin
  frame_route_gs(imp,opt,aux_route_gather_shift_sloppy, 'Gather/shift sloppy');
  end;

procedure route_gather_shift(var imp:tr_imp; opt:t_bool);
begin
  frame_route_gs(imp,opt,aux_route_gather_shift, 'Gather/shift');
  end;

procedure route_shift_scatter_sloppy(var imp:tr_imp; opt:t_bool);
begin
  frame_route_gs(imp,opt,aux_route_shift_scatter_sloppy, 'Shift/scatter sloppy');
  end;

procedure route_shift_scatter(var imp:tr_imp; opt:t_bool);
begin
  frame_route_gs(imp,opt,aux_route_shift_scatter, 'Shift/scatter');
  end;


//////
// Best performance

var
  best_performance: tr_performance;
  best_imp: tr_imp;

procedure use_best_imp(const imp:tr_imp);
var
  performance: tr_performance;
begin
  imp_performance(imp, performance);
  if cmp_performance(performance, best_performance) < 0 then begin
    best_performance := performance;
    best_imp := imp;
    end;
  end;

procedure check_best(const imp:tr_imp);
begin
  if options.verbose then begin
    imp_dump_simple(imp);
    end;
  imp_check(imp);
  use_best_imp(imp);
  end;


//////
// Test routing variations (BPC / Benes)

type
  tf_test_route = procedure (const idx:ta_subword);

procedure permute_sub(x:t_int; var idx:ta_subword; fn:tf_test_route);
var
  i: t_int;
  q: t_int;
begin
  if x = ld_bits-1 then begin
    fn(idx);
    end
  else begin
    for i := x+1 to ld_bits-1 do begin
      permute_sub(x+1,idx,fn);
      q := idx[x];
      idx[x] := idx[i];
      idx[i] := q;
      end;
    permute_sub(x+1,idx,fn);
    for i := ld_bits-1 downto x+1 do begin
      q := idx[x];
      idx[x] := idx[i];
      idx[i] := q;
      end;
    end;
  end;

procedure permute(fn:tf_test_route);
var
  idx: ta_subword;
  i: t_int;
begin
  for i := 0 to ld_bits-1 do begin
    idx[i] := i;
    end;
  permute_sub(0, idx, fn);
  end;

var
  is_bpc_perm: t_bool;
  bpc_performance: tr_performance;
  benes_performance: tr_performance;

procedure route_bpc(var imp:tr_imp; const tgt:ta_subword; k:t_bit_index);
// 2012-08-31
var
  perm: ta_index;
  src,inv_src: ta_subword;
  n,m: t_int;
  i,j,ii,kk: t_subword;
begin
  init_imp(imp,perm);
  concat_description(imp.description, 'BPC permutation ');

  for i := 0 to ld_bits-1 do begin
    imp.description := imp.description+num(tgt[i]);
    src[i] := i;
    inv_src[i] := i;
    end;
  imp.description := imp.description+'/'+hex(k);

  kk := 0;  // k for generated

  for i := 0 to ld_bits-1 do begin  // any order
    n := 1 shl i;
    ii := src[i];
    if tgt[i] = ii then begin
      if ((k xor kk) and n) <> 0 then begin
        // x := bit_index_complement(x,i);
        imp.a_step[imp.nr_step].needed_src_bits := all_bits;
        add_permute(imp, a_bfly_mask[i], n);  // 1 shl i;
        end;
      end
    else begin
      j := inv_src[tgt[i]];
      m := 1 shl j;
      if ((k and n) <> 0) XOR  // boolean xor
         ((kk and m) <> 0) then begin
        // x := bit_index_swap_complement(x,i,j);
        imp.a_step[imp.nr_step].needed_src_bits := all_bits;
        add_permute(imp, a_bfly_mask[j] and a_bfly_mask[i], m+n);
        if (kk and n) = 0 then begin
          kk := kk or m;
          end
        else begin
          kk := kk and not m;
          end;
        end
      else begin
        // x := bit_index_swap(x,i,j);
        imp.a_step[imp.nr_step].needed_src_bits := all_bits;
        add_permute(imp, a_bfly_mask[j] and not a_bfly_mask[i], m-n);
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
  finish_perm(imp);
  end;

procedure test_route_bpc(const idx:ta_subword);
// FAR;
var
  perm: ta_index;
  inv: t_bit_index;  // 0..bits-1, i.e. set of [0..ld_bits-1]
  i,j: t_int;
  found: t_bool;
  v: t_bit_index;  // 0..bits-1, i.e. set of [0..ld_bits-1]
  imp: tr_imp;
  performance: tr_performance;
begin
  init_imp(imp,perm);
  for inv := 0 to bits-1 do begin
    found := true;
    for j := 0 to bits-1 do begin
      if perm[j] <> no_index then begin
        v := 0;
        for i := 0 to ld_bits-1 do begin
          if ((j xor inv) and (1 shl i)) <> 0 then begin
            v := v or (1 shl idx[i]);
            end;
          end;
        if perm[j] <> v then begin
          found := false;
          BREAK;
          end;
        end;
      end;
    if found then begin
      is_bpc_perm := true;
      route_bpc(imp,idx,inv);
      imp_check(imp);
      // imp_dump_simple(imp);
      use_best_imp(imp);
      imp_performance(imp, performance);
      if cmp_performance(performance, bpc_performance) < 0 then begin
        bpc_performance := performance;
        end;
      end;
    end;
  end;

procedure test_route_benes(const idx:ta_subword);
// FAR;
var
  imp: tr_imp;
  performance: tr_performance;
begin
  route_benes(imp,idx);
  imp_check(imp);
  // imp_dump_simple(imp);
  use_best_imp(imp);
  imp_performance(imp, performance);
  if cmp_performance(performance, benes_performance) < 0 then begin
    benes_performance := performance;
    end;
  end;


//////
// Permutation decorators

procedure test_all(ex:t_bool);
// needs pre_imp and post_imp correctly initialized
var
  imp: tr_imp;
  perm: ta_index;
  save_is_bpc_perm: t_bool;
begin
  save_is_bpc_perm:=is_bpc_perm;

  if options.verbose then begin
    writeln;
    end;

  if is_identity then begin
    init_imp(imp,perm);
    // concat_description(imp.description, 'Identity');
    finish_perm(imp);
    check_best(imp);
    end
  else begin
    init_performance(bpc_performance);
    init_performance(benes_performance);

    if options.test_bpc AND ex then begin
      permute(test_route_bpc);
      if options.verbose then begin
        if is_bpc_perm then begin
          writeln(
            'Altered best BPC permutation: '+dump_performance(bpc_performance));
          end
        else begin
          writeln('Altered BPC: Not possible');
          end;
        end;
      end;

    if options.test_benes AND ex then begin
      permute(test_route_benes);
      if options.verbose then begin
        writeln('Altered best Benes: '+dump_performance(benes_performance));
        end;
      end;

    if options.test_bit_groups then begin
      route_bit_groups(imp);
      check_best(imp);
      end;

    if options.test_mul then begin
      route_mul1(imp);
      check_best(imp);

      route_mul2(imp);
      check_best(imp);

      route_mul3(imp);
      check_best(imp);
      end;

    if options.allow_bmi then begin
      if options.test_gather_scatter then begin
        route_gather_scatter(imp,options.opt_gs);
        check_best(imp);
        end;

      if options.test_gather_shift then begin
        route_gather_shift(imp,options.opt_gs);
        check_best(imp);
        end;

      if options.test_gather_shift_sloppy then begin
        route_gather_shift_sloppy(imp,options.opt_gs);
        check_best(imp);
        end;

      if options.test_shift_scatter then begin
        route_shift_scatter(imp,options.opt_gs);
        check_best(imp);
        end;

      if options.test_shift_scatter_sloppy then begin
        route_shift_scatter_sloppy(imp,options.opt_gs);
        check_best(imp);
        end;

      if options.test_sag then begin
        route_sag(imp);
        check_best(imp);
        end;

      if options.test_merge then begin
        route_merge(imp, false);
        check_best(imp);
        route_merge(imp, true);
        check_best(imp);
        end;
      end;
    end;

  is_bpc_perm:=save_is_bpc_perm;
  end;

procedure try_pre_rol(const perm:ta_index; opt_bswap:t_bool);
var
  i,j,xswap: t_int;
begin
  if opt_bswap then begin
    xswap := 7;
    end
  else begin
    xswap := 0;
    end;
  for i:=bits-1 downto 1 do begin  // all rotates but 0
    // progress;
    write(num(i)+' '#$0d);
    for j:=0 to xswap do begin
      init_imp0(pre_imp,perm);
      if (j and 1)<>0 then begin
        concat_description(pre_imp.description, 'Bswap');
        add_bswap(pre_imp);
        finish_step(pre_imp);
        end;
      concat_description(pre_imp.description, 'Rol '+num(i));
      add_rol(pre_imp,all_bits,i);
      finish_step(pre_imp);
      if (j and 2)<>0 then begin
        concat_description(pre_imp.description, 'Bswap');
        add_bswap(pre_imp);
        finish_step(pre_imp);
        end;

      init_imp0(post_imp,perm);
      if (j and 4)<>0 then begin
        concat_description(post_imp.description, 'Bswap');
        add_bswap(post_imp);
        finish_step(post_imp);
        end;

      test_all(options.opt_rol_ex);
      end;
    end;
  end;

procedure try_post_rol(const perm:ta_index; opt_bswap:t_bool);
var
  i,j,xswap: t_int;
begin
  if opt_bswap then begin
    xswap := 7;
    end
  else begin
    xswap := 0;
    end;
  for i:=bits-1 downto 1 do begin  // all rotates but 0
    // progress;
    write(num(i)+' '#$0d);
    for j:=0 to xswap do begin
      init_imp0(pre_imp,perm);
      if (j and 1)<>0 then begin
        concat_description(pre_imp.description, 'Bswap');
        add_bswap(pre_imp);
        finish_step(pre_imp);
        end;

      init_imp0(post_imp,perm);
      if (j and 2)<>0 then begin
        concat_description(post_imp.description, 'Bswap');
        add_bswap(post_imp);
        finish_step(post_imp);
        end;
      concat_description(post_imp.description, 'Rol '+num(i));
      add_rol(post_imp,all_bits,i);
      finish_step(post_imp);
      if (j and 4)<>0 then begin
        concat_description(post_imp.description, 'Bswap');
        add_bswap(post_imp);
        finish_step(post_imp);
        end;

      test_all(options.opt_rol_ex);
      end;
    end;
  end;

procedure try_bswap(const perm:ta_index);
var
  j: t_int;
begin
  for j:=1 to 3 do begin
    init_imp0(pre_imp,perm);
    if (j and 1)<>0 then begin
      concat_description(pre_imp.description, 'Bswap');
      add_bswap(pre_imp);
      finish_step(pre_imp);
      end;

    init_imp0(post_imp,perm);
    if (j and 2)<>0 then begin
      concat_description(post_imp.description, 'Bswap');
      add_bswap(post_imp);
      finish_step(post_imp);
      end;

    test_all(true);
    end;
  end;

procedure my_random_perm(var perm:ta_index);
var
  i: t_int;
  hi: t_int;
begin
  random_perm(perm);
  case random_int(4) of
    0: begin
      for i := 1 to random_int(bits) do begin
        perm[random_int(bits)] := no_index;
        end;
      end;
    else ;
    end;
  case random_int(4) of
    0: begin
      hi := random_int(bits);
      for i := 0 to bits-1 do begin
        if perm[i] >= hi then begin
          perm[i] := no_index;
          end;
        end;
      end;
    else ;
    end;
  case random_int(4) of
    0: begin
      hi := random_int(bits);
      for i := hi to bits-1 do begin
        perm[i] := no_index;
        end;
      end;
    else ;
    end;
  end;

procedure self_test;
var
  perm: ta_index;
  imp: tr_imp;
  loop: t_longint;
begin
  for loop := 1030 downto 1 do begin
    if (loop and $3f) = 0 then begin
      write(loop,'  '#$0d);
      // flush;
      end;

    my_random_perm(perm);
    init_imp0(pre_imp,perm);
    init_imp0(post_imp,perm);

    route_mul1(imp);
    imp_check(imp);

    route_mul2(imp);
    imp_check(imp);

    route_mul3(imp);
    imp_check(imp);

    route_bit_groups(imp);
    imp_check(imp);

    route_benes(imp,a_stage_bwd);
    imp_check(imp);

    route_benes(imp,a_stage_fwd);
    imp_check(imp);

    route_gather_scatter(imp,true);
    imp_check(imp);

    route_gather_shift_sloppy(imp,true);
    imp_check(imp);

    route_gather_shift(imp,true);
    imp_check(imp);

    route_shift_scatter_sloppy(imp,true);
    imp_check(imp);

    route_shift_scatter(imp,true);
    imp_check(imp);

    route_sag(imp);
    imp_check(imp);

    route_merge(imp,false);
    imp_check(imp);
    route_merge(imp,true);
    imp_check(imp);
    end;
  write('        '#$0d);

  for loop := 260 downto 1 do begin
    if (loop and $0f) = 0 then begin
      write(loop,'  '#$0d);
      // flush;
      end;

    my_random_perm(perm);
    init_imp0(pre_imp,perm);
    init_imp0(post_imp,perm);

    permute(test_route_bpc);
    permute(test_route_benes);
    end;
  write('        '#$0d);
  end;

var
  perm: ta_index;


//////
// Startup/parameter routines

procedure split_opt(const opt:t_string; var name,value:t_string);
var
  i,len: t_int;
begin
  name := '';
  value := '';

  len := length(opt);

  // search for line comment sign (#)
  i := 1;
  while (i <= len) AND
        (opt[i] <> '#') do begin
    i := i + 1;
    end;
  if i <= len then begin  // we found the #, now trim
    len := i - 1;
    end;

  // trim right
  while (len > 0) AND
        (opt[len] <= ' ') do begin
    len := len - 1;
    end;
  if len = 0 then begin
    EXIT;  // nothing to do
    end;

  // trim left
  i := 1;
  while (i <= len) AND
        (opt[i] <= ' ') do begin
    i := i + 1;
    end;

  // skip lead in character if present
  if (i <= len) AND
     ( (opt[i] = '/') OR
       (opt[i] = '-') ) then begin
    i := i + 1;
    end;

  // extract name
  while (i <= len) AND
        (opt[i] <> '=') AND
        (opt[i] <> ':') do begin
    name := name + locase(opt[i]);
    i := i + 1;
    end;

  // skip delimiter
  i := i + 1;

  // extract value
  while i <= len do begin
    value := value + opt[i];
    i := i + 1;
    end;
  end;

procedure setup_default_pascal;
begin
  options.comment_prefix:='// ';
  options.comment_postfix:='';
  options.hex_prefix := '$';
  options.hex_postfix := '';
  options.op_assign := ':=';
  options.op_and := 'and';
  options.op_or := 'or';
  options.op_xor := 'xor';
  options.op_shl := 'shl';
  options.op_shr := 'shr';
  end;

procedure setup_default_c;
begin
  options.comment_prefix:='// ';
  options.comment_postfix:='';
  options.hex_prefix := '0x';
  options.hex_postfix := '';
  options.op_assign := '=';
  options.op_and := '&';
  options.op_or := '|';
  options.op_xor := '^';
  options.op_shl := '<<';
  options.op_shr := '>>';
  end;

procedure recalc_cost_permute;
begin
  options.cost_bit_permute_step :=
    options.cost_shift*2+options.cost_xor*3+options.cost_and;
  options.cost_bit_permute_step_simple :=
    options.cost_shift*2+options.cost_and*2+options.cost_or;
  end;

procedure recalc_cost_opt;
begin
  options.cost_rotate := options.cost_rotate_shift;
  options.cost_shift := options.cost_rotate_shift;
  options.cost_and := options.cost_bool;
  options.cost_or := options.cost_bool;
  options.cost_xor := options.cost_bool;
  options.cost_scatter := options.cost_gs;
  options.cost_gather := options.cost_gs;

  recalc_cost_permute;
  end;

procedure setup_defaults;
begin
  options.dump_input := true;
  options.dump_inverse := true;
  options.verbose := true;
  options.brief := true;

  setup_default_pascal;
  // setup_default_c;

  options.op_pstep := 'bit_permute_step';
  options.op_pstep_simple := 'bit_permute_step_simple';
  options.op_rol := 'rol';
  options.op_gather := 'pext';
  options.op_scatter := 'pdep';
  options.op_bswap := 'bswap';

  options.in_origin := 0;  // input index origin
  options.in_base := 10;  // input number base
  options.in_indexes_are_target := false;  // /in_indexes=source or target

  options.cost_rotate_shift := 1;
  options.cost_bool := 1;
  options.cost_bswap := 1;
  options.cost_mul := 4;
  options.cost_gs := 3;
  options.cost_mask := 0;

  recalc_cost_opt;

  options.bonus_bit_permute_step := 1;  // implicitely parallel
  options.bonus_bit_permute_step_simple := 1;  // implicitely parallel
  options.bonus_gs := 3;  // 2 parallel gs
  options.bonus_mask_rol := 2;  // 2 parallel mask_rol/mask_shift ops
  options.bonus_gs_rol := 1;  // parallel mask_rol/mask_shift and gs ops

  options.allow_bswap := true;
  options.allow_bmi := false;
  options.test_bpc := true;
  options.test_bfly := true;
  options.test_ibfly := true;
  options.test_benes := true;
  options.test_bit_groups := true;
  options.test_mul := true;
  options.test_gather_scatter := true;
  options.test_gather_shift := true;
  options.test_gather_shift_sloppy := true;
  options.test_shift_scatter := true;
  options.test_shift_scatter_sloppy := true;
  options.test_sag := true;
  options.test_merge := true;
  options.opt_gs := true;
  options.opt_rol := true;
  options.opt_rol_ex := false;
  options.opt_bswap := true;

  options.self_test := false;
  end;

procedure setup(const opt:t_string);  forward;

procedure setup_file(const n:t_string);
var
  f: text;
  s: t_string;
  i: t_int;
  n1: t_string;
begin
  n1 := '';
  for i:=1 to length(n) do begin
    if n[i]<>'"' then begin
      n1 := n1 + n[i];
      end;
    end;

  assign(f, n1);
  reset(f);
  while NOT eof(f) do begin
    readln(f, s);
    setup(s);
    end;
  close(f);
  end;

procedure setup(const opt:t_string);
var
  n,v: t_string;
begin
  if opt = '' then begin
    // ignore
    end
  else if opt[1] = '@' then begin
    setup_file(copy(opt,2,length(opt)-1));
    end
  else begin
    split_opt(opt,n,v);

    if n = '' then begin
      // ignore
      end

    else if n = 'dump_input' then begin
      options.dump_input := s2i(v)<>0;
      end
    else if n = 'dump_inverse' then begin
      options.dump_inverse := s2i(v)<>0;
      end
    else if n = 'verbose' then begin
      options.verbose := s2i(v)<>0;
      end
    else if n = 'brief' then begin
      options.brief := s2i(v)<>0;
      end

    else if n = 'output_pas' then begin
      setup_default_pascal;
      end
    else if n = 'output_c' then begin
      setup_default_c;
      end

    else if n = 'comment_prefix' then begin
      if v = '' then begin
        options.comment_prefix := '';
        end
      else begin
        options.comment_prefix := v+' ';
        end;
      end
    else if n = 'comment_postfix' then begin
      if v = '' then begin
        options.comment_postfix := '';
        end
      else begin
        options.comment_postfix := ' '+v;
        end;
      end
    else if n = 'hex_prefix' then begin
      options.hex_prefix := v;
      end
    else if n = 'hex_postfix' then begin
      options.hex_postfix := v;
      end
    else if n = 'op_assign' then begin
      options.op_assign := v;
      end
    else if n = 'op_and' then begin
      options.op_and := v;
      end
    else if n = 'op_or' then begin
      options.op_or := v;
      end
    else if n = 'op_xor' then begin
      options.op_xor := v;
      end
    else if n = 'op_shl' then begin
      options.op_shl := v;
      end
    else if n = 'op_shr' then begin
      options.op_shr := v;
      end

    else if n = 'in_origin' then begin
      options.in_origin := s2i(v);
      end
    else if n = 'in_base' then begin
      options.in_base := s2i(v);
      end
    else if n = 'in_indexes' then begin
      v := lostr(v);
      if v = 'source' then begin
        options.in_indexes_are_target := false;
        end
      else if v = 'target' then begin
        options.in_indexes_are_target := true;
        end
      else begin
        writeln('ERROR: Unknown option to /in_indexes '+v);
        error_abort;
        end;
      end

    else if n = 'op_pstep' then begin
      options.op_pstep := v;
      end
    else if n = 'op_pstep_simple' then begin
      options.op_pstep_simple := v;
      end
    else if n = 'op_rol' then begin
      options.op_rol := v;
      end
    else if n = 'op_gather' then begin
      options.op_gather := v;
      end
    else if n = 'op_scatter' then begin
      options.op_scatter := v;
      end
    else if n = 'op_bswap' then begin
      options.op_bswap := v;
      end

    else if n = 'cost_rotate_shift' then begin
      options.cost_rotate_shift := s2i(v);
      recalc_cost_opt;
      end
    else if n = 'cost_bool' then begin
      options.cost_bool := s2i(v);
      recalc_cost_opt;
      end
    else if n = 'cost_bswap' then begin
      options.cost_bswap := s2i(v);
      recalc_cost_opt;
      end
    else if n = 'cost_mul' then begin
      options.cost_mul := s2i(v);
      recalc_cost_opt;
      end
    else if n = 'cost_gs' then begin
      options.cost_gs := s2i(v);
      recalc_cost_opt;
      end
    else if n = 'cost_mask' then begin
      options.cost_mask := s2i(v);
      end

    else if n = 'cost_rotate' then begin
      options.cost_rotate := s2i(v);
      recalc_cost_permute;
      end
    else if n = 'cost_shift' then begin
      options.cost_shift := s2i(v);
      recalc_cost_permute;
      end
    else if n = 'cost_and' then begin
      options.cost_and := s2i(v);
      recalc_cost_permute;
      end
    else if n = 'cost_or' then begin
      options.cost_or := s2i(v);
      recalc_cost_permute;
      end
    else if n = 'cost_xor' then begin
      options.cost_xor := s2i(v);
      recalc_cost_permute;
      end
    else if n = 'cost_scatter' then begin
      options.cost_scatter := s2i(v);
      end
    else if n = 'cost_gather' then begin
      options.cost_gather := s2i(v);
      end

    else if n = 'cost_bit_permute_step' then begin
      options.cost_bit_permute_step := s2i(v);
      end
    else if n = 'cost_bit_permute_step_simple' then begin
      options.cost_bit_permute_step_simple := s2i(v);
      end

    else if n = 'bonus_bit_permute_step' then begin
      options.bonus_bit_permute_step := s2i(v);
      end
    else if n = 'bonus_bit_permute_step_simple' then begin
      options.bonus_bit_permute_step_simple := s2i(v);
      end

    else if n = 'bonus_gs' then begin
      options.bonus_gs := s2i(v);
      end
    else if n = 'bonus_mask_rol' then begin
      options.bonus_mask_rol := s2i(v);
      end
    else if n = 'bonus_gs_rol' then begin
      options.bonus_gs_rol := s2i(v);
      end

    else if n = 'allow_bswap' then begin
      options.allow_bswap := s2i(v)<>0;
      end
    else if n = 'allow_bmi' then begin
      options.allow_bmi := s2i(v)<>0;
      end
    else if n = 'test_bpc' then begin
      options.test_bpc := s2i(v)<>0;
      end
    else if n = 'test_bfly' then begin
      options.test_bfly := s2i(v)<>0;
      end
    else if n = 'test_ibfly' then begin
      options.test_ibfly := s2i(v)<>0;
      end
    else if n = 'test_benes' then begin
      options.test_benes := s2i(v)<>0;
      end
    else if n = 'test_bit_groups' then begin
      options.test_bit_groups := s2i(v)<>0;
      end
    else if n = 'test_mul' then begin
      options.test_mul := s2i(v)<>0;
      end
    else if n = 'test_gather_scatter' then begin
      options.test_gather_scatter := s2i(v)<>0;
      end
    else if n = 'test_gather_shift' then begin
      options.test_gather_shift := s2i(v)<>0;
      end
    else if n = 'test_gather_shift_sloppy' then begin
      options.test_gather_shift_sloppy := s2i(v)<>0;
      end
    else if n = 'test_shift_scatter' then begin
      options.test_shift_scatter := s2i(v)<>0;
      end
    else if n = 'test_shift_scatter_sloppy' then begin
      options.test_shift_scatter_sloppy := s2i(v)<>0;
      end
    else if n = 'test_sag' then begin
      options.test_sag := s2i(v)<>0;
      end
    else if n = 'test_merge' then begin
      options.test_merge := s2i(v)<>0;
      end
    else if n = 'opt_gs' then begin
      options.opt_gs := s2i(v)<>0;
      end
    else if n = 'opt_rol' then begin
      options.opt_rol := s2i(v)<>0;
      end
    else if n = 'opt_rol_ex' then begin
      options.opt_rol_ex := s2i(v)<>0;
      end
    else if n = 'opt_bswap' then begin
      options.opt_bswap := s2i(v)<>0;
      end

    else if n = 'self_test' then begin
      options.self_test := s2i(v)<>0;
      end

    else begin
      writeln('ERROR: Unknown option '+n);
      error_abort;
      end;
    end;
  end;

procedure get_perm(const opt:t_string; var perm:ta_index);
var
  i: t_int;
  idx: t_int;
  ok: t_bits;
  c: char;
  my_base: t_int;
  val: t_int;
  letter_ofs: t_int;
  digit: t_int;
  s: t_string;
begin
  for i := 0 to bits-1 do begin
    perm[i] := no_index;
    end;

  my_base := options.in_base;
  ok := 0;
  idx := -1;
  i := 1;
  while i <= length(opt) do begin
    c := opt[i];
    val := -2;
    case c of

      #$00..' ', ',': begin
        i := i+1;
        CONTINUE;
        end;

      '*': begin
        val := -1;
        end;

      '$': begin
        my_base := 16;
        i := i+1;
        CONTINUE;
        end;

      '/', '-': begin
        s := '';
        while (i <= length(opt)) AND
              (opt[i] <> ' ') do begin
          s := s+opt[i];
          i := i+1;
          end;
        setup(s);
        my_base := options.in_base;  // in_base might have changed
        CONTINUE;
        end;

      '#': begin
        BREAK;  // comment until end of line
        end;

      '@': begin
        i := i+1;  // skip @
        s := '';
        while (i <= length(opt)) AND
              (opt[i] <> ' ') do begin
          if opt[i] <> '"' then begin
            s := s+opt[i];
            i := i+1;
            end
          else begin
            while (i <= length(opt)) AND
                  (opt[i] <> '"') do begin
              s := s+opt[i];
              i := i+1;
              end;
            i := i+1;
            end;
          end;
        setup_file(s);
        my_base := options.in_base;  // in_base might have changed
        CONTINUE;
        end;

      '0'..'9', 'a'..'z', 'A'..'Z': begin
        if my_base = 26 then begin
          letter_ofs := 0;
          end
        else begin
          letter_ofs := 10;
          end;
        val := 0;
        digit := 0;  // Delphi, shut up!
        while i <= length(opt) do begin
          c := opt[i];
          case c of
            '0'..'9': digit := ord(c)-ord('0');
            'a'..'z': digit := ord(c)-ord('a')+letter_ofs;
            'A'..'Z': digit := ord(c)-ord('A')+letter_ofs;
            else BREAK;
            end;
          if (my_base <> 26) AND
             (digit >= my_base) then begin
            writeln('ERROR: Invalid digit');
            error_abort;
            end;
          val := val*my_base + digit;
          i := i+1;
          end;
        i := i-1;
        end;

      else ;
      end;
    if val = -2 then begin
      writeln('ERROR: Illegal character');
      i := i+1;
      CONTINUE;
      end;
    idx := idx+1;
    if val = -1 then begin
      // ignore
      end
    else if (val < options.in_origin) OR
            (val-options.in_origin > bits-1) then begin
      writeln('ERROR: Out of range');
      error_abort;
      end
    else begin
      val := val - options.in_origin;
      if ((lo_bit shl val) and ok) <> 0 then begin
        writeln('ERROR: Dupe: '+num(val));
        error_abort;
        end;
      ok := ok or (lo_bit shl val);
      end;
    perm[idx] := val;
    my_base := options.in_base;
    i := i+1;
    end;

  if idx < 0 then begin
    // random_perm(perm);
    // for i := 1 to random_int(bits) do begin
    //   perm[random_int(bits)] := no_index;
    //   end;
    my_random_perm(perm);
    end;
  end;

procedure startup;
var
  opt: t_string;
  i: t_int;
  inv_perm: ta_index;
begin
  setup_defaults;

  setup_file('calcperm.ini');

  opt := '';
  for i := 1 to paramcount do begin
    opt := opt+' '+paramstr(i);
    end;

  get_perm(opt, perm);

  if options.in_indexes_are_target then begin
    invert_perm(perm,inv_perm);
    perm := inv_perm;
    end;

  init_imp0(pre_imp,perm);
  init_imp0(post_imp,perm);
  end;

var
  inv_perm: ta_index;
  imp: tr_imp;
  is_bfly_perm: t_bool;
  is_ibfly_perm: t_bool;


//////
// Main program

begin
  if NOT init_general then begin
    HALT(1);
    end;

  writeln('Permutation code generator (in Pascal, bits=',bits,')...');
  if sizeof(t_bits)*8 <> bits then begin
    writeln('Sizes wrong! sizeof(t_bits)=', sizeof(t_bits));
    HALT(1);
    end;

  my_randseed := random_int(32767);

  startup;

  if options.self_test then begin
    self_test;
    writeln('OK');
    end
  else begin
    if options.dump_input then begin
      write('Permutation vector: ');
      dump_perm(perm);
      end;

    if options.dump_inverse then begin
      write('Inverse vector: ');
      invert_perm(perm,inv_perm);
      dump_perm(inv_perm);
      end;

    init_performance(best_performance);
    init_performance(bpc_performance);
    init_performance(benes_performance);
    is_bpc_perm := false;
    is_bfly_perm := false;
    is_ibfly_perm := false;

    if options.verbose then begin
      writeln;
      end;

    if is_identity then begin
      is_bpc_perm := true;
      is_bfly_perm := true;
      is_ibfly_perm := true;
      init_imp(imp,perm);
      concat_description(imp.description, 'Identity');
      finish_perm(imp);
      check_best(imp);
      end
    else begin

      if options.test_bpc then begin
        permute(test_route_bpc);
        if options.verbose then begin
          if is_bpc_perm then begin
            writeln('Best BPC permutation: '+dump_performance(bpc_performance));
            end;
          end;
        end;

      if options.test_bfly then begin
        route_benes(imp,a_stage_bwd);
        if imp.is_bfly then begin
          check_best(imp);
          is_bfly_perm := true;
          end;
        end;

      if options.test_ibfly then begin
        route_benes(imp,a_stage_fwd);
        if imp.is_bfly then begin
          check_best(imp);
          is_ibfly_perm := true;
          end;
        end;

      if options.test_benes then begin
        permute(test_route_benes);
        if options.verbose then begin
          writeln('Best Benes: '+dump_performance(benes_performance));
          end;
        end;

      if options.test_bit_groups then begin
        route_bit_groups(imp);
        check_best(imp);
        end;

      if options.test_mul then begin
        route_mul1(imp);
        check_best(imp);

        route_mul2(imp);
        check_best(imp);

        route_mul3(imp);
        check_best(imp);
        end;

      if options.allow_bmi then begin
        if options.test_gather_scatter then begin
          route_gather_scatter(imp,options.opt_gs);
          check_best(imp);
          end;

        if options.test_gather_shift then begin
          route_gather_shift(imp,options.opt_gs);
          check_best(imp);
          end;

        if options.test_gather_shift_sloppy then begin
          route_gather_shift_sloppy(imp,options.opt_gs);
          check_best(imp);
          end;

        if options.test_shift_scatter then begin
          route_shift_scatter(imp,options.opt_gs);
          check_best(imp);
          end;

        if options.test_shift_scatter_sloppy then begin
          route_shift_scatter_sloppy(imp,options.opt_gs);
          check_best(imp);
          end;

        if options.test_sag then begin
          route_sag(imp);
          check_best(imp);
          end;

        if options.test_merge then begin
          route_merge(imp,false);
          check_best(imp);
          route_merge(imp,true);
          check_best(imp);
          end;
        end;

      if options.opt_rol then begin
        try_pre_rol(perm, options.opt_bswap AND options.allow_bswap);
        try_post_rol(perm, options.opt_bswap AND options.allow_bswap);
        write(' '#$0d);  // get rid of progress display
        end;
      if options.opt_bswap AND options.allow_bswap then begin
        try_bswap(perm);
        end;
      end;

    if options.brief OR options.verbose then begin
      writeln;
      if options.test_bpc then begin
        writeln('BPC permutation: '+q2s(is_bpc_perm));
        end;
      if options.test_bfly then begin
        writeln('Butterfly: '+q2s(is_bfly_perm));
        end;
      if options.test_ibfly then begin
        writeln('Inverse Butterfly: '+q2s(is_ibfly_perm));
        end;

      writeln;
      writeln('=== Best method ===');
      end;

    if best_performance.cost = maxint then begin
      writeln('NOT ROUTABLE WITH ALLOWED METHODS');
      end
    else begin
      imp_dump(best_imp);
      end;
    end;
  end.

// eof.
