//////
// Intro

// Some bit hacks and permutations

// (c) 2011..2020 Sigrid/Jasper Neumann
// www.sirrida.de / programming.sirrida.de
// E-Mail: info@sirrida.de

// Granted to the public domain
// First version: 2013-02-15
// Last change: 2013-02-15

// Include file to supply general types and functions.

// At program start you should call
//   init_general
// for some general checks.
// Also, the random generator (random_int) is seeded.
//
// if NOT init_general then begin
//   HALT(1);
//   end;

type
  // Fixed size integer types
(*$ifdef __GPC__ *)
  // GNU Pascal
  t_8u = cardinal attribute (size = 8);
  t_8s = integer attribute (size = 8);
  t_16u = cardinal attribute (size = 16);
  t_16s = integer attribute (size = 16);
  t_32u = cardinal attribute (size = 32);
  t_32s = integer attribute (size = 32);
  t_64u = cardinal attribute (size = 64);
  t_64s = integer attribute (size = 64);
  // t_128u = cardinal attribute (size = 128);  // not supported
  // t_128s = integer attribute (size = 128);  // not supported
(*$else *)
  // Free Pascal and Delphi
  t_8u = byte;
  t_8s = shortint;
  t_16u = word;
  t_16s = smallint;
  t_32s = longint;
  t_32u = longword;
  t_64s = int64;
  t_64u = int64;  // Should be UInt64 but this is undefined for Delphi < 2005
  // t_128u = UInt128;  // Free Pascal and Delphi don't have such type
(*$endif *)
  t_int = integer;  // General integer type; >= 16 bit
  t_uint = cardinal;  // General unsigned integer type; >= 16 bit
  t_longint = longint;  // General integer type; >= 32 bit
  t_char = char;
  t_bool = boolean;

function random_int(x:t_int):t_int;
// 0..x-1, x quite small, typically <=32767
// Replace with your own generator if you want
begin
  random_int := random(x);
  end;

function init_general:t_bool;
// General initializations and checks.
// Results false if some error is detected, also an error message is printed.
var
  res: t_bool;
begin
  res := true;
  if sizeof(t_8s)*8 <> 8 then begin
    writeln('t_8s defective');
    res := false;
    end;
  if sizeof(t_8u)*8 <> 8 then begin
    writeln('t_8u defective');
    res := false;
    end;
  if sizeof(t_16s)*8 <> 16 then begin
    writeln('t_16s defective');
    res := false;
    end;
  if sizeof(t_16u)*8 <> 16 then begin
    writeln('t_16u defective');
    res := false;
    end;
  if sizeof(t_32s)*8 <> 32 then begin
    writeln('t_32s defective');
    res := false;
    end;
  if sizeof(t_32u)*8 <> 32 then begin
    writeln('t_32u defective');
    res := false;
    end;
  if sizeof(t_64s)*8 <> 64 then begin
    writeln('t_64s defective');
    res := false;
    end;
  if sizeof(t_64u)*8 <> 64 then begin
    writeln('t_64u defective');
    res := false;
    end;
  if sizeof(t_int)*8 < 16 then begin
    writeln('t_int defective');
    res := false;
    end;
  if sizeof(t_uint)*8 < 16 then begin
    writeln('t_uint defective');
    res := false;
    end;
  if sizeof(t_longint)*8 < 32 then begin
    writeln('t_longint defective');
    res := false;
    end;

  randomize;

  init_general := res;
  end;

// eof.
