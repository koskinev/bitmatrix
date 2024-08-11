//////
// Intro

// Endian specific access types

// (c) 2011..2020 Sigrid/Jasper Neumann
// www.sirrida.de / programming.sirrida.de
// E-Mail: info@sirrida.de

// Granted to the public domain
// First version: 2013-02-14
// Last change: 2013-02-14

// Compile with
// Delphi:      dcc32 /cc calcperm.pas
// Free Pascal: fpc -Mtp calcperm.pas

// Here are as an example two classes featuring protected access to fields
// of exchange records with an explicit endianess.
// These exchange records are principally needed whenever you want to
// communicate with other programs or with devices.

{$i general.pas }

(*$a1 *)
  // Force byte packed structures.
  // Pragma push/pop is unfortunately not generally supported,
  // so we have this global.

function bswap_32(q:t_32u):t_32u;
begin
  q := ((q and $00ff00ff) shl 8) or ((q shr 8) and $00ff00ff);
  bswap_32 := (q shl 16) or (q shr 16);
  end;

type

  tr_intel_32u = object
  // Little endian, Intel byte order, least significant byte first
  private
    mem: t_32u;
  public
    function value:t_32u;
    procedure assign(q:t_32u);
    end;

function tr_intel_32u.value:t_32u;
begin
(*$ifdef ENDIAN_BIG *)
  value := bswap_32(mem);
(*$else *)
  value := mem;
(*$endif *)
  end;

procedure tr_intel_32u.assign(q:t_32u);
begin
(*$ifdef ENDIAN_BIG *)
  mem := bswap_32(q);
(*$else *)
  mem := q;
(*$endif *)
  end;

type

  tr_motorola_32u = object
  // Big endian, Motorola byte order, most significant byte first
  private
    mem: t_32u;
  public
    function value:t_32u;
    procedure assign(q:t_32u);
    end;

function tr_motorola_32u.value:t_32u;
begin
(*$ifdef ENDIAN_BIG *)
  value := mem;
(*$else *)
  value := bswap_32(mem);
(*$endif *)
  end;

procedure tr_motorola_32u.assign(q:t_32u);
begin
(*$ifdef ENDIAN_BIG *)
  mem := q;
(*$else *)
  mem := bswap_32(q);
(*$endif *)
  end;

type

  tr_exchange = packed record
    // An example of an exchange record
    x: tr_intel_32u;
    y: tr_motorola_32u;
    end;


//////
// Main program

var
  v: tr_exchange;
  r: t_32u;
  loop: t_longint;
begin
  if NOT init_general then begin
    HALT(1);
    end;

  writeln('Testing endian access records (in Pascal)...');

  if sizeof(tr_exchange)<>8 then begin
    writeln('tr_exchange defective');
    HALT(1);
    end;
  if bswap_32($12345678)<>$78563412 then begin
    writeln('bswap_32 defective');
    HALT(1);
    end;
  if bswap_32($87654321)<>$21436587 then begin
    writeln('bswap_32 defective');
    HALT(1);
    end;

  r:=0;
  for loop:=1 to 10000 do begin
    r:=r*256+t_32u(random_int(256));
    v.x.assign(r);
    if v.x.value<>r then begin
      writeln('tr_intel_32u defective');
      HALT(1);
      end;
    v.y.assign(r);
    if v.y.value<>r then begin
      writeln('tr_motorola_32u defective');
      HALT(1);
      end;
    end;
  writeln('OK');
  end.

// eof.
