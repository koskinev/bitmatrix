;=====
; Intro

; Sample usage of PEXT and PDEP
; These x86 BMI-2 commands are introduced on
; http://software.intel.com/file/36945 (319433-011)

; (c) 2011..2020 Sigrid/Jasper Neumann
; http://programming.sirrida.de
; E-Mail: info@sirrida.de

; Granted to the public domain
; First version: 2011-07-20
; Last change: 2013-08-15

; You can use the free assembler NASM to assemble these instructions
; if you use a version of 2011-07-08 or later, see snapshots.
; The Intel Software Development Emulator >= 4.29 (dated 2011-07-01)
; is capable of emulating these commands.

; I assume the cpu features BMI-2 and POPCNT to be present.

; Version for 32 bit mathematics in 32 bit mode
; Assumed free registers: eax, edx, ecx
; Calling convention: "Register" as in Delphi-32 or FPC on Windows-32


;=====
; Function list

; compress_right_32_5
; expand_right_32_5
; compress_left_32_5
; expand_left_32_5
; shuffle_32_0_5
; unshuffle_32_0_5
; sag_32_5
; inv_sag_32_5
; index_of_bit_i_32

;
bits 32

%macro PROC 1
  align 16
  global %1
  %1:
%endmacro


;=====
; Working size: 32 bits

PROC compress_right_32_5  ; (value:eax; mask:edx):eax
; = compress_right_32(value,mask,5)
  pext eax,eax,edx
  ret

PROC expand_right_32_5  ; (value:eax; mask:edx):eax
; = expand_right_32(value,mask,5)
  pdep eax,eax,edx
  ret

PROC compress_left_32_5  ; (value:eax; mask:edx):eax
; = compress_left_32(value,mask,5)
  pext eax,eax,edx
  popcnt ecx,edx
  ror eax,cl
  ret

PROC expand_left_32_5  ; (value:eax; mask:edx):eax
; = expand_left_32(value,mask,5)
  popcnt ecx,edx
  rol eax,cl
  pdep eax,eax,edx
  ret

PROC shuffle_32_0_5  ; (value:eax):eax
; = shuffle_32(value,0,5)
  mov ecx,0x55555555
  pdep edx,eax,ecx
  shr eax,16
  not ecx
  pdep eax,eax,ecx
  or eax,edx
  ret

PROC unshuffle_32_0_5  ; (value:eax):eax
; = unshuffle_32(value,0,5)
  mov ecx,0x55555555
  pext edx,eax,ecx
  not ecx
  pext eax,eax,ecx
  shl eax,16
  or eax,edx
  ret

PROC sag_32_5  ; (value:eax; mask:edx):eax
; = sag_32(value,mask,5)
  pext ecx,eax,edx
  not edx
  pext eax,eax,edx
  popcnt edx,edx
  xchg edx,ecx  ; necessary to avoid extra register
  ror eax,cl
  or eax,edx
  ret

PROC inv_sag_32_5  ; (value:eax; mask:edx):eax
; = inv_sag_32(value,mask,5)
  push ebx
  pdep ebx,eax,edx
  popcnt ecx,edx
  not edx
  ror eax,cl
  pdep eax,eax,edx
  or eax,ebx
  pop ebx
  ret

PROC index_of_bit_i_32  ; (i:eax; source:edx):eax
; Compute the index of the i-th (i:0..31) set bit
; if present => 0..31; if not present => -1
; i must be in 0..31 (unchecked)
; 2012-01-15 Sigrid/Jasper Neumann; last change: 2012-01-28
  mov ecx,1
  shlx eax,ecx,eax  ; shl replaced to get rid of mov (wrong registers); bmi2
  pdep eax,eax,edx
  mov ecx,-1        ; indicate not present
  bsr eax,eax
  cmovz eax,ecx     ; replacement of conditional jump
  ret

; All AMD and Intel processors seem to leave the target of bsr as is
; if fed with 0. AMD documents this but Intel leaves it as unspecified.
; Thanks to Harold Aptroot (2013-06-07) for pointing this out.
; I verified this with Intel P2, Atom, i7 (32 bit OS).
; Thus the code could be optimized as this:
;  mov ecx,1
;  shlx eax,ecx,eax  ; shl replaced to get rid of mov (wrong registers); bmi2
;  pdep ecx,eax,edx
;  mov eax,-1        ; indicate not present
;  bsr eax,ecx
;  ret

PROC mask_bit_i_32  ; (i:eax; source:edx):eax
; Compute the mask of the i-th (i:0..31) set bit
; if present => one bit set; if not present => 0
; i must be in 0..31 (unchecked)
; 2013-08-15 Sigrid/Jasper Neumann
  mov ecx,1
  shlx eax,ecx,eax  ; shl replaced to get rid of mov (wrong registers); bmi2
  pdep eax,eax,edx
  ret

; EOF.
