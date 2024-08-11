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
; Last change: 2013-06-12

; You can use the free assembler NASM to assemble these instructions
; if you use a version of 2011-07-08 or later, see snapshots.
; The Intel Software Development Emulator >= 4.29 (dated 2011-07-01)
; is capable of emulating these commands.

; I assume the cpu features BMI-2 and POPCNT to be present.

; Version for 32 and 64 bit mathematics in 64 bit mode
; Assumed free registers: rax, rcx, rdx, r8, r9
; Calling convention: Windows-64


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
; compress_right_64_6
; expand_right_64_6
; compress_left_64_6
; expand_left_64_6
; shuffle_64_0_6
; unshuffle_64_0_6
; sag_64_6
; inv_sag_64_6
; index_of_bit_i_64

;
bits 64

%macro PROC 1
  align 16
  global %1
  %1:
%endmacro


;=====
; Working size: 32 bits

PROC compress_right_32_5  ; (value:ecx; mask:edx):eax
; = compress_right_32(value,mask,5)
  pext eax,ecx,edx
  ret

PROC expand_right_32_5  ; (value:ecx; mask:edx):eax
; = expand_right_32(value,mask,5)
  pdep eax,ecx,edx
  ret

PROC compress_left_32_5  ; (value:ecx; mask:edx):eax
; = compress_left_32(value,mask,5)
  pext eax,ecx,edx
  popcnt ecx,edx
  ror eax,cl
  ret

PROC expand_left_32_5  ; (value:ecx; mask:edx):eax
; = expand_left_32_5(value,mask,5)
  mov eax,ecx
  popcnt ecx,edx
  rol eax,cl
  pdep eax,eax,edx
  ret

PROC shuffle_32_0_5  ; (value:ecx):eax
; = shuffle_32(value,0,5)
  mov eax,0x55555555
  pdep edx,ecx,eax
  shr ecx,16
  not eax
  pdep eax,ecx,eax
  or eax,edx
  ret

PROC unshuffle_32_0_5  ; (value:ecx):eax
; = unshuffle_32(value,0,5)
  mov eax,0x55555555
  pext edx,ecx,eax
  not eax
  pext eax,ecx,eax
  shl eax,16
  or eax,edx
  ret

PROC sag_32_5  ; (value:ecx; mask:edx):eax
; = sag_32(value,mask,5)
  pext r8d,ecx,edx
  not edx
  pext eax,ecx,edx
  popcnt ecx,edx
  ror eax,cl
  or eax,r8d
  ret

PROC inv_sag_32_5  ; (value:ecx; mask:edx):eax
; = inv_sag_32(value,mask,5)
  pdep r8d,ecx,edx
  mov eax,ecx
  popcnt ecx,edx
  not edx
  ror eax,cl
  pdep eax,eax,edx
  or eax,r8d
  ret

PROC index_of_bit_i_32  ; (i:ecx; source:edx):eax
; Compute the index of the i-th (i:0..31) set bit
; if present => 0..31; if not present => -1
; i must be in 0..31 (unchecked)
; 2012-01-15 Sigrid/Jasper Neumann; last change: 2012-01-28
  mov eax,1
  shl eax,cl
  pdep eax,eax,edx
  mov ecx,-1        ; indicate not present
  bsr eax,eax
  cmovz eax,ecx     ; replacement of conditional jump
  ret

; All AMD and Intel processors seem to leave the target of bsr as is
; if fed with 0. AMD documents this but Intel leaves it as unspecified.
; Thanks to Harold Aptroot (2013-06-07) for pointing this out.
; Thus the code could be optimized as this:
;  mov eax,1
;  shl eax,cl
;  pdep ecx,eax,edx
;  mov eax,-1        ; indicate not present
;  bsr eax,ecx
;  ret

PROC mask_bit_i_32  ; (i:eax; source:edx):eax
; Compute the mask of the i-th (i:0..31) set bit
; if present => one bit set; if not present => 0
; i must be in 0..31 (unchecked)
; 2013-08-15 Sigrid/Jasper Neumann
  mov eax,1
  shl eax,cl
  pdep eax,eax,edx
  ret


;=====
; Working size: 64 bits

PROC compress_right_64_6  ; (value:ecx; mask:edx):eax
; = compress_right_64(value,mask,6)
  pext rax,rcx,rdx
  ret

PROC expand_right_64_6  ; (value:rcx; mask:rdx):rax
; = expand_right_64(value,mask,6)
  pdep rax,rcx,rdx
  ret

PROC compress_left_64_6  ; (value:rcx; mask:rdx):rax
; = compress_left_64(value,mask,6)
  pext rax,rcx,rdx
  popcnt rcx,rdx
  ror rax,cl
  ret

PROC expand_left_64_6  ; (value:rcx; mask:rdx):rax
; = expand_left_64(value,mask,6)
  mov rax,rcx
  popcnt rcx,rdx
  rol rax,cl
  pdep rax,rax,rdx
  ret

PROC shuffle_64_0_6  ; (value:rcx):rax
; = shuffle_64(value,0,6)
  mov rax,0x5555555555555555
  pdep rdx,rcx,rax
  shr rcx,32
  not rax
  pdep rax,rcx,rax
  or rax,rdx
  ret

PROC unshuffle_64_0_6  ; (value:rcx):rax
; = unshuffle_64(value,0,6)
  mov rax,0x5555555555555555
  pext rdx,rcx,rax
  not rax
  pext rax,rcx,rax
  shl rax,32
  or rax,rdx
  ret

PROC sag_64_6  ; (value:rcx; mask:rdx):rax
; = sag_64(value,mask,6)
  pext r8,rcx,rdx
  not rdx
  pext rax,rcx,rdx
  popcnt rcx,rdx
  ror rax,cl
  or rax,r8
  ret

PROC inv_sag_64_6  ; (value:rcx; mask:rdx):rax
; = inv_sag_64(value,mask,6)
  pdep r8,rcx,rdx
  mov rax,rcx
  popcnt rcx,rdx
  not rdx
  ror rax,cl
  pdep rax,rax,rdx
  or rax,r8
  ret

PROC index_of_bit_i_64  ; (i:eax; source:rdx):eax
; Compute the index of the i-th (i:0..63) set bit
; if present => 0..63; if not present => -1
; i must be in 0..63 (unchecked)
; 2012-01-15 Sigrid/Jasper Neumann; last change: 2013-08-15
  mov eax,1         ; upper dword filled with 0
  shl rax,cl
  pdep rax,rax,rdx
  mov ecx,-1        ; indicate not present
  bsr rax,rax       ; upper dword not needed
  cmovz eax,ecx     ; replacement of conditional jump
  ret

; All AMD and Intel processors seem to leave the target of bsr as is
; if fed with 0. AMD documents this but Intel leaves it as unspecified.
; Thanks to Harold Aptroot (2013-06-07) for pointing this out.
; Thus the code could be optimized as this:
;  mov eax,1         ; upper dword filled with 0
;  shl rax,cl
;  pdep rcx,rax,rdx
;  mov eax,-1        ; indicate not present
;  bsr rax,rcx       ; upper dword not needed
;  ret

PROC mask_bit_i_64  ; (i:eax; source:edx):eax
; Compute the mask of the i-th (i:0..63) set bit
; if present => one bit set; if not present => 0
; i must be in 0..63 (unchecked)
; 2013-08-15 Sigrid/Jasper Neumann
  mov eax,1         ; upper dword filled with 0
  shl rax,cl
  pdep rax,rax,rdx
  ret

; eof.
