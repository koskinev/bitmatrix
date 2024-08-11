;=====
; Intro

; Some useful macros for shifting xmm/ymm registers

; (c) 2012..2020 Sigrid/Jasper Neumann
; http://programming.sirrida.de
; E-Mail: info@sirrida.de

; Granted to the public domain
; First version: 2012-07-05
; Last change: 2014-09-01

; You can use the free assembler NASM to assemble these instructions.


;=====
; Macro list

; xpsrldq       bitwise shift 128 bit entities right logical
; xpslldq       bitwise shift 128 bit entities left logical
; vxpsrldq      bitwise shift 128 bit entities right logical, avx variant
; vxpslldq      bitwise shift 128 bit entities left logical, avx variant

%macro xpslldq 2-3
; Simulate "pslldq xmmreg,shift" but shifting bits instead of bytes.
; Needs SSE2.
; %1: xmm register to be shifted
; %2: shift-count (constant)
; %3: temp xmm register, needed when %2 is not divisible by 8 and %2 < 64

  %if (%2) < 0
    %error Shift negative
  %elif (%2) >= 128
    %warning Shift too large, zeroing
    pxor %1,%1
  %elif ((%2) & 7) = 0
    %if (%2) <> 0
      pslldq %1, (%2)/8  ; bytewise shift is sufficient
    %endif
  %elif (%2) >= 64
    pslldq %1, 64/8  ; shift by 64 bit
    psllq %1, (%2)-64  ; shift the remaining amount
  %else  ; emulate shifting by 1..63 bit
    %ifempty %3
      %error Temp register needed
    %endif
    movdqa %3, %1
    pslldq %1, 64/8
    psllq %3, (%2)
    psrlq %1, 64-(%2)
    por %1, %3
  %endif
%endmacro

%macro xpsrldq 2-3
; Simulate "psrldq xmmreg,shift" but shifting bits instead of bytes.
; Needs SSE2.
; %1: xmm register to be shifted
; %2: shift-count (constant)
; %3: temp xmm register, needed when %2 is not divisible by 8 and %2 < 64

  %if (%2) < 0
    %error Shift negative
  %elif (%2) >= 128
    %warning Shift too large, zeroing
    pxor %1,%1
  %elif ((%2) & 7) = 0
    %if (%2) <> 0
      psrldq %1, (%2)/8
    %endif
  %elif (%2) >= 64
    psrldq %1, 64/8
    psrlq %1, (%2)-64
  %else
    %ifempty %3
      %error Temp register needed
    %endif
    movdqa %3, %1
    psrldq %1, 64/8
    psrlq %3, (%2)
    psllq %1, 64-(%2)
    por %1, %3
  %endif
%endmacro

%macro vxpslldq 2-3
; Simulate "vpslldq xmmreg,shift" but shifting bits instead of bytes.
; AVX variant of xpslldq possibly saving one movdqa.
; Can also be used with AVX2/YMM to do the same in two lanes at once.
; %1: xmm register to be shifted
; %2: shift-count (constant)
; %3: temp xmm register, needed when %2 is not divisible by 8 and %2 < 64

  %if (%2) < 0
    %error Shift negative
  %elif (%2) >= 128
    %warning Shift too large, zeroing
    vpxor %1,%1
  %elif ((%2) & 7) = 0
    %if (%2) <> 0
      vpslldq %1, (%2)/8
    %endif
  %elif (%2) >= 64
    vpslldq %1, 64/8
    vpsllq %1, (%2)-64
  %else
    %ifempty %3
      %error Temp register needed
    %endif
    vpslldq %3, %1, 64/8
    vpsllq %1, (%2)
    vpsrlq %3, 64-(%2)
    vpor %1, %3
  %endif
%endmacro

%macro vxpsrldq 2-3
; Simulate "vpsrldq xmmreg,shift" but shifting bits instead of bytes.
; AVX variant of xpsrldq possibly saving one movdqa.
; Can also be used with AVX2/YMM to do the same in two lanes at once.
; %1: xmm register to be shifted
; %2: shift-count (constant)
; %3: temp xmm register, needed when %2 is not divisible by 8 and %2 < 64

  %if (%2) < 0
    %error Shift negative
  %elif (%2) >= 128
    %warning Shift too large, zeroing
    vpxor %1,%1
  %elif ((%2) & 7) = 0
    %if (%2) <> 0
      vpsrldq %1, (%2)/8
    %endif
  %elif (%2) >= 64
    vpsrldq %1, 64/8
    vpsrlq %1, (%2)-64
  %else
    %ifempty %3
      %error Temp register needed
    %endif
    vpsrldq %3, %1, 64/8
    vpsrlq %1, (%2)
    vpsllq %3, 64-(%2)
    vpor %1, %3
  %endif
%endmacro

; EOF.
