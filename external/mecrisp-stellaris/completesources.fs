
\ Partial ARM Cortex M3/M4 Disassembler, Copyright (C) 2013  Matthias Koch
\ This is free software under GNU General Public License v3.
\ Knows all M0 and some M3/M4 machine instructions,
\ resolves call entry points, literal pools and handles inline strings.
\ Usage: Specify your target address in disasm-$ and give disasm-step some calls.

\ ------------------------
\  A quick list of words
\ ------------------------

: list ( -- )
  cr
  dictionarystart
  begin
    dup 6 + ctype space
    dictionarynext
  until
  drop
;

\ ---------------------------------------
\  Memory pointer and instruction fetch
\ ---------------------------------------

0 variable disasm-$   \ Current position for disassembling

: disasm-fetch        \ ( -- Data ) Fetches opcodes and operands, increments disasm-$
    disasm-$ @ h@     \             Holt Opcode oder Operand, incrementiert disasm-$
  2 disasm-$ +!   ;

\ --------------------------------------------------
\  Try to find address as code start in Dictionary
\ --------------------------------------------------

: disasm-string ( -- ) \ Takes care of an inline string
  disasm-$ @ dup ctype skipstring disasm-$ !
;

: name. ( Address -- ) \ If the address is Code-Start of a dictionary word, it gets named.
  1 bic \ Thumb has LSB of address set.

  >r
  dictionarystart
  begin
    dup   6 + dup skipstring r@ = if ."   --> " ctype else drop then
    dictionarynext
  until
  drop
  r>

  case \ Check for inline strings ! They are introduced by calls to ." or s" internals.
    ['] ." $1E + of ."   -->  .' " disasm-string ." '" endof \ It is ." runtime ?
    ['] s"  $4 + of ."   -->  s' " disasm-string ." '" endof \ It is .s runtime ?
    ['] c"  $4 + of ."   -->  c' " disasm-string ." '" endof \ It is .c runtime ?
  endcase
;

\ -------------------
\  Beautiful output
\ -------------------

: u.4  0 <# # # # # #> type ;
: u.8  0 <# # # # # # # # # #> type ;
: u.ns 0 <# #s #> type ;
: const. ."  #" u.ns ;
: addr. u.8 ;

: register. ( u -- )
  case
    13 of ."  sp" endof
    14 of ."  lr" endof
    15 of ."  pc" endof
    dup ."  r" decimal u.ns hex
  endcase ;

\ ----------------------------------------
\  Disassembler logic and opcode cutters
\ ----------------------------------------

: opcode? ( Opcode Bits Mask -- Opcode ? ) \ (Opcode and Mask) = Bits
  rot ( Bits Mask Opcode )
  tuck ( Bits Opcode Mask Opcode )
  and ( Bits Opcode Opcode* )
  rot ( Opcode Opcode* Bits )
  =
;

: reg.    ( Opcode Position -- Opcode ) over swap rshift  $7 and register. ;
: reg16.  ( Opcode Position -- Opcode ) over swap rshift  $F and register. ;
: reg16split. ( Opcode -- Opcode ) dup $0007 and over 4 rshift $0008 and or register. ;
: registerlist. ( Opcode -- Opcode ) 8 0 do dup 1 i lshift and if i register. space then loop ;

: imm3. ( Opcode Position -- Opcode ) over swap rshift  $7  and const. ;
: imm5. ( Opcode Position -- Opcode ) over swap rshift  $1F and const. ;
: imm8. ( Opcode Position -- Opcode ) over swap rshift  $FF and const. ;

: imm3<<1. ( Opcode Position -- Opcode ) over swap rshift  $7  and shl const. ;
: imm5<<1. ( Opcode Position -- Opcode ) over swap rshift  $1F and shl const. ;
: imm8<<1. ( Opcode Position -- Opcode ) over swap rshift  $FF and shl const. ;

: imm3<<2. ( Opcode Position -- Opcode ) over swap rshift  $7  and shl shl const. ;
: imm5<<2. ( Opcode Position -- Opcode ) over swap rshift  $1F and shl shl const. ;
: imm7<<2. ( Opcode Position -- Opcode ) over swap rshift  $7F and shl shl const. ;
: imm8<<2. ( Opcode Position -- Opcode ) over swap rshift  $FF and shl shl const. ;

: condition. ( Condition -- )
  case
    $0 of ." eq" endof  \ Z set
    $1 of ." ne" endof  \ Z clear
    $2 of ." cs" endof  \ C set
    $3 of ." cc" endof  \ C clear

    $4 of ." mi" endof  \ N set
    $5 of ." pl" endof  \ N clear
    $6 of ." vs" endof  \ V set
    $7 of ." vc" endof  \ V clear

    $8 of ." hi" endof  \ C set Z clear
    $9 of ." ls" endof  \ C clear or Z set
    $A of ." ge" endof  \ N == V
    $B of ." lt" endof  \ N != V

    $C of ." gt" endof  \ Z==0 and N == V
    $D of ." le" endof  \ Z==1 or N != V
  endcase
;

: imm12. ( Opcode -- Opcode )
  dup $FF and                 \ Bits 0-7
  over  4 rshift $700 and or  \ Bits 8-10
  over 15 rshift $800 and or  \ Bit  11
  ( Opcode imm12 )
  dup 8 rshift
  case
    0 of $FF and                                  const. endof \ Plain 8 Bit Constant
    1 of $FF and                 dup 16 lshift or const. endof \ 0x00XY00XY
    2 of $FF and        8 lshift dup 16 lshift or const. endof \ 0xXY00XY00
    3 of $FF and dup 8 lshift or dup 16 lshift or const. endof \ 0xXYXYXYXY

    \ Shifted 8-Bit Constant
    swap
      \ Otherwise, the 32-bit constant is rotated left until the most significant bit is bit[7]. The size of the left
      \ rotation is encoded in bits[11:7], overwriting bit[7]. imm12 is bits[11:0] of the result.
      dup 7 rshift swap $7F and $80 or swap rrotate const.
  endcase
;

\ --------------------------------------
\  Name resolving for blx r0 sequences
\ --------------------------------------

0 variable destination-r0

\ ----------------------------------
\  Single instruction disassembler
\ ----------------------------------

: disasm-thumb-2 ( Opcode16 -- Opcode16 )
  dup 16 lshift disasm-fetch or ( Opcode16 Opcode32 )

  $F000D000 $F800D000 opcode? if  \ BL
                                ( Opcode )
                                ." bl  "
                                dup $7FF and ( Opcode DestinationL )
                                over ( Opcode DestinationL Opcode )
                                16 rshift $7FF and ( Opcode DestinationL DestinationH )
                                dup $400 and if $FFFFF800 or then ( Opcode DestinationL DestinationHsigned )
                                11 lshift or ( Opcode Destination )
                                shl
                                disasm-$ @ +
                                dup addr. name. \ Try to resolve destination
                              then

  \ MOVW / MOVT
  \ 1111 0x10 t100 xxxx 0xxx dddd xxxx xxxx
  \ F    2    4    0    0    0    0    0
  \ F    B    7    0    8    0    0    0

  $F2400000 $FB708000 opcode? if \ MOVW / MOVT
                                ( Opcode )
                                dup $00800000 and if ." movt"
                                                  else ." movw"
                                                  then

                                8 reg16. \ Destination register

                                \ Extract 16 Bit constant from opcode:
                                dup        $FF and              ( Opcode Constant* )
                                over     $7000 and  4 rshift or ( Opcode Constant** )
                                over $04000000 and 15 rshift or ( Opcode Constant*** )
                                over $000F0000 and  4 rshift or ( Opcode Constant )
                                dup ."  #" u.4
                                ( Opcode Constant )
                                over $00800000 and if 16 lshift destination-r0 @ or destination-r0 !
                                                   else                             destination-r0 !
                                                   then
                              then

  \
  \ 1111 0i0x xxxs nnnn 0iii dddd iiii iiii
  \ F    0    0    0    0    0    0    0
  \ F    A    0    0    8    0    0    0

  $F0000000 $FA008000 opcode? not if else \ Data processing, modified 12-bit immediate
                                dup 21 rshift $F and
                                case
                                  %0000 of ." and" endof
                                  %0001 of ." bic" endof
                                  %0010 of ." orr" endof
                                  %0011 of ." orn" endof
                                  %0100 of ." eor" endof
                                  %1000 of ." add" endof
                                  %1010 of ." adc" endof
                                  %1011 of ." sbc" endof
                                  %1101 of ." sub" endof
                                  %1110 of ." rsb" endof
                                  ." ?"
                                endcase
                                dup 1 20 lshift and if ." s" then \ Set Flags ?
                                8 reg16. 16 reg16. \ Destionation and Source registers
                                imm12.
                              then

  case \ Decode remaining "singular" opcodes used in Mecrisp-Stellaris:

    $F8470D04 of ." str r0 [ r7 #-4 ]!" endof
    $F8471D04 of ." str r1 [ r7 #-4 ]!" endof
    $F8472D04 of ." str r2 [ r7 #-4 ]!" endof
    $F8473D04 of ." str r3 [ r7 #-4 ]!" endof
    $F8476D04 of ." str r6 [ r7 #-4 ]!" endof

    $F8576026 of ." ldr r6 [ r7 r6 lsl #2 ]" endof
    $F85D6C08 of ." ldr r6 [ sp #-8 ]" endof

    $FAB6F686 of ." clz r6 r6" endof

    $FB90F6F6 of ." sdiv r6 r0 r6" endof
    $FBB0F6F6 of ." udiv r6 r0 r6" endof
    $FBA00606 of ." umull r0 r6 r0 r6" endof
    $FBA00806 of ." smull r0 r6 r0 r6" endof

  endcase \ Case drops Opcode32
  ( Opcode16 )
;

: disasm ( -- ) \ Disassembles one machine instruction and advances disasm-$

disasm-fetch \ Instruction opcode on stack the whole time.

$4140 $FFC0 opcode? if ." adcs"  0 reg. 3 reg. then          \ ADC
$1C00 $FE00 opcode? if ." adds" 0 reg. 3 reg. 6 imm3. then   \ ADD(1) small immediate two registers
$3000 $F800 opcode? if ." adds" 8 reg. 0 imm8. then          \ ADD(2) big immediate one register
$1800 $FE00 opcode? if ." adds" 0 reg. 3 reg. 6 reg. then    \ ADD(3) three registers
$4400 $FF00 opcode? if ." add"  reg16split. 3 reg16. then    \ ADD(4) two registers one or both high no flags
$A000 $F800 opcode? if ." add"  8 reg. ."  pc " 0 imm8<<2. then  \ ADD(5) rd = pc plus immediate
$A800 $F800 opcode? if ." add"  8 reg. ."  sp " 0 imm8<<2. then  \ ADD(6) rd = sp plus immediate
$B000 $FF80 opcode? if ." add sp" 0 imm7<<2. then            \ ADD(7) sp plus immediate

$4000 $FFC0 opcode? if ." ands" 0 reg. 3 reg. then           \ AND
$1000 $F800 opcode? if ." asrs" 0 reg. 3 reg. 6 imm5. then   \ ASR(1) two register immediate
$4100 $FFC0 opcode? if ." asrs" 0 reg. 3 reg. then           \ ASR(2) two register
$D000 $F000 opcode? not if else dup $0F00 and 8 rshift       \ B(1) conditional branch
                       case
                         $00 of ." beq" endof  \ Z set
                         $01 of ." bne" endof  \ Z clear
                         $02 of ." bcs" endof  \ C set
                         $03 of ." bcc" endof  \ C clear

                         $04 of ." bmi" endof  \ N set
                         $05 of ." bpl" endof  \ N clear
                         $06 of ." bvs" endof  \ V set
                         $07 of ." bvc" endof  \ V clear

                         $08 of ." bhi" endof  \ C set Z clear
                         $09 of ." bls" endof  \ C clear or Z set
                         $0A of ." bge" endof  \ N == V
                         $0B of ." blt" endof  \ N != V

                         $0C of ." bgt" endof  \ Z==0 and N == V
                         $0D of ." ble" endof  \ Z==1 or N != V
                         \ $0E: Undefined Instruction
                         $0F of ." swi"  0 imm8.   drop exit endof
                       endcase
                       space
                       dup $FF and dup $80 and if $FFFFFF00 or then
                       shl disasm-$ @ 1 bic + 2 + addr.
                    then

$E000 $F800 opcode? if ." b"                                 \ B(2) unconditional branch
                      dup $7FF and shl
                      dup $800 and if $FFFFF000 or then
                      disasm-$ @ + 2+
                      space addr.
                    then

$4380 $FFC0 opcode? if ." bics" 0 reg. 3 reg. then           \ BIC
$BE00 $FF00 opcode? if ." bkpt" 0 imm8. then                 \ BKPT

\ BL/BLX handled as Thumb-2 instruction on M3/M4.

$4780 $FF87 opcode? if ." blx"  3 reg16. then                \ BLX(2)
$4700 $FF87 opcode? if ." bx"   3 reg16. then                \ BX
$42C0 $FFC0 opcode? if ." cmns" 0 reg. 3 reg. then           \ CMN
$2800 $F800 opcode? if ." cmp"  8 reg. 0 imm8. then          \ CMP(1) compare immediate
$4280 $FFC0 opcode? if ." cmp"  0 reg. 3 reg. then           \ CMP(2) compare register
$4500 $FF00 opcode? if ." cmp"  reg16split. 3 reg16. then    \ CMP(3) compare high register
$B660 $FFE8 opcode? if ." cps"  0 imm5. then                 \ CPS
$4040 $FFC0 opcode? if ." eors" 0 reg. 3 reg. then           \ EOR

$C800 $F800 opcode? if ." ldmia" 8 reg. ."  {" registerlist. ." }" then     \ LDMIA

$6800 $F800 opcode? if ." ldr" 0 reg. ."  [" 3 reg. 6 imm5<<2. ."  ]" then  \ LDR(1) two register immediate
$5800 $FE00 opcode? if ." ldr" 0 reg. ."  [" 3 reg. 6 reg. ."  ]" then      \ LDR(2) three register
$4800 $F800 opcode? if ." ldr" 8 reg. ."  [ pc" 0 imm8<<2. ."  ]  Literal " \ LDR(3) literal pool
                       dup $FF and shl shl ( Opcode Offset ) \ Offset for PC
                       disasm-$ @ 2+ 3 bic + ( Opcode Address )
                       dup addr. ." : " @ addr. then

$9800 $F800 opcode? if ." ldr"  8 reg. ."  [ sp" 0 imm8<<2. ."  ]" then     \ LDR(4)

$7800 $F800 opcode? if ." ldrb" 0 reg. ."  [" 3 reg. 6 imm5. ."  ]" then    \ LDRB(1) two register immediate
$5C00 $FE00 opcode? if ." ldrb" 0 reg. ."  [" 3 reg. 6 reg.  ."  ]" then    \ LDRB(2) three register

$8800 $F800 opcode? if ." ldrh" 0 reg. ."  [" 3 reg. 6 imm5<<1. ."  ]" then \ LDRH(1) two register immediate
$5A00 $FE00 opcode? if ." ldrh" 0 reg. ."  [" 3 reg. 6 reg.  ."  ]" then    \ LDRH(2) three register

$5600 $FE00 opcode? if ." ldrsb" 0 reg. ."  [" 3 reg. 6 reg. ."  ]" then    \ LDRSB
$5E00 $FE00 opcode? if ." ldrsh" 0 reg. ."  [" 3 reg. 6 reg. ."  ]" then    \ LDRSH

$0000 $F800 opcode? if ." lsls" 0 reg. 3 reg. 6 imm5. then   \ LSL(1)
$4080 $FFC0 opcode? if ." lsls" 0 reg. 3 reg. then           \ LSL(2) two register
$0800 $F800 opcode? if ." lsrs" 0 reg. 3 reg. 6 imm5. then   \ LSR(1) two register immediate
$40C0 $FFC0 opcode? if ." lsrs" 0 reg. 3 reg. then           \ LSR(2) two register
$2000 $F800 opcode? if ." movs" 8 reg. 0 imm8. then          \ MOV(1) immediate
$4600 $FF00 opcode? if ." mov" reg16split. 3 reg16. then     \ MOV(3)

$4340 $FFC0 opcode? if ." muls" 0 reg. 3 reg. then           \ MUL
$43C0 $FFC0 opcode? if ." mvns" 0 reg. 3 reg. then           \ MVN
$4240 $FFC0 opcode? if ." negs" 0 reg. 3 reg. then           \ NEG
$4300 $FFC0 opcode? if ." orrs" 0 reg. 3 reg. then           \ ORR

$BC00 $FE00 opcode? if ." pop {"  registerlist. dup $0100 and if ."  pc " then ." }" then \ POP
$B400 $FE00 opcode? if ." push {" registerlist. dup $0100 and if ."  lr " then ." }" then \ PUSH

$BA00 $FFC0 opcode? if ." rev"   0 reg. 3 reg. then         \ REV
$BA40 $FFC0 opcode? if ." rev16" 0 reg. 3 reg. then         \ REV16
$BAC0 $FFC0 opcode? if ." revsh" 0 reg. 3 reg. then         \ REVSH
$41C0 $FFC0 opcode? if ." rors"  0 reg. 3 reg. then         \ ROR
$4180 $FFC0 opcode? if ." sbcs"  0 reg. 3 reg. then         \ SBC

$C000 $F800 opcode? if ." stmia" 8 reg. ."  {" registerlist. ." }" then     \ STMIA

$6000 $F800 opcode? if ." str" 0 reg. ."  [" 3 reg. 6 imm5<<2. ."  ]" then  \ STR(1) two register immediate
$5000 $FE00 opcode? if ." str" 0 reg. ."  [" 3 reg. 6 reg. ."  ]" then      \ STR(2) three register
$9000 $F800 opcode? if ." str" 8 reg. ."  [ sp + " 0 imm8<<2. ."  ]" then   \ STR(3)

$7000 $F800 opcode? if ." strb" 0 reg. ."  [" 3 reg. 6 imm5. ."  ]" then    \ STRB(1) two register immediate
$5400 $FE00 opcode? if ." strb" 0 reg. ."  [" 3 reg. 6 reg.  ."  ]" then    \ STRB(2) three register

$8000 $F800 opcode? if ." strh" 0 reg. ."  [" 3 reg. 6 imm5<<1. ."  ]" then \ STRH(1) two register immediate
$5200 $FE00 opcode? if ." strh" 0 reg. ."  [" 3 reg. 6 reg.  ."  ]" then    \ STRH(2) three register

$1E00 $FE00 opcode? if ." subs" 0 reg. 3 reg. 6 imm3. then   \ SUB(1)
$3800 $F800 opcode? if ." subs" 8 reg. 0 imm8. then          \ SUB(2)
$1A00 $FE00 opcode? if ." subs" 0 reg. 3 reg. 6 reg. then    \ SUB(3)
$B080 $FF80 opcode? if ." sub sp" 0 imm7<<2. then            \ SUB(4)

$B240 $FFC0 opcode? if ." sxtb" 0 reg. 3 reg. then           \ SXTB
$B200 $FFC0 opcode? if ." sxth" 0 reg. 3 reg. then           \ SXTH
$4200 $FFC0 opcode? if ." tst"  0 reg. 3 reg. then           \ TST
$B2C0 $FFC0 opcode? if ." uxtb" 0 reg. 3 reg. then           \ UXTB
$B280 $FFC0 opcode? if ." uxth" 0 reg. 3 reg. then           \ UXTH


\ 16 Bit Thumb-2 instruction ?

$BF00 $FF00 opcode? not if else                              \ IT...
                      dup $000F and
                      case
                        $8 of ." it" endof

                        over $10 and if else $8 xor then
                        $C of ." itt" endof
                        $4 of ." ite" endof

                        over $10 and if else $4 xor then
                        $E of ." ittt" endof
                        $6 of ." itet" endof
                        $A of ." itte" endof
                        $2 of ." itee" endof

                        over $10 and if else $2 xor then
                        $F of ." itttt" endof
                        $7 of ." itett" endof
                        $B of ." ittet" endof
                        $3 of ." iteet" endof
                        $D of ." ittte" endof
                        $5 of ." itete" endof
                        $9 of ." ittee" endof
                        $1 of ." iteee" endof
                      endcase
                      space
                      dup $00F0 and 4 rshift condition.
                    then

\ 32 Bit Thumb-2 instruction ?

$E800 $F800 opcode? if disasm-thumb-2 then
$F000 $F000 opcode? if disasm-thumb-2 then


\ If nothing of the above hits: Invalid Instruction... They are not checked for.

\ Try name resolving for blx r0 sequences:

$2000 $FF00 opcode? if dup $FF and destination-r0  ! then \ movs r0, #...
$3000 $FF00 opcode? if dup $FF and destination-r0 +! then \ adds r0, #...
$0000 $F83F opcode? if destination-r0 @                   \ lsls r0, r0, #...
                         over $07C0 and 6 rshift lshift
                       destination-r0 ! then
dup $4780 =         if destination-r0 @ name. then        \ blx r0

drop \ Forget opcode
; \ disasm

\ ------------------------------
\  Single instruction printing
\ ------------------------------

: memstamp \ ( Addr -- ) Shows a memory location nicely
    dup u.8 ." : " h@ u.4 ."   " ;

: disasm-step ( -- )
    disasm-$ @                 \ Note current position
    dup memstamp disasm cr     \ Disassemble one instruction

    begin \ Write out all disassembled memory locations
      2+ dup disasm-$ @ <>
    while
      dup memstamp cr
    repeat
    drop
;

\ ------------------------------
\  Disassembler for definitions
\ ------------------------------

: seec ( -- ) \ Continues to see
  base @ hex cr

  begin
    disasm-$ @ h@           $4770 =  \ Flag: Loop terminates with bx lr
    disasm-$ @ h@ $FF00 and $BD00 =  \ Flag: Loop terminates with pop { ... pc }
    or
    disasm-step
  until

  base !
;

: see ( -- ) \ Takes name of definition and shows its contents from beginning to first ret
  ' disasm-$ !
  seec
;

\ -----------------------------------------------------------------------------
\   A few tools for dictionary wizardy
\ -----------------------------------------------------------------------------

: executablelocation? ( addr -- ? )
  dup  addrinram?              \ In RAM
  over flashvar-here u< and     \ and below the variables and buffers
  swap addrinflash? or           \ or in flash ?
;

: link>flags ( addr -- addr* ) 4 + 1-foldable ;
: link>name  ( addr -- addr* ) 6 + 1-foldable ;
: link>code  ( addr -- addr* ) 6 + skipstring ;

0 variable searching-for
0 variable closest-found

: code>link  ( entrypoint -- addr | 0 ) \ Try to find this code start address in dictionary

    searching-for !
  0 closest-found !

  compiletoram? 0= >r  \ Save current compile mode
  compiletoram          \ Always scan in compiletoram mode, in order to also find definitions in RAM.

  dictionarystart
  begin
    dup link>code searching-for @ = if dup closest-found ! then
    dictionarynext
  until
  drop

  r> if compiletoflash then \ Restore compile mode

  closest-found @
;

: inside-code>link ( addr-inside -- addr | 0 ) \ Try to find this address inside of a definition

  dup executablelocation? not if drop 0 exit then  \ Do not try to find locations which are not executable

    searching-for !
  0 closest-found !

  compiletoram? 0= >r  \ Save current compile mode
  compiletoram          \ Always scan in compiletoram mode, in order to also find definitions in RAM.

  dictionarystart
  begin

    dup link>code searching-for @ u<=
    if \ Is the address of this entry BEFORE the address which is to be found ?
      \ Distance to current   Latest best distance
      searching-for @ over -  searching-for @ closest-found @ -  <
      if dup closest-found ! then \ Is the current entry closer to the address which is to be found ?
    then

    dictionarynext
  until
  drop

  r> if compiletoflash then \ Restore compile mode

  \ Do not cross RAM/Flash borders:

  searching-for @ addrinflash?
  closest-found @ addrinflash? xor if 0 else closest-found @ then
;

: traceinside. ( addr -- )
  inside-code>link if
  ." ( "                 closest-found @ link>code   hex.
  ." + " searching-for @ closest-found @ link>code - hex.
  ." ) "
  closest-found @ link>name ctype
  then
;

: variable>link  ( location -- addr | 0 ) \ Try to find this variable or buffer in flash dictionary

    searching-for !
  0 closest-found !

  compiletoram? 0= >r  \ Save current compile mode
  compiletoram          \ Always scan in compiletoram mode, in order to also find definitions in RAM.

  dictionarystart
  begin

    dup link>flags h@   \ Fetch Flags of current definition
    $7FF0 and            \ Snip off visibility bit and reserved length
    dup          $140 =   \ Variables and buffers are either initialised variables or
    swap          $80 = or \ "0-foldable and reserves uninitialised memory" when defined in flash memory.
                            \ Take care: You cannot easily use $40 bit "0-foldable" to check for variables and buffers in RAM.
    if
      dup link>code execute searching-for @ = if dup closest-found ! then
    then

    dictionarynext
  until
  drop

  r> if compiletoflash then \ Restore compile mode

  closest-found @
;

: variable-name. ( addr -- ) \ Print the name of this variable or buffer, if possible
  dup flashvar-here u< if inside-code>link else variable>link then
  ?dup if link>name ctype then
;
\ -----------------------------------------------------------
\   Cooperative Multitasking
\ -----------------------------------------------------------

\ Configuration:

128 cells constant stackspace \ 128 stack elements for every task

\ Internal stucture of task memory:
\  0: Pointer to next task
\  4: Task currently active ?
\  8: Saved stack pointer
\ 12: Handler for Catch/Throw
\  Parameter stack space
\  Return    stack space

false 0 true flashvar-here 4 cells - 4 nvariable boot-task \ Boot task is active, without handler and has no extra stackspace.
boot-task boot-task ! \ For compilation into RAM only

boot-task variable up \ User Pointer
: next-task  ( -- task )    up @ inline ;
: task-state ( -- state )   up @ 1 cells + inline ;
: save-task  ( -- save )    up @ 2 cells + inline ;
: handler    ( -- handler ) up @ 3 cells + inline ;

: (pause) ( stacks fly around )
    [ $B430 h, ]        \ push { r4  r5 } to save I and I'
    rp@ sp@ save-task !  \ save return stack and stack pointer
    begin
      next-task @ up !     \ switch to next running task
    task-state @ until
    save-task @ sp! rp!  \ restore pointers
    unloop ;              \ pop { r4  r5 } to restore the loop registers

: wake ( task -- ) 1 cells +  true swap ! ; \ Wake a random task (IRQ save)
: idle ( task -- ) 1 cells + false swap ! ;  \ Idle a random task (IRQ save)

\ -------------------------------------------------------
\  Round-robin list task handling - do not use in IRQ !
\ -------------------------------------------------------

: stop ( -- ) false task-state ! pause ; \ Stop current task
: multitask  ( -- ) ['] (pause) hook-pause ! ;
: singletask ( -- ) [']  nop    hook-pause ! ;

: task-in-list? ( task -- ? ) \ Checks if a task is currently inside of round-robin list (do not use in IRQ)
  next-task
  begin
    ( Task-Address )
    2dup = if 2drop true exit then
    @ dup next-task = \ Stop when end of circular list is reached
  until
  2drop false
;

: previous ( task -- addr-of-task-before )
  \ Find the task that has the desired one in its next field
  >r next-task begin dup @ r@ <> while @ repeat rdrop
;

: insert ( task -- ) \ Insert a task into the round-robin list
  dup task-in-list?  \ Is the desired task currently linked into ?
  if drop else next-task @ over ! next-task ! then
;

: remove ( task -- ) \ Remove a task from the round-robin list
  dup task-in-list?  \ Is the desired task currently linked into ?
  if dup @ ( task next )
     swap previous ( next previous ) !
  else drop then
;

\ -----------------------------------------
\ Create a new task - do not use in IRQ !
\ -----------------------------------------

: task: ( "name" -- )  stackspace cell+ 2*  4 cells +  buffer: ;

: preparetask ( task continue -- )
  swap >r ( continue R: task )

    \ true  r@ 1 cells + ! \ Currently running
      false r@ 3 cells + ! \ No handler

    r@ 4 cells + stackspace + ( continue start-of-parameter-stack )
      dup   r@ 2 cells + ! \ Start of parameter stack

    dup stackspace + ( continue start-of-parameter-stack start-of-return-stack )
    tuck      ( continue start-of-return-stack start-of-parameter-stack start-of-return-stack )
    2 cells - ( continue start-of-return-stack start-of-parameter-stack start-of-return-stack* ) \ Adjust for saved loop index and limit
    swap  !   ( continue start-of-return-stack ) \ Store the adjusted return stack pointer into the parameter stack
    !         \ Store the desired entry address at top of the tasks return stack

  r> insert
;

: activate ( task --   R: continue -- )
  true over 1 cells + ! \ Currently running
  r> preparetask
;

: background ( task --   R: continue -- )
  false over 1 cells + ! \ Currently idling
  r> preparetask
;

\ --------------------------------------------------
\  Multitasking insight
\ --------------------------------------------------

: tasks ( -- ) \ Show tasks currently in round-robin list
  hook-pause @ singletask \ Stop multitasking as this list may be changed during printout.

  \ Start with current task.
  next-task cr

  begin
    ( Task-Address )
    dup             ." Task Address: " hex.
    dup           @ ." Next Task: " hex.
    dup 1 cells + @ ." State: " hex.
    dup 2 cells + @ ." Stack: " hex.
    dup 3 cells + @ ." Handler: " hex. cr

    @ dup next-task = \ Stop when end of circular list is reached
  until
  drop

  hook-pause ! \ Restore old state of multitasking
;

\ --------------------------------------------------
\  Exception handling
\ --------------------------------------------------

: catch ( x1 .. xn xt -- y1 .. yn throwcode / z1 .. zm 0 )
    [ $B430 h, ]  \ push { r4  r5 } to save I and I'
    sp@ >r handler @ >r rp@ handler !  execute
    r> handler !  rdrop  0 unloop ;

: throw ( throwcode -- )  dup IF
	handler @ 0= IF false task-state ! THEN \ unhandled error: stop task
	handler @ rp! r> handler ! r> swap >r sp! drop r>
	UNLOOP  EXIT
    ELSE  drop  THEN ;

\ Requires dictionary-tools.txt

\ --------------------------------------------------
\  Multitasking insight
\ --------------------------------------------------

: tasks ( -- ) \ Show tasks currently in round-robin list
  hook-pause @ singletask \ Stop multitasking as this list may be changed during printout.

  \ Start with current task.
  next-task cr

  begin
    ( Task-Address )
    dup             ." Task Address: " hex.
    dup           @ ." Next Task: " hex.
    dup 1 cells + @ ." State: " hex.
    dup 2 cells + @ ." Stack: " hex.
    dup 3 cells + @ ." Handler: " hex.
    dup             ." Name: " variable-name. cr

    @ dup next-task = \ Stop when end of circular list is reached
  until
  drop

  hook-pause ! \ Restore old state of multitasking
;

\ --------------------------------------------------
\  Multitasking debug tools
\ --------------------------------------------------

:  depth ( -- n ) up @ boot-task = if  depth    else up @ 4 cells stackspace    + + sp@ - 2 arshift then ;
: rdepth ( -- n ) up @ boot-task = if rdepth 1- else up @ 4 cells stackspace 2* + + rp@ - 2 arshift then ;

: .s ( -- )
  base @ >r decimal depth ." Stack: [" . ." ] " r> base !
  depth >r
  begin
    r@ 0 >
  while
    r@ pick .
    r> 1- >r
  repeat
  rdrop
  ."  TOS: " dup . ."  *>" cr
;

: u.s ( -- )
  base @ >r decimal depth ." Stack: [" . ." ] " r> base !
  depth >r
  begin
    r@ 0 >
  while
    r@ pick u.
    r> 1- >r
  repeat
  rdrop
  ."  TOS: " dup u. ."  *>" cr
;

: h.s ( -- )
  base @ >r decimal depth ." Stack: [" . ." ] " r> base !
  depth >r
  begin
    r@ 0 >
  while
    r@ pick hex.
    r> 1- >r
  repeat
  rdrop
  ."  TOS: " dup hex. ."  *>" cr
;

\ -----------------------------------------------------------------------------
\   Various small tools
\ -----------------------------------------------------------------------------

: 2and ( d1 d2 -- d ) ( l2 h2 l1 h1 ) rot and ( l2 l1 h ) -rot and ( h l ) swap 4-foldable ;
: 2or  ( d1 d2 -- d ) ( l2 h2 l1 h1 ) rot or  ( l2 l1 h ) -rot or  ( h l ) swap 4-foldable ;
: 2xor ( d1 d2 -- d ) ( l2 h2 l1 h1 ) rot xor ( l2 l1 h ) -rot xor ( h l ) swap 4-foldable ;

: 2lshift  ( ud u -- ud* ) 0 ?do dshl loop 3-foldable ;
: 2rshift  ( ud u -- ud* ) 0 ?do dshr loop 3-foldable ;
: 2arshift (  d u --  d* ) 0 ?do d2/  loop 3-foldable ;

: d0<> ( d -- ? ) d0= not 2-foldable ;

: u.4 ( u -- ) 0 <# # # # # #> type ;
: u.2 ( u -- ) 0 <# # # #> type ;

: h.4 ( u -- ) base @ hex swap  u.4  base ! ;
: h.2 ( u -- ) base @ hex swap  u.2  base ! ;

\ -----------------------------------------------------------------------------
\   I/O pin primitives
\ -----------------------------------------------------------------------------

$40020000 constant GPIO-BASE
      $00 constant GPIO.MODER   \ Reset 0 Port Mode Register
                                \   00=Input  01=Output  10=Alternate  11=Analog
      $04 constant GPIO.OTYPER  \ Reset 0 Port Output type register
                                \   (0) Push/Pull vs. (1) Open Drain
      $08 constant GPIO.OSPEEDR \ Reset 0 Output Speed Register
                                \   00=2 MHz  01=25 MHz  10=50 MHz  11=100 MHz
      $0C constant GPIO.PUPDR   \ Reset 0 Pullup / Pulldown
                                \   00=none  01=Pullup  10=Pulldown
      $10 constant GPIO.IDR     \ RO      Input Data Register
      $14 constant GPIO.ODR     \ Reset 0 Output Data Register
      $18 constant GPIO.BSRR    \ WO      Bit set/reset register
      $20 constant GPIO.AFRL    \ Reset 0 Alternate function  low register
      $24 constant GPIO.AFRH    \ Reset 0 Alternate function high register

: bit ( u -- u )  \ turn a bit position into a single-bit mask
  1 swap lshift  1-foldable ;
: bit! ( mask addr f -- )  \ set or clear specified bit(s)
  if bis! else bic! then ;


\ -----------------------------------------------------------------------------
\  Ring buffers by Jean-Claude Wippler
\ -----------------------------------------------------------------------------

\ Ring buffers, for serial ports, etc - size must be 4..256 and power of 2
\ TODO setup is a bit messy right now, should put buffer: word inside init

\ Each ring needs 4 extra bytes for internal housekeeping:
\   addr+0 = ring mask, i.e. N-1
\   addr+1 = put index: 0..255 (needs to be masked before use)
\   addr+2 = get index: 0..255 (needs to be masked before use)
\   addr+3 = spare
\   addr+4..addr+4+N-1 = actual ring buffer, N bytes
\ Example:
\   16 4 + buffer: buf  buf 16 init-ring

: init-ring ( addr size -- )  \ initialise a ring buffer
  1- swap !  \ assumes little-endian so mask ends up in ring+0
;

: c++@ ( addr -- b addr+1 ) dup c@ swap 1+ ;  \ fetch and autoinc byte ptr

: ring-step ( ring 1/2 -- addr )  \ common code for saving and fetching
  over + ( ring ring-g/p ) dup c@ >r ( ring ring-g/p R: g/p )
  dup c@ 1+ swap c!  \ increment byte under ptr
  dup c@ r> and swap 4 + + ;

: ring# ( ring -- u )  \ return current number of bytes in the ring buffer
\ TODO could be turned into a single @ word access and made interrupt-safe
  c++@ c++@ c++@ drop - and ;
: ring? ( ring -- f )  \ true if the ring can accept more data
  dup ring# swap c@ < ;
: >ring ( b ring -- )  \ save byte to end of ring buffer
  1 ring-step c! ;
: ring> ( ring -- b )  \ fetch byte from start of ring buffer
  2 ring-step c@ ;

\ -----------------------------------------------------------------------------
\  Terminal injection
\ -----------------------------------------------------------------------------

128 4 + buffer: character-ring   \ Character buffer for "key"

: inject-character ( -- )
  character-ring ring?
  if
    character-ring >ring
  else
    drop \ Forget characters which would overflow the ring buffer.
  then
;

: inject-string ( addr len -- )
  0 ?do
    dup c@ inject-character
    1+
  loop
  drop
;

: ring-key? ( -- ? )
  serial-key? if serial-key inject-character then
  character-ring ring# 0<>
;

: ring-key ( -- c )
  begin ring-key? until
  character-ring ring>
;

: enable-ring ( -- )
  character-ring 128 init-ring
  ['] ring-key  hook-key  !
  ['] ring-key? hook-key? !
;

: disable-ring ( -- )
  ['] serial-key  hook-key  !
  ['] serial-key? hook-key? !
;

\ -----------------------------------------------------------------------------
\  Sleep mode
\ -----------------------------------------------------------------------------

false variable just-woke-up

: sleep-a-while ( -- )

  \ cr ." Entering sleep mode..." cr

  singletask
  led-green ioc!

  \ Insert a power down sequence here, perhaps cutting clock to peripherals
  \ Simplest possibility: Divide clock down to a very low frequency.

  \ Prepare the row which carries the power button
  %000000010 16 lshift %111111101 or  PE0 io-base GPIO.BSRR +  !
  10 us

  \ Simulate behaviour by busy waiting for PC1 going low
  begin PC1 io@ not until

  \ Insert a power up sequence here, reenabling and initialising peripherals

  true just-woke-up !
  led-green ios!
  multitask

  \ cr ." Welcome back !" cr cr
;

\ -----------------------------------------------------------------------------
\  Key mapping
\ -----------------------------------------------------------------------------

0. 2variable    last-keys
0. 2variable current-keys

: key-press ( xd -- ? )
  2dup current-keys 2@ 2and d0<>
  -rot    last-keys 2@ 2and d0=
  and
;

: key-release ( xd -- ? )
  2dup current-keys 2@ 2and d0=
  -rot    last-keys 2@ 2and d0<>
  and
;

false variable shift-lock
false variable alpha-lock

: normal?     ( -- ? ) shift-lock @ not alpha-lock @ not and ;
: shift?      ( -- ? ) shift-lock @     alpha-lock @ not and ;
: alpha?      ( -- ? ) shift-lock @ not alpha-lock @     and ;
: shiftalpha? ( -- ? ) shift-lock @     alpha-lock @     and ;

: keym
  postpone key-press
  postpone if

    postpone normal?
    postpone if
    postpone s"
    postpone inject-string
    postpone then

    postpone shift?
    postpone if
    postpone s"
    postpone inject-string
    postpone then

    postpone alpha?
    postpone if
    postpone s"
    postpone inject-string
    postpone then

    postpone shiftalpha?
    postpone if
    postpone s"
    postpone inject-string
    postpone then

  postpone then
  immediate
;

: keymap ( -- )

  \ ---------------------------------------------
  $00000000.00000001 key-press if 27 inject-character 91 inject-character 68 inject-character then \ Cursor left
  $00000000.00000002 key-press if 27 inject-character 91 inject-character 65 inject-character then \ Cursor up
  $00000000.00000004 key-press if 27 inject-character 91 inject-character 66 inject-character then \ Cursor down
  $00000000.00000008 key-press if 27 inject-character 91 inject-character 67 inject-character then \ Cursor right
  \ ---------------------------------------------
  $00000000.00000010 key-press if 27 inject-character 91 inject-character 72 inject-character then \ OK --> Pos1
  $00000000.00000020 key-press if 27 inject-character 91 inject-character 70 inject-character then \ Return key --> End
  $00000000.00000040 key-press if 4 @ execute then \ Home --> Restart Mecrisp-Stellaris
  $00000000.00000080 key-release if just-woke-up @ if false just-woke-up ! else sleep-a-while then then \ Power button --> Sleep mode
  \ ---------------------------------------------
  $00000000.00001000 key-press if -1 shift-lock xor! then
  $00000000.00002000 key-press if -1 alpha-lock xor! then
  $00000000.00004000 keym x,n,t"cut":":"
  $00000000.00008000 keym var"copy";";"
  $00000000.00010000 keym Nut&Bolt"paste"'"'"
  $00000000.00020000 key-press if 8 inject-character then \ Backspace
  \ ---------------------------------------------
  $00000000.00040000 keym e^x"["a"A"
  $00000000.00080000 keym ln"]"b"B"
  $00000000.00100000 keym log"{"c"C"
  $00000000.00200000 keym i"}"d"D"
  $00000000.00400000 keym ,"_"e"E"
  $00000000.00800000 keym x^y"sto>"f"F"
  \ ---------------------------------------------
  $00000000.01000000 keym sin"asin"g"G"
  $00000000.02000000 keym cos"acos"h"H"
  $00000000.04000000 keym tan"atan"i"I"
  $00000000.08000000 keym Pi"="j"J"
  $00000000.10000000 keym sqrt"<"k"K"
  $00000000.20000000 keym ^2">"l"L"
  \ ---------------------------------------------
  $00000000.40000000 keym 7"7"m"M"
  $00000000.80000000 keym 8"8"n"N"
  $00000001.00000000 keym 9"9"o"O"
  $00000002.00000000 keym ("("p"P"
  $00000004.00000000 keym )")"q"Q"
  \ ---------------------------------------------
  $00000010.00000000 keym 4"4"r"R"
  $00000020.00000000 keym 5"5"s"S"
  $00000040.00000000 keym 6"6"t"T"
  $00000080.00000000 keym *"*"u"U"
  $00000100.00000000 keym /"/"v"V"
  \ ---------------------------------------------
  $00000400.00000000 keym 1"1"w"W"
  $00000800.00000000 keym 2"2"x"X"
  $00001000.00000000 keym 3"3"y"Y"
  $00002000.00000000 keym +"+"z"Z"
  $00004000.00000000 keym -"-" " "
  \ ---------------------------------------------
  $00010000.00000000 keym 0"0"?"?"
  $00020000.00000000 keym ."."!"!"
  $00040000.00000000 keym x10^x"x10^x"x10^x"x10^x"
  $00080000.00000000 key-press if 32 inject-character then \ Space \ keym Ans"Ans"Ans"Ans"
  $00100000.00000000 key-press if 10 inject-character then \ LF    \ keym EXE"EXE"EXE"EXE"
  \ ---------------------------------------------
;

\ -----------------------------------------------------------------------------
\  Keyboard handling
\ -----------------------------------------------------------------------------

: keyboard-handler ( -- )
  scan-keyboard-debounce current-keys 2!

  current-keys 2@ last-keys 2@ 2xor d0<>   \ Any changes in key state ?
  if
    \ ." Key change: " current-keys 2@ hex. hex. cr
    keymap \ Insert a lot of handling here !
  then

  current-keys 2@ last-keys 2!
;

\ -----------------------------------------------------------------------------
\  Task for handling the keyboard in background
\ -----------------------------------------------------------------------------

task: keyboard-task

: keyboard& ( -- )
  keyboard-task activate

    begin
      keyboard-handler
      pause \ Pause is called in the delay used in scan-keyboard, therefore strictly speaking no pause is necessary here.
    again
;

: keyboard-into-terminal ( -- )

  init-led
  init-delay
  init-keyboard
  enable-ring

  led-green ios!

  multitask
  keyboard&
;

\ -------------------------------------------------------------
\  Unicode UTF-8 encoding decoder
\ -------------------------------------------------------------

0 variable utf8collection
0 variable utf8continuation

: utf8-character-length ( c -- c u )
  dup %11000000 and %11000000 = if dup 24 lshift not clz else 1 then ;

: drawcharacter ( c -- ) \ Handles a stream of UTF-8 bytes and translates this into Unicode letters.

  dup 10 = if drop 0 font-x ! 16 font-y +! exit then \ Special handling for line feed.

  utf8continuation @
  if   \ Continue to receive an extended character into buffer

    %00111111 and utf8collection @ 6 lshift or utf8collection !  \ Six more bits
    -1 utf8continuation +!                                       \ One less continuation byte to expect
    utf8continuation @ 0= if utf8collection @ drawunicode then   \ Draw character if complete encoding was buffered.

  else \ Begin of a new character

    utf8-character-length 1- ?dup

    if \ Start of a new character or a sequence
      dup utf8continuation !
      25 + tuck lshift swap rshift \ Remove the length encoding by shifting it out of the register temporarily
      utf8collection !
    else \ One byte characters are classic 7 bit ASCII and can be drawn immediately
      drawunicode
    then

  then
;

\ -------------------------------------------------------------
\  Write a string and split into individual characters
\ -------------------------------------------------------------

: get-first-char ( addr len -- addr   len c ) over c@ ;
: cut-first-char ( addr len -- addr+1 len-1 ) 1- swap 1+ swap ;

: drawstring ( addr u x y -- )
  font-y ! font-x !

  begin
    dup 0<>
  while
    get-first-char
    drawcharacter
    cut-first-char
  repeat
  2drop
;

: cornerstone ( Name ) ( -- )
  <builds begin here sectorborder 0= while 0 h, repeat
  does>   dint singletask
          begin dup  sectorborder 0= while 2+   repeat
          sectorborder 12 swap ?do i eraseflashsector loop reset
;
