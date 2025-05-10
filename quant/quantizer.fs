\ Quantizer
\
\ - Scale
\ - Produce notes (CV/DAC)
\ - Receive notes (CV/ADC)
\ - Change scale
\ - Display note
\ - Display scale
\ - Autotune
\ - ARP
\ - Bass note / second voice
\ - Chord progression
\ - transpose?
\ - Memory?
\
\ HW:
\ - CV in
\ - CV out
\ - 12 buttons
\ - Note display (FF)
\

\ : cornerstone ( Name ) ( -- )
\   <builds begin here $7FF and while 0 h, repeat
\   does>   begin dup  $7FF and while 2+   repeat
\           eraseflashfrom
\ ;

\ #require stm32l432-ra/lib_registers.txt
\ #require stm32l432-ra/lib_systick.txt
\ #require stm32l432-ra/lib_adc.txt

\ compiletoram

33000 constant VCC      \ output range .1 mV
33000 constant octaves \ total range 0.1 mV


36 constant midi-min
33000 constant cv-max
10000 12 / constant cv-tone \ 1V/Oct

0 variable q-loop
0 variable q-in

\ H B A G#G F#
  1 1 1 1 1 1
  1 1 1 1 1 1 12 nvariable notes
\ F E D#D C#C

: note-active? ( nr -- flag )
  cells notes + @ 0<>
;

: q-note-set ( val tone -- )
  cells notes + !
;

: q-note-toggle ( tone -- )
  cells notes + 0 bit swap xor!
;

: q-notes-set ( val -- )
  12 0 do dup i q-note-set loop
  drop
;

: q-notes-none ( -- )
  0 q-notes-set
;

: q-notes-all ( -- )
  1 q-notes-set
;

: q-chord-major ( root -- )
  q-notes-none
  1 over q-note-set
  1 over 7 + 12 mod q-note-set
  1 swap 9 + 12 mod q-note-set
;

\ FIXME: mask wrong?
\ : q-note-toggle ( tone -- )
\   0 bit swap cells notes + xor!
\ ;

: s2$ ( n -- len addr )
  s>d <# #s #>
;

: s>f ( n -- f )
  0 swap
inline 1-foldable ;

: f>s ( f -- n )
  nip
inline 2-foldable ;

: note2name ( note -- len addr )
  case
    0 of s" C" endof
    1 of s" C#" endof
    2 of s" D" endof
    3 of s" D#" endof
    4 of s" E" endof
    5 of s" F" endof
    6 of s" F#" endof
    7 of s" G" endof
    8 of s" G#" endof
    9 of s" A" endof
    10 of s" B" endof
    11 of s" H" endof
    s2$
  endcase
;

: midi2name ( midi -- oct len addr )
  midi-min -
  12 /mod swap ( oct note )
  note2name
;

: midi-type ( midi -- )
  midi2name type .
;

: display-notes ( -- )
  12 0 do
    cr i dup
    note2name type ." : "
    note-active? if ." 1" else ." 0" then
  loop
;


: cv-type ( cv -- )
  dup cr ." CV: "
  s>f 10000,0 f/ 4 f.n
  ." V"
;

\ --- Input -----------------------

: q-adc-init
  adc-init
;

: adc-shutdown
  adc-stop
  adc-deinit
;

: adc>volt ( adc.u -- volt )
  vcc *
  12 rshift
  \ scaling factor: vcc octaves / *
;

: volt>midi-cents ( volt -- midi )
  100 *
  cv-tone /       ( midi-cents )
  midi-min 100 * +
;

\\ fake some variety
: quant-flutter ( time note -- time note )
  over 30 mod 15 - 100 *
  +
;

\ \ mock ADC
\ 0 variable flutter
\ : adc-next ( -- volt )
\   1 flutter +!
\   flutter @ 2000
\   quant-flutter
\   nip
\   4096 min
\   0 max
\   dup adc>volt cr ." (CV IN): " cv-type
\ ;

\ returns midi note cents, e.g. C4 = 6000
: quant-cv-read ( -- note )
  adc-next ( adc up to 4096 )
  dup cr ." in: " .
  adc>volt
  volt>midi-cents
  dup cr ." cents: " .
;

\ --- Output -----------------------

: dac-init ( -- )
  29 bit RCC _rAPB1ENR1 bis! \ enable DAC clock
  10 0 do loop               \ delay at least 2 cycles
  0 bit DAC _daCR bis!       \ enable DAC1
  MODE_ANALOG 4 PORTA SET-MODER
;

: dac-shutdown ( -- )
  0 bit DAC _daCR bic!
  29 bit RCC _rAPB1ENR1 bic!
;

: quant-midi>cv ( midi -- cv )
  midi-min -
  cv-tone *
  0 max
  cv-max min
;

: volt>dac ( u -- u )
  \ scaling factor: octaves vcc / * ?
  12 lshift
  VCC /
  $FFF min
  0 max
;

: dac1! ( volt -- )
  volt>dac ( dac up to 4095 )
  dup cr ." dac: " .
  DAC _daDHR12R1 !
;

: quant-cv-write ( midi -- )
  quant-midi>cv
  dup cv-type
  dac1!
;
\
\ --- Quantizing --------------------

: quant-note-round ( note -- midi )
  100 /mod swap    ( midi rest )
  50 >=
  if 1 + then
;

: even?
  1 and 0=
;

\ Calculates an offset for up/down search
\ 0 -1 1 -2 2 ...
: quant-offset ( cnt -- offset )
  12 over - 2/ ( cnt dist )
  swap even? if negate then
;

: quant-idx ( note cnt -- idx)
  quant-offset ( range -6 to  5 )
  +            ( range -6 to 16 )
  12 +         ( range  6 to 28 )
  12 mod       ( idx 0 to 11 )
;

: quant-quantize-note ( note -- quant )
  11           ( note cnt )
  begin
    2dup quant-idx  ( note cnt idx )
    dup note-active? if
      -rot 2drop 0  ( quant 0 )
    else
      drop          ( note idx )
    then
    1-
    dup 0<
  until
  drop ( quant )
;

\ Quantizes the midi note
\ - splits octave and tone
\ - quantizes the tone according to active
\ - detect whether the octave was left
\   (distance of absolute tones > 6)
\ - correct and apply the octave
: quant-quantize ( midi -- midi )
  12 /mod swap            ( oct note )
  dup quant-quantize-note ( oct note quant )
  tuck -                  ( oct quant dist )
  dup abs 6 > if \ left the octave
    0< if                 ( oct quant )
      swap 1-
    else
      swap 1+
    then
  else
    drop swap
  then                    ( quant oct )
  12 * +
;

\ --- Main -----------------------

: quant-step ( -- )
  quant-cv-read
  quant-note-round
  dup quant-quantize swap ( quant in )
  cr ." Note: " midi-type ( quant )
  dup ." -> " midi-type
  quant-cv-write
;

: t 10 0 do quant-step cr cr loop ;

: quant-reset ( -- )
  0 dac1!
;

: quant-loop ( time -- )
  0
  init-Systimer
  cr ." Running"
  begin
    1+
    quant-step
    cr
    drop
	\ dup delay-ms
    over 0 do loop
	key?
  until
  drop
  quant-reset
;

: quant-run ( -- )
  $FFFFF quant-loop
;

: quant-init
  cr ." Initializing DAC..."
  dac-init
  cr ." Initializing ADC..."
  q-adc-init
  cr ." Quantizing"
;

: quant-shutdown
  dac-shutdown
  adc-shutdown
;
