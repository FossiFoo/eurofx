\ ## ADC start
\ enable timer (RCC)
\ clear ADRDY in ISR (write 1)
\ set ADEN
\ wait for ADRDY
\ (set channel SQR)
\ (set time SMPR)
\ write 1 ADSTART

\ ralph.sahli@bluewin.ch

: adc-init-hw
  cr ." ADC Init"
  %11 28 lshift RCC _rCCIPR bis!     \ use SYSCLK for ADC
  13 bit RCC _rAHB2ENR bis!          \ enable ADC clock

  29 bit ADC _adc_CR bic!            \ disable deep sleep
  28 bit ADC _adc_CR bis!            \ enable voltage regulator
;

: adc-cal
  cr ." Calibrating... "
  %1 31 lshift %10111 or ADC _adc_CR bic!
  %1 31 lshift ADC _adc_CR bis!
  begin %1 31 lshift ADC _adc_CR @ and while nop repeat
  ." done"
;

: adc-cfg
  MODE_ANALOG 5 PORTA SET-MODER      \ set PIN to analog mode
  $FFFF ADC _adc_SQR1 bic!
  10 6 lshift ADC _adc_SQR1 bis!      \ PIN 9 (A3/PA_4)
  \ 13 bit ADC $0C + bis!
  %100 15 lshift ADC _adc_SMPR1 bis! \ 47.5 clock cycles
;

: adc-enable
  cr ." Enabling... "
  1 ADC _adc_ISR bis!                \ ack ADRDY
  ." ACK RDY... "
  1 ADC _adc_CR bis!                 \ enable ADC
  ." ADEN... "
  begin 1 ADC _adc_ISR @ dup . and until
  ." done"
;

: adc-init
  adc-init-hw
  adc-cal
  adc-cfg
  adc-enable
;

: adc-deinit
  1 bit ADC _adc_CR bis! \ disable ADC
;

: adc-stop
  4 bit ADC _adc_CR bis! \ stop
;

: adc-start
  2 bit ADC _adc_CR bis! \ start
;

: adc-read
  ADC _adc_DR @
;

: adc-next
  adc-read
  adc-start
;

: adc-status
  ADC _adc_ISR 4 r-dump
;

: d
  ADC _adc_CR 4 r-dump
;

: adc-loop
  init-Systimer
  begin
    adc-start
    cr adc-read
	100 delay-ms
	key?
  until
;
