
\ Just to have a twinkling shine :-)
\
\		Ralph Sahli, 2016
\
\		resources used:
\ 			PB3   Green LED
\
\		REQUIRES: lib_registers.txt
\		REQUIRES: lib_systick.txt

: blinky ( -- )
    MODE_OUTPUT 3 PORTB set-moder		\ PB3 -> output mode
	init-Systimer
	cr ." press any key to quit"
	begin
		3 bit PORTB _pODR xor!			\ toggle LED
		1000 delay-ms
		key?
	until
;
