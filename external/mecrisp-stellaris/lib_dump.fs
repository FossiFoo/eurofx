( DUMP Library)

\ helper words -----------------------------------------------------------
: .PRINTCHAR DUP $20 < OVER $7E > OR IF DROP $2E THEN EMIT ;
: .PRINTFLAG IF $31 ELSE $2E THEN EMIT 2 SPACES ;

: 2# 0 <# # # #> TYPE ;
: 4# 0 <# # # # # #> TYPE ;
: 8# 0 <# # # # # # # # # #> TYPE ;

: .ADDRHEAD CR ." Addr       " ;				\ header address
: .BHEAD $10 0 DO SPACE I . LOOP ;				\ header byte dump
: .HHEAD $10 0 DO 3 SPACES I . 2 +LOOP ; 		\ header halfword dump
: .WHEAD $10 0 DO 7 SPACES I . 4 +LOOP ;		\ header word dump
: .AHEAD 4 SPACES ." 0123456789ABCDEF" ;		\ header ascii values

: .ADDRESS CR DUP 8# SPACE DUP ;  								\ printing address
: .BVAL 2 SPACES $10 0 DO DUP C@ 2# SPACE 1+ LOOP DROP ;		\ printing byte values in hex
: .HVAL 2 SPACES 8 0 DO DUP H@ 4# SPACE 2+ LOOP DROP ;			\ printing halfword values in hex
: .WVAL 2 SPACES 4 0 DO DUP @ 8# SPACE 4 + LOOP DROP ;			\ printing word values in hex
: .AVAL	4 SPACES $10 0 DO DUP C@ .PRINTCHAR 1+ LOOP ;			\ printing only printable ascii values
: .bitVAL $20 0 DO 1 $1F I - lshift OVER bit@ .PRINTFLAG LOOP 3 SPACES @ 8# ; \ printing word value in binary
\ end helper words ---------------------------------------------------


\ byte dump
: b-dump ( c-addr cnt -- )
	BASE @ >R HEX						\ save base
	.ADDRHEAD .BHEAD .AHEAD		\ print header
	$10 / 0 DO
		.ADDRESS .BVAL .AVAL			\ print byte values
	LOOP
	R> BASE ! DROP ;						\ restore base

\ halfword dump
: h-dump ( a-addr cnt -- )
	BASE @ >R HEX
	.ADDRHEAD .HHEAD .AHEAD
	$10 / 0 DO
		.ADDRESS .HVAL .AVAL
	LOOP
	R> BASE ! DROP ;

\ word dump
: w-dump ( a-addr cnt -- )
	BASE @ >R HEX
	.ADDRHEAD .WHEAD .AHEAD
	$10 / 0 DO
		.ADDRESS .WVAL .AVAL
	LOOP
	R> BASE ! DROP ;

\ register dump
: r-dump  ( a-addr cnt -- )
	cr cr over hex. ." :"
	BASE @ >R DECIMAL
	CR ." Offset " $20 0 DO $1F I - 2# SPACE LOOP 4 SPACES ." value"
	HEX
	4 / 0 DO
		CR 2 SPACES DUP $FF AND 2# 4 SPACES DUP .bitVAL 4 +
	LOOP
	R> BASE ! DROP ;


\ beispiel register dump
\ PORTD_Base $40 r-dump
