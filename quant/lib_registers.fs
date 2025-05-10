\
\ 	register definitions for STM32L432
\
\		Ralph Sahli, 2016
\

48000000 variable hclk	\ system clock: initial 48 MHz from HSI RC

: registers ( -- )
	0 ;				\ offset start

: reg
    <builds 		( offset -- newoffset )
		dup , cell+
    does>			( structure-base -- structure-member-address )
		@ + ;

: regC
    <builds 		( offset -- newoffset )
		dup , cell+
    does>			( structure-base channel -- structure-member-address )
		@ swap 1- 20 * + + ;

: end-registers ( -- )
	drop ;			\ last offset

\ bit masks
: bit ( n -- n )
	1 swap lshift 1-foldable ;

$E000E100 constant NVIC

$40021000 constant RCC
	registers
		reg _rCR			\ Clock control register
		drop $08
		reg _rCFGR			\ Clock configuration register
		reg _rPLLCFGR		\PLL configuration register
		reg _rPLLSAI1CFGR	\ PLLSAI1 configuration register
		drop $48
		reg _rAHB1ENR		\ AHB1 peripheral clock enable register
		reg _rAHB2ENR		\ AHB3 peripheral clock enable register
		drop $58
		reg _rAPB1ENR1		\ APB11 peripheral clock enable register
		reg _rAPB1ENR2		\ APB12 peripheral clock enable register
		reg _rAPB2ENR		\ APB2 peripheral clock enable register
		drop $88
        reg _rCCIPR         \ Peripheral indipendent clock config
		reg _rBDCR			\ RCC Backup domain control register
		reg _rCSR			\ Control/status register
	end-registers

$40007400 constant DAC
    registers
        reg _daCR
        reg _daSWTRGR  \ software trigger
        reg _daDHR12R1 \ holding
        reg _daDHR12L1
        reg _daDHR8R1
        reg _daDHR12R2
        reg _daDHR12L2
        reg _daDHR8R2
        reg _daDHR12RD
        reg _daDHR12LD
        reg _daDHR8RD
        reg _daDOR1     \ output
        reg _daDOR2
        reg _daSR       \ status
        drop $3C
        reg _daMCR      \ mode
        reg _daSHSR1    \ SH sample time
        reg _daSHSR2
        reg _daSHHR     \ SH hold time
        reg _daSHRR     \ SH refresh time
    end-registers

\ DAC1_OUT PA4 (A3)
\ DAC2_OUT PA5 (A4)

$50040000 constant ADC
    registers
        reg _adc_ISR   \ status
        reg _adc_IER   \ enable
        reg _adc_CR
        drop $0C
        reg _adc_CFGR1
        reg _adc_CFGR2 \ oversampling
        reg _adc_SMPR1 \ sample time
        drop $20
        reg _adc_TR1   \ watchdog threshold
        reg _adc_TR2
        reg _adc_TR3
        drop $30
        reg _adc_SQR1
        reg _adc_SQR2
        reg _adc_SQR3
        drop $3C
        reg _adc_SQR4
        drop $40
        reg _adc_DR
        drop $308
        reg _adc_CCR
    end-registers

$40010030 constant VREFBUF
    registers
        reg _vref_CSR
    end-registers

$48000400 constant PORTB
$48000000 constant PORTA
	registers
		reg _pMODER   	\ Port Mode Register - 00=Input  01=Output  10=Alternate  11=Analog
		reg _pOTYPER  	\ Port Output type register - (0) Push/Pull vs. (1) Open Drain
		reg _pOSPEEDR 	\ Output Speed Register - 00=2 MHz  01=25 MHz  10=50 MHz  11=100 MHz
		reg _pPUPDR		\ Pullup / Pulldown - 00=none  01=Pullup  10=Pulldown
		reg _pIDR		\ Input Data Register
		reg _pODR     	\ Output Data Register
		reg _pBSRR		\ port bit set/reset register
		reg _pLCKR		\ port configuration lock register
		reg _pAFRL		\ Alternate function  low register
		reg _pAFRH		\ Alternate function high register
	end-registers



$40012C00 constant TIM1
$40000000 constant TIM2
$40000400 constant TIM3
$40001000 constant TIM6
$40001400 constant TIM7
$40014000 constant TIM15
$40014400 constant TIM16

	registers
		reg _tCR1		\ TIMx control register 1
		reg _tCR2		\ TIMx control register 2
		reg _tSMCR		\ TIMx slave mode control register
		reg _tDIER		\ TIMx DMA/Interrupt enable register
		reg _tSR		\ TIMx status register
		reg _tEGR		\ TIMx event generation register
		reg _tCCMR1		\ TIMx capture/compare mode register 1
		reg _tCCMR2		\ TIMx capture/compare mode register 2
		reg _tCCER		\ TIMx capture/compare enable register
		reg _tCNT		\ TIMx counter
		reg _tPSC		\ TIMx prescaler
		reg _tARR		\ TIMx auto-reload register
		reg _tRCR		\ TIM16/TIM17 repetition counter register
		reg _tCCR1		\ TIMx capture/compare register 1
		reg _tCCR2		\ TIMx capture/compare register 2
		reg _tCCR3		\ TIMx capture/compare register 3
		reg _tCCR4		\ TIMx capture/compare register 4
		reg _tBDTR		\ TIM16/TIM17 break and dead-time register
		reg _tDCR		\ TIMx DMA control register
		reg _tDMAR		\ TIMx DMA address for full transfer
	end-registers

\ Port Mode Register - 00=Input  01=Output  10=Alternate  11=Analog
%00 constant MODE_Input
%01 constant MODE_Output
%10 constant MODE_Alternate
%11 constant MODE_Analog
: set-moder ( mode pin# baseAddr -- )
	>R 2* %11 over lshift r@ _pMODER bic! 	\ clear ..
	lshift R> _pMODER bis!					\ .. set
;

\ Port Output Speed Register - 00=2 MHz  01=25 MHz  10=50 MHz  11=100 MHz
%00 constant SPEED_LOW
%01 constant SPEED_MEDIUM
%10 constant SPEED_HIGH
%11 constant SPEED_VERYHIGH
: set-opspeed ( speed pin# baseAddr -- )
	>R 2* %11 over lshift r@ _pOSPEEDR bic!	\ clear ..
	lshift R> _pOSPEEDR bis!				\ .. set
;

\ Port alternate function
: set-alternate ( af# pin# baseAddr -- )
	>R dup 8 < if
		4 * lshift R> _pAFRL
	else
		8 - 4 * lshift R> _pAFRH
	then
	bis!
;

\ calculate rounded baudrate
: baud ( n -- reg-val )
	hclk @ over /mod -rot
	swap 2/ >= if 1+ then				\ round
;
