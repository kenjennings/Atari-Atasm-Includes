;===============================================================================
;                                                       CBM PRG STUDIO MACROS
;===============================================================================
;                                                       - Peter 'Sig' Hewett
;                                                                       2016
;; Atari-fied for eclipse/wudsn/atasm by Ken Jennings
;; FYI -- comments with double ;; are Ken's for Atari
;-------------------------------------------------------------------------------
;  Helper macros to shorten repeditive tasks and make more readable code
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;                                                                  LOADPOINTER
;-------------------------------------------------------------------------------
; usage :
; loadpointer <zeropage_pointer>, <label>
;
; loads the address of <label> into <zeropage_pointer>
; NOTE : the lable MUST be an absolute address
;-------------------------------------------------------------------------------

;;defm loadPointer
;;        lda #</2
;;        sta /1
;;        lda #>/2
;;        sta /1 + 1

;;        endm

;; Atari -- atasm version.

	.MACRO loadPointer
		.IF %0<>2
			.ERROR "loadPopinter: incorrect number of arguments"
		.ELSE
			lda #<%2
			sta %1
			lda #>%2
			sta %1 + 1
		.ENDIF
	.ENDM
	
;; Atari -- atasm -- save regs so Atari-specific routines don't 
;; disturb the logic/flow of the main code.

	.MACRO saveRegs ;; macro 
		PHP 
		PHA 
		TYA 
		PHA 
		TXA 
		PHA 
	.ENDM 

;; Atari -- atasm -- safe return from a routine that used saveRegs

	.MACRO safeRTS ;; macro 
		PLA 
		TAX 
		PLA 
		TAY 
		PLA 
		PLP 
		RTS 
	.ENDM 

;--------------------------------------------------------------------------------

