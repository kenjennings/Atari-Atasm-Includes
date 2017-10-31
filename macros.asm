;-------------------------------------------------------------------------------
;	MACROS
;-------------------------------------------------------------------------------
; Helper macros to shorten repeditive tasks and make more readable code
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;                                                                  LOADINT   A
;-------------------------------------------------------------------------------
; mLoadInt <Destination Address>, <Source Address>
;
; Loads the 16-bit value stored at <Source Address> into <Destination Address>.
; 
; Can be used to assign an address to a page 0 location for 
; later indirect addressing.
; In general, copies a 16-bit value to any address.
; Like C = D.
;-------------------------------------------------------------------------------

.macro mLoadInt
	.IF %0<>2
		.ERROR "LoadInt: 2 arguments required."
	.ELSE
		lda %2
		sta %1
		lda %2 + 1
		sta %1 + 1
	.ENDIF
.endm

;-------------------------------------------------------------------------------
;                                                                  LOADINTP  A
;-------------------------------------------------------------------------------
; mLoadIntP <Destination Address>, <Value/Address/Pointer>
;
; Loads the immediate 16-bit <Value/Address/Pointer> into <Destination Address>.
; 
; Can be used to assign an address to a page 0 location for 
; later indirect addressing.
; In general, stores an immediate 16-bit value at any address.
; Like:
;  C = 12  or 
;  C = &D
;-------------------------------------------------------------------------------

.macro mLoadIntP
	.if %0<>2
		.error "LoadIntP: 2 arguments required."
	.else
		lda #<%2
		sta %1
		lda #>%2
		sta %1 + 1
	.endif
.endm

;-------------------------------------------------------------------------------
;                                                                  ALIGN
;-------------------------------------------------------------------------------
; mAlign <Size>
;
; Forces the program address (*=) to the next nearest address 
; aligned to the specified BASE2 size.
; 
; Size may be BASE2 number: 2, 4, 8, 16, 32... up to 16384 (16K).
; 
; Typically would be used to align the current address to a page (256 bytes)
; or to 1K, 2K, 4K in preparation of defining various graphics resources.
;
; Note that if the current location is at correct alignment the 
; normal treatment would have advanced the program counter to the
; next aligned increment.  The adjustment subtracts a byte from 
; the program address and then applies the shift to the next 
; aligned size which is the current location.
;
; Yes, I clearly see the obvious pattern here, but can't work out 
; how to do this in a more clever way.  So, a lot of separate 
; .IF/.ENDIF blocks.
;-------------------------------------------------------------------------------

.macro mAlign
	.if %0<>1
		.error "Align: 1 argument required."
	.else
		MALIGN_TEMP .= 0
		.if %1 = ~0000000000000010 ; 2
			MALIGN_TEMP .= ~1111111111111110
		.endif
		.if %1 = ~0000000000000100 ; 4
			MALIGN_TEMP .= ~1111111111111100
		.endif
		.if %1 = ~0000000000001000 ; 8
			MALIGN_TEMP .= ~1111111111111000
		.endif

		.if %1 = ~0000000000010000 ; 16
			MALIGN_TEMP .= ~1111111111110000
		.endif
		.if %1 = ~0000000000100000 ; 32
			MALIGN_TEMP .= ~1111111111000000
		.endif
		.if %1 = ~0000000001000000 ; 64
			MALIGN_TEMP .= ~1111111110000000
		.endif
		.if %1 = ~0000000010000000 ; 128
			MALIGN_TEMP .= ~1111111100000000
		.endif

		.if %1 = ~0000000100000000 ; 256
			MALIGN_TEMP .= ~1111111000000000
		.endif
		.if %1 = ~0000001000000000 ; 512
			MALIGN_TEMP .= ~1111110000000000
		.endif
		.if %1 = ~0000010000000000 ; 1024
			MALIGN_TEMP .= ~1111100000000000
		.endif
		.if %1 = ~0000100000000000 ; 2048
			MALIGN_TEMP .= ~1111000000000000
		.endif

		.if %1 = ~0001000000000000 ; 4096
			MALIGN_TEMP .= ~1110000000000000
		.endif
		.if %1 = ~0010000000000000 ; 8192
			MALIGN_TEMP .= ~1100000000000000
		.endif
		.if %1 = ~0100000000000000 ; 16384
			MALIGN_TEMP .= ~1000000000000000
		.endif

		.if MALIGN_TEMP
		 	*= [[*-1]&MALIGN_TEMP]+%1 	; Align to start of size
		.else
			.error "Align: %1 argument is not a base 2 value."
		.endif
	.endif
.endm



;-------------------------------------------------------------------------------
;                                                                  SAVEAY
;-------------------------------------------------------------------------------
; mSaveAY 
;
; Save A, Y CPU registers on stack. 
;
; Used on entry into an interrupt, or for preserving
; registers before entering a routine.
;-------------------------------------------------------------------------------

.macro mSaveAY 
	PHA 
	TYA 
	PHA  

.endm 

;-------------------------------------------------------------------------------
;                                                                  SAVEAX
;-------------------------------------------------------------------------------
; mSaveAX
;
; Save A, Y CPU registers on stack. 
;
; Used on entry into an interrupt, or for preserving
; registers before entering a routine.
;-------------------------------------------------------------------------------

.macro mSaveAX
	PHA 
	TXA 
	PHA  

.endm 

;-------------------------------------------------------------------------------
;                                                                  SAVEAYX
;-------------------------------------------------------------------------------
; mSaveAYX 
;
; Save A, Y, X CPU registers on stack. 
;
; Used on entry into an interrupt, or for preserving
; registers before entering a routine.
;-------------------------------------------------------------------------------

.macro mSaveAYX 
	PHA 
	TYA 
	PHA 
	TXA 
	PHA 

.endm 

;-------------------------------------------------------------------------------
;                                                                  RESTOREAY
;-------------------------------------------------------------------------------
; mRestoreAY
;
; Restore A, Y CPU registers from stack. 
;
; Used on exiting an interrupt, or for restoring 
; registers before ending a routine.
;-------------------------------------------------------------------------------

.macro mRestoreAY  
	PLA 
	TAY 
	PLA 

.endm 

;-------------------------------------------------------------------------------
;                                                                  RESTOREAX
;-------------------------------------------------------------------------------
; mRestoreAX
;
; Restore A, X CPU registers from stack. 
;
; Used on exiting an interrupt, or for restoring 
; registers before ending a routine.
;-------------------------------------------------------------------------------

.macro mRestoreAX 
	PLA 
	TAX 
	PLA 

.endm 

;-------------------------------------------------------------------------------
;                                                                  RESTOREAYX
;-------------------------------------------------------------------------------
; mRestoreAYX 
;
; Restore A, Y, X CPU registers from stack. 
;
; Used on exiting an interrupt, or for restoring 
; registers before ending a routine.
;-------------------------------------------------------------------------------

.macro mRestoreAYX 
	PLA 
	TAX 
	PLA 
	TAY 
	PLA 

.endm 

;-------------------------------------------------------------------------------
;                                                                  SAVEREGS
;-------------------------------------------------------------------------------
; usage :
; mSaveRegs 
;
; Saves the CPU registers so subroutines do not disturb the 
; register states and logic/flow of the main code.
;-------------------------------------------------------------------------------

.macro mSaveRegs  
	PHP 
	mSaveAYX
.endm 

;-------------------------------------------------------------------------------
;                                                                  SAFERTS
;-------------------------------------------------------------------------------
; mSafeRTS 
;
; Restores CPU registers for safe return from a routine 
; that used saveRegs to preserve the CPU registers.
;
; Includes the RTS.
;-------------------------------------------------------------------------------

.macro mSafeRTS  
	mRestoreAYX
	PLP 
	
	RTS 
.endm 





;===============================================================================
; ****   ******   **    *****
; ** **    **    ****  **  
; **  **   **   **  ** **
; **  **   **   **  ** ** ***
; ** **    **   ****** **  **
; ****   ****** **  **  *****
;===============================================================================

;-------------------------------------------------------------------------------
;                                                               DEBUGBYTE    A Y
;-------------------------------------------------------------------------------
; mDebugByte <Address>, <X position>
;
; Calls the DiagByte routine to convert a byte into the two-byte, 
; hex representation and write this to a position in the 
; diagnostic screen memory intended for display on the screen. 
;-------------------------------------------------------------------------------

	.macro mDebugByte  ; Address, position offset
		.if %0<>2
			.error "DebugByte: 2 arguments required."
		.else
			lda %1   ; Load byte in address
			ldy #%2  ; Load screen line X offset.
			jsr DiagByte
		.endif
	.endm

