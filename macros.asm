;===============================================================================
;	MACROS
;===============================================================================
; Helper macros to shorten repetitive tasks and make more readable code
;
;===============================================================================
; 16-BIT DATA -  Store/copy 16-bit values
;===============================================================================
; mStoreWordA (Absolute Address)
; mStoreWordI (Immediate)
;
;===============================================================================
; MEMORY
;===============================================================================
; mAlign
;
;===============================================================================
; DISK SHENANIGANS
;===============================================================================
; mDiskPoke (Byte)
; mDiskDPoke (Word)
;
;===============================================================================
; 6502 REGISTER MAINTENANCE
;===============================================================================
; mSaveAY 
; mSaveAX
; mSaveAYX 
; mRestoreAY
; mRestoreAX
; mRestoreAYX 
; mSaveRegs 
; mSafeRTS
;
;===============================================================================
; INTERRUPTS
;===============================================================================
; mDLIChain 
;
;===============================================================================
; DIAG
;===============================================================================
; mDebugByte 
;
;===============================================================================


;===============================================================================
; 16-BIT DATA 
;===============================================================================
; Store/Copy 16-bit values
;===============================================================================

;-------------------------------------------------------------------------------
;                                                                  STOREWORDA   A
;-------------------------------------------------------------------------------
; mStoreWordA <Destination Address>, <Source Address>
;
; Stores the 16-bit value at <Source Address> into <Destination Address>. 
; 
; Like (in C):  C = D.
; 
; Or more like:  *C = *D
;
; Or in BASIC XL:  Dpoke C, Dpeek(D)
;-------------------------------------------------------------------------------

.macro mStoreWordA
	.IF %0<>2
		.ERROR "mStoreWordA: 2 arguments (dest addr, source addr) required."
	.ELSE
		lda %2
		sta %1
		lda %2 + 1
		sta %1 + 1
	.ENDIF
.endm

; Old synonym
;
.macro mLoadInt
	mStoreWordA %1, %2
.endm

;-------------------------------------------------------------------------------
;                                                                  STOREWORDI  A
;-------------------------------------------------------------------------------
; mStoreWordI <Destination Address>, <Value>
;
; Loads the immediate 16-bit <Value> into the <Destination Address>.
;
; (Or should this be Store
;
; Like (in C):
;  C = 12  or 
;  C = &D
;
; In BASIC XL:  Dpoke C, D
;-------------------------------------------------------------------------------

.macro mStoreWordI
	.if %0<>2
		.error "mStoreWordI: 2 arguments (dest addr, 16-bit value) required."
	.else
		lda #<%2
		sta %1
		lda #>%2
		sta %1 + 1
	.endif
.endm

; Old synonym
;
.macro mLoadIntP
	mStoreWordI %1, %2
.endm

;===============================================================================
; MEMORY
;===============================================================================
; Assembler Tricks...
; 
; Force program address to next address aligned to a border in memory.
;===============================================================================

;-------------------------------------------------------------------------------
;                                                                  ALIGN
;-------------------------------------------------------------------------------
; mAlign <Size>
;
; Forces the program address (*=) to the next nearest address 
; aligned to the specified BASE2 size.
; 
; Size must be BASE2 number: 2, 4, 8, 16, 32... up to 16384 (16K).
; 
; Typically would be used to align the current address to a page 
; (256 bytes) or to 1K, 2K, 4K in preparation for declaring space 
; for various graphics resources.
;
; NOTE!!  If the current location is at the correct alignment the 
; macro does NOT move to the next aligned location. 
; The normal treatment would have advanced the program counter to the
; next aligned increment.  This would create potentially unintended, 
; unused "holes" in the memory map.    
; The macro works by subtracting a byte from the program address and 
; then applies the shift to the next aligned size which then returns 
; to the current location.
;
; Yes, I clearly see the obvious pattern below, but can't work out 
; how to do this in a more clever way.  So, a lot of separate 
; .IF/.ENDIF blocks.
; The input must be limited to a base 2 value, and the mask is 
; shifted per each value.  
;-------------------------------------------------------------------------------

.macro mAlign
	.if %0<>1
		.error "Align: 1 argument (base 2 size) required."
	.else
		MALIGN_TEMP .= 0
		.if %1=~0000000000000010 ; 2
			MALIGN_TEMP .= ~1111111111111110
		.endif
		.if %1=~0000000000000100 ; 4
			MALIGN_TEMP .= ~1111111111111100
		.endif
		.if %1=~0000000000001000 ; 8
			MALIGN_TEMP .= ~1111111111111000
		.endif

		.if %1=~0000000000010000 ; 16
			MALIGN_TEMP .= ~1111111111110000
		.endif
		.if %1=~0000000000100000 ; 32
			MALIGN_TEMP .= ~1111111111100000
		.endif
		.if %1=~0000000001000000 ; 64
			MALIGN_TEMP .= ~1111111111000000
		.endif
		.if %1=~0000000010000000 ; 128
			MALIGN_TEMP .= ~1111111110000000
		.endif

		.if %1=~0000000100000000 ; 256
			MALIGN_TEMP .= ~1111111100000000
		.endif
		.if %1=~0000001000000000 ; 512
			MALIGN_TEMP .= ~1111111000000000
		.endif
		.if %1=~0000010000000000 ; 1024
			MALIGN_TEMP .= ~1111110000000000
		.endif
		.if %1=~0000100000000000 ; 2048
			MALIGN_TEMP .= ~1111100000000000
		.endif

		.if %1=~0001000000000000 ; 4096
			MALIGN_TEMP .= ~1111000000000000
		.endif
		.if %1=~0010000000000000 ; 8192
			MALIGN_TEMP .= ~1110000000000000
		.endif
		.if %1=~0100000000000000 ; 16384
			MALIGN_TEMP .= ~1100000000000000
		.endif
		
		; Really?  More?  Do you know how many 32K boundaries are in 64K?

		.if MALIGN_TEMP>0
		 	*= [[*-1]&MALIGN_TEMP]+%1 	; Align to start of (next) size
		.else
			.error "Align: Argument for size is not a base 2 value."
		.endif
	.endif
.endm


;===============================================================================
; DISK SHENANIGANS
;===============================================================================
; The Atari executable file is a structured format.  The file contents identify
; starting address, ending address, and the data to load.  This feature 
; ordinarily allows the assembler to optimize the file size by describing only
; the segments of memory needed for the program.  However, it can also be
; abused to set values into any memory location during the program load time,
; such as the operating system shadow registers.  This allows the act of 
; loading the program to also perform a degree of initialization that applies
; configuration to the system without the program expending its own code 
; space to load and store values.
;
; The assembler supports this simply by changing the program address *=
; and then declaring storage (.byte, etc.)  These macros capture the 
; current program address in a temporary variable, set the current
; address,  declare the supplied value, then restore the program 
; address to the originally captured value.
;
; Effective use of the disk load feature allows presentation of title screens, 
; animation, music, etc. during the time the main program is loading.
;
; Mac/65 keeps program location changes in the order in which they occur
; in the assembly source. But, atasm collects, sorts, and optimizes the 
; program location changes. 
; 
; If the atasm behavior is undesirable, then there are two solutions:
; * Use .bank which prevents declarations after the .bank from being mixed 
;   with declarations before the .bank.
; * Build separately, and then use the DOS Append function to merge the 
;   separately generated executable files. 
; 

;
;===============================================================================

;-------------------------------------------------------------------------------
;                                                                  DiskPoke
;-------------------------------------------------------------------------------
; mDiskPoke <Address> <byte value>
;
; Utilize the Atari's structured disk format to load a BYTE value into a memory
; location at the program load time.
;-------------------------------------------------------------------------------

.macro mDiskPoke
	.if %0<>2
		.error "DiskPoke: 2 arguments (dest addr, byte value) required."
	.else
		.if %2>$FF
			.error "DiskPoke: Agument 2 for byte value is greater then $FF"
		.else
			DISKPOKE_TEMP .= *
			*=%1
			.byte %2
			*=DISKPOKE_TEMP
		.endif
	.endif
.endm 

;-------------------------------------------------------------------------------
;                                                                  DiskDPoke
;-------------------------------------------------------------------------------
; mDiskDPoke <Address> <16-bit value>
;
; Utilize the Atari's structured disk format to load a 16-bit WORD value into a 
; memory location at the program load time.
;-------------------------------------------------------------------------------

.macro mDiskDPoke
	.if %0<>2
		.error "DiskDPoke: 2 arguments (dest addr, integer value) required."
	.else
		DISKDPOKE_TEMP .= *
		*=%1
		.word %2
		*=DISKDPOKE_TEMP
	.endif
.endm 


;===============================================================================
; 6502 REGISTER MAINTENANCE
;===============================================================================
; Various shortcuts for managing 6502 A, X, Y registers typically used 
; when entering/exiting interrupts.  
;
; Also, a couple routines for entry/exit from a routine called by JSR to 
; preserve the registers and CPU flags, so the routine does not affect
; the caller.
;===============================================================================

;-------------------------------------------------------------------------------
;                                                                  SAVEAY A Y
;-------------------------------------------------------------------------------
; mSaveAY 
;
; Save A, Y CPU registers on stack. 
;-------------------------------------------------------------------------------

.macro mSaveAY 
	PHA 
	TYA 
	PHA  

.endm 

;-------------------------------------------------------------------------------
;                                                                  SAVEAX A X
;-------------------------------------------------------------------------------
; mSaveAX
;
; Save A, Y CPU registers on stack. 
;-------------------------------------------------------------------------------

.macro mSaveAX
	PHA 
	TXA 
	PHA  

.endm 

;-------------------------------------------------------------------------------
;                                                                  SAVEAYX A Y X
;-------------------------------------------------------------------------------
; mSaveAYX 
;
; Save A, Y, X CPU registers on stack. 
;-------------------------------------------------------------------------------

.macro mSaveAYX 
	PHA 
	TYA 
	PHA 
	TXA 
	PHA 

.endm 

;-------------------------------------------------------------------------------
;                                                                  RESTOREAY A Y
;-------------------------------------------------------------------------------
; mRestoreAY
;
; Restore A, Y CPU registers from stack. 
;-------------------------------------------------------------------------------

.macro mRestoreAY  
	PLA 
	TAY 
	PLA 

.endm 

;-------------------------------------------------------------------------------
;                                                                  RESTOREAX A X
;-------------------------------------------------------------------------------
; mRestoreAX
;
; Restore A, X CPU registers from stack. 
;-------------------------------------------------------------------------------

.macro mRestoreAX 
	PLA 
	TAX 
	PLA 

.endm 

;-------------------------------------------------------------------------------
;                                                               RESTOREAYX A X Y
;-------------------------------------------------------------------------------
; mRestoreAYX 
;
; Restore A, Y, X CPU registers from stack. 
;-------------------------------------------------------------------------------

.macro mRestoreAYX 
	PLA 
	TAX 
	PLA 
	TAY 
	PLA 

.endm 

;-------------------------------------------------------------------------------
;                                                               SAVEREGS A X Y P
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
;                                                                SAFERTS A X Y P
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
; INTERRUPTS
;===============================================================================

;-------------------------------------------------------------------------------
;                                                               DLICHAIN A 
;-------------------------------------------------------------------------------
; mDLIChain <address from current DLI> <address to next DLI>
;
; Sets new VDSLST vector for the next Display List Interrupt.
;
; If the high byte is the same do not change it.
;
;-------------------------------------------------------------------------------

.macro mDLIChain
	lda #<%2
	sta VDSLST
	.if [>%1]=[>%2]
		lda #>%2
		sta VDSLST + 1
	.endif
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
		.error "DebugByte: 2 arguments (address, screen X position) required."
	.else
		lda %1   ; Load byte in address
		ldy #%2  ; Load screen line X offset.
		jsr DiagByte
	.endif
.endm
