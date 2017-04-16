;; OS memory and vectors
;; For atasm
;; Ken Jennings
;;=================================================
;; Use the ANTIC/GTIA/POKEY files for shadow
;; register declarations.
;;=================================================
;; Most of the first half of Page Zero is claimed
;; by the OS.  The second half is for the cartridge
;; ROM, and the rest is for the Floating point 
;; package.
;;
;; If a cartridge is not inserted then the Page Zero
;; space for the cartridge is available for any 
;; machine language program.   
;;
;; Likewise, if the floating-point package will not 
;; be used then the FP registers and working area 
;; can be treated as free for use by a machine 
;; language program.
;;=================================================

;;=================================================
;; OS Page 0
;;=================================================
LINZBS = $00 ; word

CASINI = $02 ; word, Cassette initialization vector
RAMLO =  $04 ; word, power up memory test.  Disk boot address.

CTAFLG = $06 ; nonzero value means Left/A cartridge present
CTBFLG = $07 ; nonzero value means Right/B  cartridge present

WARMST = $08 ; Warmstart flag. 0 = powerup in progress. $FF normal reset occurred.
BOOT =   $09 ; Boot status. 0 = no boot.  
    ;; 1 = disk boot OK.  do reset via DOSVEC
    ;; 2 = cassette boot OK. do reset via CASINI

DOSVEC = $0A ; word. Entry vector for DOS (actually to start DUP.SYS).
DOSINI = $0C ; word. Init address for DOS or Cassette RUN address.

APPMHI = $0E ; word. Application high memory in use.

;; POKMSK = $10 in POKEY.asm

BRKKEY = $11 ; 0 = Break pressed.  

;; Real Time Clock incremented during the vertical blank.
;; This is three addresses $12, $13, and $14.
;; The value of $14 is incremented  every vertical blank.
;; When the value of $14 reaches $FF on the next increment it rolls 
;; over to $00 and then the value of $13 increments.
;; $14 rollover/$13 increment occurs approximately every 4.27 seconds.
;; Likewise, when the value if $13 reaches $FF and it rolls 
;; over to $00,then the value of $12 increments.
;; $13 rollover/$12 increment occurs approximately every 18.2 minutes.
RTCLOK =   $12 ; and $13, and $14.  
RTCLOK60 = $14 ; incremented every jiffy/frame.

BUFADR = $15 ; word.  temporary address of disk buffer
ICCOMT = $17 ; CIO command.
DSKFMS = $18 ; word. File Management System vector.
DSKUTL = $1A ; word. Disk Utilities pointer.

PTIMOT = $1C ; printer timeout.  approx 64 second per 60 values.
PBPNT =  $1D ; Printer buffer pointer.  index into buffer.
PBUFSZ = $1E ; Printer buffer size.
PTEMP =  $1F ; Temporary printer value used by print handler.

;; Zero Page copy of CIO's IOCB
ICHIDZ = $20 ; Handler Index
ICDNOZ = $21 ; Device or drive number
ICCOMZ = $22 ; Command
ICSTAZ = $23 ; IOCB status result
ICBALZ = $24 ; Buffer address (lo byte)
ICBAHZ = $25 ; Buffer address (hi byte)
ICPTLZ = $26 ; Put Byte rouotine address (lo byte)
ICPTHZ = $27 ; Put Byte rouotine address (hi byte)
ICBLLZ = $28 ; Buffer length (lo byte)
ICBLHZ = $29 ; Buffer length (hi byte)
ICAX1Z = $2A ; Aux byte 1 (open parameters)
ICAX2Z = $2B ; Aux byte 2
ICAX3Z = $2C ; Aux byte 3 (BASIC Note/Point)
ICAX4Z = $2D ; Aux byte 4 (BASIC Note/Point)
ICAX5Z = $2E ; Aux byte 5
ICAX6Z = $2F ; Aux byte 6

STATUS = $30 ; SIO status
CHKSUM = $31 ; SIO data frame checksum.

BUFRLO = $32 ; SIO and DCB address of data to send or receive (lo byte)
BUFRHI = $33 ; SIO and DCB address of data to send or receive (hi byte)
BFENLO = $34 ; SIO and DCB address after BUFRLO/BUFRHI  (lo byte)
BFENHI = $35 ; SIO and DCB address after BUFRLO/BUFRHI  (hi byte)

CRETRY = $36 ; Command frame retries.  Usually $0D.
DRETRY = $37 ; Device retries.  Usually $01.

BUFRFL = $38 ; Flag buffer full. $FF is full.
RECVDN = $39 ; Flag receive done. $FF is done.
XMTDON = $3A ; Flag transmit done. $FF is done.
CHKSNT = $3B ; Flag checksum sent. $FF is sent.
NOCKSM = $3C ; Flag $00 = checksum follows data.  not zero = no checksum.

BPTR =   $3D ; Index to data in cassette buffer. 
FTYPE =  $3E ; Gap between cassette blocks. $01 to $7F = normal. $80 to $00 = short gaps.
FEOF =   $3F ; EOF for cassette. $00 = No EOF.  !$00 = EOF detected.
FREQ =   $40 ; Number of beeps for cassette start.  1 = Play.  2 = Record.
SOUNDR = $41 ; Play I/O sounds to speaker. 0 = silence.  !0 = I/O sound.

;; Critical I/O flag.  
;; Set to stop some automated timers and updates.
;;  $00 = Normal behavior. 
;; !$00 = Critical I/O mode.
;; When CRITIC is set (non-zero) the following activities change:
;; Stage 2/Deferred Vertical Blank Interrupt STOPS.
;; (Stage 1/Immediate Vertical Blank Interrupt continues.)
;; Software Timers 2, 3, 4, and 5 stop.
;; Keyboard repeat disabled.
CRITIC = $42 ;  

FMZSPG = $43 ;; 7 bytes up to $49. Disk FMS page 0 temporary registers (below)
ZBUFP =  $43 ; word.  Pointer to filename.
ZDRVA =  $45 ; word. Drive pointer/sector temporary value.
ZSBA =   $47 ; word. temporary sector pointer.
ERRNO =  $49 ; Disk I/O error.  FMS initializes to $9F.

CKEY =   $4A ; Cassette Cold Start to boot cassette.  Set by holding START key.
CASSBT = $4B ; Flag Cassette Boot. 0 = cassette boot unsuccessful.

DSTAT =  $4C ; status from S: handler. 

;; Atari's "Attract" mode.
;; After no keyboard input for several minutes the Atari OS cycles the 
;; colors to prevent CRT image burn-in.  Reset this to 0 periodically
;; to prevent the OS engaging the attract mode.
ATRACT = $4D

;; Dark attract mask. Set to $FE/254 when attract mode is not active.
;; Set to $F6/246 when attract mode is active.  This masks the 
;; color lunminance bits to make screen colors stay below 50% 
;; brighness. 
DRKMSK = $4E

;; Color shift mask When attract mode is on the color registers are
;; exclusive-OR's with the values in $4e and $4f  during the OS's 
;; stage two vertical blank interrupt.  (see RTCLOK)
;; When set to zero and value of DRKMSK is $f6/246, the luminance 
;; is reduced 50%. COLRSH contains the value of RTCLOK+1 which is 
;; incremented approximately each 4.27 seconds causing the colors
;; to cycle at that period of time.       
COLRSH = $4F

TEMP =   $50 ; S: temporary value. (write character to screen)
HOLD1 =  $51 ; S: temporary value. (lines for Display List)

LMARGN = $52 ; E: left margin of GR.0 text mode and text windows
RMARGN = $53 ; E: right margin of GR.0 text mode and text windows
ROWCRS = $54 ; S: current cursor row (Y) 
COLCRS = $55 ; word.  S: current cursor column (X)

DINDEX = $57 ; S: current screen text/graphics mode
SAVMSC = $58 ; word. Address of first byte of screen memory.

OLDROW = $5A ; Previous cursor row from $54. Used for Drawto and Fill
OLDCOL = $5B ; word. Previous cursor column from $55/$56. Used for Drawto and Fill
OLDCHR = $5D ; Prior value of character at cursor
OLDADR = $5E ; word. memory location of cursor.

NEWROW = $60 ; Destination row for Drawto and Fill.
NEWCOL = $62 ; word. Destination column for Drawto and Fill.
LOGCOL = $64 ; Logical line cursor column.
ADRESS = $65 ; word. S: Temp address for Display List, copy of SAVMSC, etc.

MLTTMP = $66 ; word. Temp value for S: and in OPEN
SAVADR = $68 ; word. S: temporary value. 

RAMTOP = $6A ; First page after end of usable memory.

BUFCNT = $6B ; E: temp logical line size.
BUFSTR = $6C ; word. E: temp value

BITMSK = $6E ; S: bit mapping value
SHFAMT = $6F ; S: pixel shift amount per graphics mode
ROWAC =  $70 ; word. S: temporary row value
COLAC =  $72 ; word. S: temporary column value

ENDPT =  $74 ; word.  S: end point for Drawto.  Copy of DELTAR or DELTAC

DELTAR = $76 ; S: ABS( NEWROW - ROWCRS )
DELTAC = $77 ; word.  S: ABS( NEWCOL - COLCRS )
ROWINC = $79 ; S: Row +1 or -1
COLINC = $7A ; S: Column +1 or -1

SWPFLG = $7B ; S: text window swap control. 0 = graphics. $FF = text window.
HOLDCH = $7C ; S: byte value for shifting.
INSDAT = $7D ; S: temporary character value
COUNTR = $7E ; word. S: Loop control for line drawing. Value of DELTAR or DELTAC.

;;=================================================
;; Cartridge-specific Page 0  $7F to $D1
;;=================================================

;;=================================================
;; Atari BASIC/OSS BASIC XL values
;;=================================================
LOMEM  = $80 ; word. BASIC start of memory.

VNTP   = $82 ; word. BASIC Variable Name Table Pointer.
VNTD   = $84 ; word. BASIC Variable Name Table End address (Dummy) 
VVTP   = $86 ; word. BASIC Variable Value Table Pointer.

STMTAB = $88 ; word. BASIC Start of Statements/user's BASIC program.
STMCUR = $8A ; word. BASIC pointer to current statement.

STARP  = $8C ; word. BASIC String and Array table pointer.
RUNSTK = $8E ; word. BASIC Pointer to GOSUB/FOR-NEXT stack.

MEMTP =  $90 ; word. BASIC pointer to end of user BASIC program.

STOPLN = $92 ; word. BASIC line number where execution stopped due to Break key or error.

ERSAVE = $C3 ; BASIC error code for stop or TRAP.

PTABW =  $C9 ; BASIC tab width - number of columns between tab stops.

;;=================================================
;; OS Floating Point Library
;;=================================================
FR0 =    $D4 ; float.  Floating point register and USR return value to BASIC.
FRE =    $DA ; float.  Floating point register (extra).
FR1 =    $E0 ; float.  Floating point register 1.
FR2 =    $E6 ; float.  Floating point register 2.
FRX =    $EC ; Floating Point spare value
EEXP =   $ED ; Floating Point Exponent
NSIGN =  $EE ; Floating Point Sign.
ESIGN =  $EF ; Floating Point Sign of exponent.
FCHRFL = $F0 ; Flag for first character
DIGRT =  $F1 ; Digits to the right of the decimal.
CIX =    $F2 ; current character input index. Offset into INBUFF
INBUFF = $F3 ; word. input for text to BCD conversion.  output at LBUFF
ZTEMP1 = $F5 ; word. Floating point temporary register.
ZTEMP4 = $F7 ; word. Floating point temporary register.
ZTEMP3 = $F9 ; word. Floating point temporary register.
RADFLG = $FB ; or DEGFLG.  0 = radians.  6 = degrees.
FLPTR =  $FC ; word. Pointer to first Floating Point number for operation..
FPTR2 =  $FE ; word. Pointer to Floating Point number for operation.


;;=================================================
;; OS Page 2 
;;=================================================
VDSLST = $0200 ; word. Display List interrupt address.

VPRCED = $0202 ; word. Peripheral proceed line vector.
VINTER = $0204 ; word. Peripheral interrupt vector.
VBREAK = $0206 ; word. BRK instruction vector.

VKEYBD = $0208 ; word. POKEY keyboard interrupt vector.
VSERIN = $020A ; word. POKEY serial I/O receive data ready interrupt vector
VSEROR = $020C ; word. POKEY serial I/O transmit data ready interrupt vector
VSEROC = $020E ; word. POKEY serial bus transmit complete interrupt vector.

VTIMR1 = $0210 ; word. POKEY timer 1 interrupt vector.
VTIMR2 = $0212 ; word. POKEY timer 2 interrupt vector.
VTIMR4 = $0214 ; word. POKEY timer 4 interrupt vector.

VIMIRQ = $0216 ; word. IRQ immediate vector.


;;=================================================
;; COUNTDOWN TIMERS
;;===============================================================
;;  TIMER    | CDTMV1  | CDTMV2  | CDTMV3   | CDTMV4  | CDTMV5  |
;;---------------------------------------------------------------
;; Decrement | stage 1 | stage 2 | stage 2  | stage 2 | stage 2 |
;; in VBI?   |         |         |          |         |         |
;;---------------------------------------------------------------
;; Interrupt | CDTMA1  | CDTMA2  |          |         |         |
;; Vector?   |         |         |          |         |         |
;;---------------------------------------------------------------
;; Countdown |         |         | CDTMF3   | CDTMF4  | CDTMF5  |
;; Flag?     |         |         |          |         |         |
;;---------------------------------------------------------------
;; OS use?   | I/O     |  no     | cassette |  no     |  no     |
;;           | timing  |         | I/O      |         |         |
;;===============================================================
CDTMV1 = $0218 ; word. Countdown Timer Value 1.
CDTMV2 = $021A ; word. Countdown Timer Value 2.
CDTMV3 = $021C ; word. Countdown Timer Value 3.
CDTMV4 = $021E ; word. Countdown Timer Value 4.
CDTMV5 = $0220 ; word. Countdown Timer Value 5.

VVBLKI = $0222 ; word. VBLANK immediate interrupt vector. 
VVBLKD = $0224 ; word. VBLANK deferred interrupt vector.

CDTMA1 = $0226 ; word. System Timer 1 vector address.
CDTMA2 = $0228 ; word. System Timer 2 vector address.
CDTMF3 = $022A ; Set when CDTMV3 counts down to 0.
SRTIMR = $022B ; keyboard software repeat timer.
CDTMF4 = $022C ; Set when CDTMV4 counts down to 0.
INTEMP = $022D ; Temp value used by SETVBL.
CDTMF5 = $022E ; Set when CDTMV5 counts down to 0.

;; SDMCTL = $022F in ANTIC.asm
;; SDLSTL = $0230 in ANTIC.asm
;; SSKCTL = $0232 in POKEY.asm
;; LPENH  = $0234 in ANTIC.asm
;; LPENV  = $0235 in ANTIC.asm

BRKKY =  $0236 ; Break key interrupt vector

;; SIO Command Frame:
CDEVIC = $023A ; SIO Bus ID number
CCOMND = $023B ; SIO Bus command code
CAUX1 =  $023C ; Command auxiliary byte 1
CAUX2 =  $023D ; Command auxiliary byte 2

TMPSIO = $023E ; SIO temporary byte
ERRFLG = $023F ; SIO error flag (except timeout)
DFLAGS = $0240 ; Disk flags from first byte of boot sector.
DBSECT = $0241 ; Number of Boot sectors read.
BOOTAD = $0242 ; word. Address of the boot loader.

COLDST = $0244 ; Coldstart Flag. 0 = reset is warmstart.  1 = reset is coldstart.

DSKTIM = $0246 ; Disk I/O timeout countdown.

LINBUF = $0247 ; 40 characters. temporary buffer for screen data.

;; GPRIOR = $026F in GTIA.asm
;; PADDL0 = $0270 in POKEY.asm
;; PADDL1 = $0271 in POKEY.asm
;; PADDL2 = $0272 in POKEY.asm
;; PADDL3 = $0273 in POKEY.asm
;; PADDL4 = $0274 in POKEY.asm
;; PADDL5 = $0275 in POKEY.asm
;; PADDL6 = $0276 in POKEY.asm
;; PADDL7 = $0277 in POKEY.asm
;; STICK0 = $0278 in POKEY.asm
;; STICK1 = $0279 in POKEY.asm
;; STICK2 = $027A in POKEY.asm
;; STICK3 = $027B in POKEY.asm
;; PTRIG0 = $027C in POKEY.asm
;; PTRIG1 = $027D in POKEY.asm
;; PTRIG2 = $027E in POKEY.asm
;; PTRIG3 = $027F in POKEY.asm
;; PTRIG4 = $0280 in POKEY.asm
;; PTRIG5 = $0281 in POKEY.asm
;; PTRIG6 = $0282 in POKEY.asm
;; PTRIG7 = $0283 in POKEY.asm
;; STRIG0 = $0284 in POKEY.asm
;; STRIG1 = $0285 in POKEY.asm
;; STRIG2 = $0286 in POKEY.asm
;; STRIG3 = $0287 in POKEY.asm

CSTAT =  $0288 ; Cassette status register.
WMODE =  $0289 ; Cassette Write mode.  0 = read. $80 = write
BLIM =   $028A ; Cassette Buffer Limit. character count in buffer: 0 to $80.

TXTROW = $0290 ; E: text window cursor row.
TXTCOL = $0291 ; word. E: text window cursor column.
TINDEX = $0293 ; Split-screen text window graphics mode.  



;;=================================================
;; OS Page 3
;;=================================================
;; CIO Block.  ** denotes commonly used fields **
IOCB =  $0340   ; Base IO Control Block
ICHID = IOCB+$00 ; Handler ID
ICDNO = IOCB+$01 ; Device number
ICCMD = IOCB+$02 ; ** CIO Command **
ICSTA = IOCB+$03 ; CIO Status
ICBAL = IOCB+$04 ; ** Buffer address (low) **
ICBAH = IOCB+$05 ; ** Buffer address (high) **
ICPTL = IOCB+$06 ; Put char routine (low)
ICPTH = IOCB+$07 ; Put char routine (high)
ICBLL = IOCB+$08 ; ** Buffer length (low) **
ICBLH = IOCB+$09 ; ** Buffer length (high) **
ICAX1 = IOCB+$0A ; ** Aux Byte 1 **
ICAX2 = IOCB+$0B ; ** Aux Byte 2 **
ICAX3 = IOCB+$0C ; Aux Byte 3  
ICAX4 = IOCB+$0D ; Aux Byte 4  
ICAX5 = IOCB+$0E ; Aux Byte 5  
ICAX6 = IOCB+$0F ; Aux Byte 6  


;;=================================================

ROM_CSET = $E000

;; Central I/O Vector
;; All CIO operations go through this address.
CIOV = $E456

;; JSR to set Vertical Blank Interupt Vector/Timer values.
;; X register is the MSB of vector/routine or timer value.
;; Y register is the LSB of vector/routine or timer value.
;; A register is the number of the Vertical Blank routine to change:
;;    1 == CDTMV1 - decremented Immediate VBI Stage 1 -- JSR to CDTMA1 $0226
;;    2 == CDTMV2 - decremented Immediate VBI Stage 2 -- JSR to CDTMA2 $0228
;;    3 == CDTMV3 - decremented Immediate VBI Stage 2 -- Zero CDTMF3 $022A
;;    4 == CDTMV4 - decremented Immediate VBI Stage 2 -- Zero CDTMF4 $022C
;;    5 == CDTMV5 - decremented Immediate VBI Stage 2 -- Zero CDTMF5 $022E
;;    6 == Immediate VBI
;;    7 == deferred VBI
SETVBV = $E45C

;; User Immediate VBI routine should end by a JMP to this address 
;; to continue the OS Vertical Blank routine. 
SYSVBV = $E45F

;; User Deferred VBI routine should end by a JMP to this address 
;; to continue the OS Vertical Blank routine. 
XITVBV = $E462
