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
;;
;; OS Page 0

;; Real Time Clock incremented during the vertical blank.
;; This is three addresses $12, $13, and $14.
;; The value of $14 is incremented  every vertical blank.
;; When the value of $14 reaches $FF on the next increment it rolls 
;; over to $00 and then the value of $13 increments.
;; $14 rollover/$13 increment occurs approximately every 4.27 seconds.
;; Likewise, when the value if $13 reaches $FF and it rolls 
;; over to $00,then the value of $12 increments.
;; $13 rollover/$12 increment occurs approximately every 18.2 munutes.
RTCLOK = $12 ; and $13, and $14.  
RTCLOK60 = $14 ; incremented every jiffy/frame.
;;
;; Zero Page copy of CIO's IOCB
;; to be continued....


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

;;
;; OS Page 2 
VDSLST = $0200 ;; Display List interrupt.


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
