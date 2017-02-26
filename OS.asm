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
RTCLOK = $12 ; and $13, and $14.  
RTCLOK60 = $14 ; incremented every jiffy/frame.
;;
;; Zero Page copy of CIO's IOCB
;; to be continued....


;; 
ATRACT = $4D

;;
;; OS Page 2 
VDSLST = $0200 ;; Display List interrupt.

