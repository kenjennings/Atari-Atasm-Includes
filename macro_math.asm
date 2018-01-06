;===============================================================================
;	MATH MACROS
;===============================================================================
; Helper macros to shorten repetitive tasks and make more readable code
;===============================================================================

;===============================================================================
; 8-BIT MATH
;===============================================================================
; 
;===============================================================================

;-------------------------------------------------------------------------------
;                                                         BYTE_M_EQ_V_SUB_V   A
;-------------------------------------------------------------------------------
; mByte_M_eq_V_Sub_V <result (address)>, <value1>, <value2>
;
; Subtract literal <Value2> from literal <Value1>, store in <Result (address)>.
;
; or 
; result = Value1 - Value2
; 
; or (more exactly) 
; Poke result, Value1 - Value2
;-------------------------------------------------------------------------------
.macro mByte_M_eq_V_Sub_V
	.if %0<>3
		.error "mByte_M_eq_V_Sub_V: 3 arguments (result addr, value1, value2) required."
	.else
	    sec        ; clear carry/borrow
		lda #<%2   ; Low byte of Value1
		sbc #<%3   ; subtract low byte of Value2
		sta %1     ; save in Result
	.endif
.endm


;-------------------------------------------------------------------------------
;                                                         BYTE_M_EQ_M_SUB_V   A
;-------------------------------------------------------------------------------
; mByte_M_eq_M_Sub_V <result (address)>, <address>, <value>
;
; Subtract literal <Value> from value at <Address>, store in <Result (address)>.
;
; or 
; result = address - Value
; 
; or (more exactly) 
; Poke result, Peek(address) - Value 
;-------------------------------------------------------------------------------
.macro mByte_M_eq_M_Sub_V
  .if %0<>3
    .error "mByte_M_eq_M_Sub_V: 3 arguments (result addr, addr, value) required."
  .else
    sec        ; clear carry/borrow
    lda %2     ; Low byte of Address
    sbc #<%3   ; subtract low byte of Value
    sta %1     ; save in Result
	.endif
.endm


;-------------------------------------------------------------------------------
;                                                         BYTE_M_EQ_V_SUB_M   A
;-------------------------------------------------------------------------------
; mByte_M_eq_M_Sub_V <result (address)>, <value>, <address>
;
; Subtract value at <Address> from literal <Value>, store in <Result (address)>.
;
; or 
; result = Value - Address
; 
; or (more exactly) 
; Poke result, Value - Peek(address)
;-------------------------------------------------------------------------------
.macro mByte_M_eq_M_Sub_V
  .if %0<>3
    .error "mByte_M_eq_M_Sub_V: 3 arguments (result addr, value, addr) required."
  .else
    sec        ; clear carry/borrow
    lda #<%2   ; Low byte of Value
    sbc %3     ; subtract low byte of Address
    sta %1     ; save in Result
	.endif
.endm

;-------------------------------------------------------------------------------
;                                                         BYTE_M_EQ_M_SUB_M   A
;-------------------------------------------------------------------------------
; mByte_M_eq_M_Sub_V <result (address)>, <address1>, <address2>
;
; Subtract value at <address2> from value at <Address1>, store in <Result (address)>.
;
; or 
; result = address1 - address2
; 
; or (more exactly) 
; Poke result, Peek(address1) - Peek(address2) 
;-------------------------------------------------------------------------------
.macro mByte_M_eq_M_Sub_V
  .if %0<>3
    .error "mByte_M_eq_M_Sub_V: 3 arguments (result addr, addr1, addr2) required."
  .else
    sec        ; clear carry/borrow
    lda %2     ; Low byte of Address
    sbc %3     ; subtract low byte of Value
    sta %1     ; save in Result
	.endif
.endm
  


;===============================================================================
; 16-BIT MATH
;===============================================================================
; 
;===============================================================================

;-------------------------------------------------------------------------------
;                                                         WORD_M_EQ_V_SUB_V   A
;-------------------------------------------------------------------------------
; mWord_M_eq_V_Sub_V <result (address)>, <value1>, <value2>
;
; Subtract literal <Value2> from literal <Value1>, store in <Result (address)>.
;
; or 
; result = Value1 - Value2
; 
; or (more exactly) 
; Dpoke result, Value1 - Value2
;-------------------------------------------------------------------------------
.macro mWord_M_eq_V_Sub_V
  .if %0<>3
    .error "mWord_M_eq_V_Sub_V: 3 arguments (result addr, value1, value2) required."
  .else
    sec        ; clear carry/borrow
    lda #<%2   ; Low byte of Value1
    sbc #<%3   ; subtract low byte of Value2
    sta %1     ; save in low byte of Result
    lda #>%2   ; high byte of Value1
    sbc #>%3   ; subtract high byte of Value2
    sta %1+1   ; save in high byte of Result 
	.endif
.endm


;-------------------------------------------------------------------------------
;                                                         WORD_M_EQ_M_SUB_V   A
;-------------------------------------------------------------------------------
; mWord_M_eq_M_Sub_V <result (address)>, <address>, <value>
;
; Subtract literal <Value> from value at <Address>, store in <Result (address)>.
;
; or 
; result = address - Value
; 
; or (more exactly) 
; Dpoke result, Dpeek(address) - Value 
;-------------------------------------------------------------------------------
.macro mWord_M_eq_M_Sub_V
  .if %0<>3
    .error "mWord_M_eq_M_Sub_V: 3 arguments (result addr, addr, value) required."
  .else
    sec        ; clear carry/borrow
    lda %2     ; Low byte of Address
    sbc #<%3   ; subtract low byte of Value
    sta %1     ; save in low byte of Result
    lda %2+1   ; high byte of Address
    sbc #>%3   ; subtract high byte of Value
    sta %1+1   ; save in high byte of Result 
	.endif
.endm


;-------------------------------------------------------------------------------
;                                                         WORD_M_EQ_V_SUB_M   A
;-------------------------------------------------------------------------------
; mWord_M_eq_M_Sub_V <result (address)>, <value>, <address>
;
; Subtract value at <Address> from literal <Value>, store in <Result (address)>.
;
; or 
; result = Value - Address
; 
; or (more exactly) 
; Dpoke result, Value - Dpeek(address)
;-------------------------------------------------------------------------------
.macro mWord_M_eq_V_Sub_M
  .if %0<>3
    .error "mWord_M_eq_V_Sub_M: 3 arguments (result addr, value, addr) required."
  .else
    sec        ; clear carry/borrow
    lda #<%2     ; Low byte of Value
    sbc %3   ; subtract low byte of Address
    sta %1     ; save in low byte of Result
    lda #>%2   ; high byte of Value
    sbc %3+1   ; subtract high byte of Address
    sta %1+1   ; save in high byte of Result 
	.endif
.endm

;-------------------------------------------------------------------------------
;                                                         WORD_M_EQ_M_SUB_M   A
;-------------------------------------------------------------------------------
; mWord_M_eq_M_Sub_V <result (address)>, <address1>, <address2>
;
; Subtract value at <address2> from value at <Address1>, store in <Result (address)>.
;
; or 
; result = address1 - address2
; 
; or (more exactly) 
; Dpoke result, Dpeek(address1) - Dpeek(address2) 
;-------------------------------------------------------------------------------
.macro mWord_M_eq_M_Sub_M
  .if %0<>3
    .error "mWord_M_eq_M_Sub_M: 3 arguments (result addr, addr1, addr2) required."
  .else
    sec        ; clear carry/borrow
    lda %2     ; Low byte of Address
    sbc %3     ; subtract low byte of Value
    sta %1     ; save in low byte of Result
    lda %2+1   ; high byte of Address
    sbc %3+1   ; subtract high byte of Value
    sta %1+1   ; save in high byte of Result 
	.endif
.endm
  
