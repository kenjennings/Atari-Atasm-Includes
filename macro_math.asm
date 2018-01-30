;===============================================================================
;	MATH MACROS
;===============================================================================
; 8-bit and 16-bit math
; 
; Warning -- WORK IN PROGRESS, UNTESTED
;
; Naming conventions...
; 
; mSIZE_...  Size is the size of the result.  Byte or Word
; 
; M = Memory.  Address supplied to reference the value.
; 
; V = Value.   Literal value. Same size as the result.
;
;===============================================================================
; 8-BIT MATH - ADDITION
;===============================================================================
; mByte_M_eq_V_Add_V
; mByte_M_eq_M_Add_V
; mByte_M_eq_V_Add_M
; mByte_M_eq_M_Add_M
; mByte_Add
; 
;===============================================================================
; 8-BIT MATH - SUBTRACTION
;===============================================================================
; mByte_M_eq_V_Sub_V
; mByte_M_eq_M_Sub_V
; mByte_M_eq_V_Sub_M
; mByte_M_eq_M_Sub_M
; mByte_Sub
;
;===============================================================================
; 8-BIT MATH - OTHER TRICKS
;===============================================================================
; mByte_M_Abs_M
; mByte_M_Abs_V
; mByte_Abs
;
;===============================================================================

        

;===============================================================================
; 16-BIT MATH - ADDITION
;===============================================================================
; mWord_M_eq_V_Add_V
; mWord_M_eq_M_Add_V
; mWord_M_eq_V_Add_M
; mWord_M_eq_M_Add_M
; mWord_Add
; 
;===============================================================================
; 16-BIT MATH - SUBTRACTION
;===============================================================================
; mWord_M_eq_V_Sub_V
; mWord_M_eq_M_Sub_V
; mWord_M_eq_V_Sub_M
; mWord_M_eq_M_Sub_M
; mWord_Sub
;===============================================================================


;===============================================================================
; 8-BIT MATH - ADDITION
;===============================================================================
; 
;===============================================================================

;-------------------------------------------------------------------------------
;                                                        BYTE_M_EQ_V_PLUS_V   A
;-------------------------------------------------------------------------------
; mByte_M_eq_V_Add_V <result (address)>, <value1>, <value2>
;
; Add literal <Value2> plus literal <Value1>, store in <Result (address)>.
;
; or 
; result = Value1 + Value2
; 
; or (more exactly) 
; Poke result, Value1 + Value2
;-------------------------------------------------------------------------------
.macro mByte_M_eq_V_Add_V
	.if %0<>3
		.error "mByte_M_eq_V_Add_V: 3 arguments (result addr, value1, value2) required."
	.else
		clc        ; clear carry/borrow
		lda #<%2   ; Low byte of Value1
		adc #<%3   ; add low byte of Value2
		sta %1     ; save in Result
	.endif
.endm


;-------------------------------------------------------------------------------
;                                                        BYTE_M_EQ_M_PLUS_V   A
;-------------------------------------------------------------------------------
; mByte_M_eq_M_Add_V <result (address)>, <address>, <value>
;
; Add literal <Value> from value at <Address>, store in <Result (address)>.
;
; or 
; result = address + Value
; 
; or (more exactly) 
; Poke result, Peek(address) + Value 
;-------------------------------------------------------------------------------
.macro mByte_M_eq_M_Add_V
  .if %0<>3
    .error "mByte_M_eq_M_Add_V: 3 arguments (result addr, addr, value) required."
  .else
    clc        ; clear carry/borrow
    lda %2     ; Low byte of Address
    adc #<%3   ; add low byte of Value
    sta %1     ; save in Result
	.endif
.endm


;-------------------------------------------------------------------------------
;                                                        BYTE_M_EQ_V_PLUS_M   A
;-------------------------------------------------------------------------------
; mByte_M_eq_V_Add_M <result (address)>, <value>, <address>
;
; Add value at <Address> from literal <Value>, store in <Result (address)>.
;
; or 
; result = Value + Address
; 
; or (more exactly) 
; Poke result, Value + Peek(address)
;-------------------------------------------------------------------------------
.macro mByte_M_eq_V_Add_M
  .if %0<>3
    .error "mByte_M_eq_V_Add_M: 3 arguments (result addr, value, addr) required."
  .else
    clc        ; clear carry/borrow
    lda #<%2   ; Low byte of Value
    adc %3     ; add low byte of Address
    sta %1     ; save in Result
	.endif
.endm

;-------------------------------------------------------------------------------
;                                                        BYTE_M_EQ_M_PLUS_M   A
;-------------------------------------------------------------------------------
; mByte_M_eq_M_Add_M <result (address)>, <address1>, <address2>
;
; Add value at <address2> from value at <Address1>, store in <Result (address)>.
;
; or 
; result = address1 + address2
; 
; or (more exactly) 
; Poke result, Peek(address1) + Peek(address2) 
;-------------------------------------------------------------------------------
.macro mByte_M_eq_M_Add_M
  .if %0<>3
    .error "mByte_M_eq_M_Add_M: 3 arguments (result addr, addr1, addr2) required."
  .else
    clc        ; clear carry/borrow
    lda %2     ; Low byte of Address
    adc %3     ; add low byte of Value
    sta %1     ; save in Result
	.endif
.endm

;-------------------------------------------------------------------------------
;                                                         BYTE_ADD   A
;-------------------------------------------------------------------------------
; mByte_Add <result (address)>, <argument1>, <argument2>
;
; Add value at <argument2> to value at/of <argument1>, 
; store in <Result (address)>.
;
; This provides a wrapper that figures out which math macro to call
; simplifying the choice of M and V.
;
; If argument2 is a value less than 256 it will assume that argument2
; is a V parameter for explicity value.  If it is greater then 256 it
; will assume it is an address.
; 
; This is correct in most cases.  However, if page zero addresses are 
; intended for arguments then the programmer must explicitly
; invoke the M_Sub_M macro.  This macro will only treat argument1 as an 
; address if it equals the result address assuming common use of X=X+Y. 
; And, it is possible this choice may be wrong.  
; 
;-------------------------------------------------------------------------------
.macro mByte_Add
	.if %0<>3
		.error "mByte_Add: 3 arguments (result addr, arg1, arg2) required."
	.else
		.if %2>255 .OR %1=%2 ; arg1 = M and allowing for X = X + Y
			.if %3>255 ; arg2 = M
				mByte_M_eq_M_Add_M %1, %2, %3 ; M = M + M
			.else ; arg2 = V
				mByte_M_eq_M_Add_V %1, %2, %3 ; M = M + V
			.endif
		.else     ; arg1 =  V
			.if %3>255 ; arg2 = M
				mByte_M_eq_V_Add_M %1, %2, %3 ; M = V + M
			.else ; arg2 = V
				mByte_M_eq_V_Add_V %1, %2, %3 ; M = V + V
			.endif
		.endif
	.endif
.endm

;===============================================================================
; 8-BIT MATH - SUBTRACTION
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
		sec        ; set carry/borrow
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
    sec        ; set carry/borrow
    lda %2     ; Low byte of Address
    sbc #<%3   ; subtract low byte of Value
    sta %1     ; save in Result
	.endif
.endm


;-------------------------------------------------------------------------------
;                                                         BYTE_M_EQ_V_SUB_M   A
;-------------------------------------------------------------------------------
; mByte_M_eq_V_Sub_M <result (address)>, <value>, <address>
;
; Subtract value at <Address> from literal <Value>, store in <Result (address)>.
;
; or 
; result = Value - Address
; 
; or (more exactly) 
; Poke result, Value - Peek(address)
;-------------------------------------------------------------------------------
.macro mByte_M_eq_V_Sub_M
  .if %0<>3
    .error "mByte_M_eq_V_Sub_M: 3 arguments (result addr, value, addr) required."
  .else
    sec        ; set carry/borrow
    lda #<%2   ; Low byte of Value
    sbc %3     ; subtract low byte of Address
    sta %1     ; save in Result
	.endif
.endm

;-------------------------------------------------------------------------------
;                                                         BYTE_M_EQ_M_SUB_M   A
;-------------------------------------------------------------------------------
; mByte_M_eq_M_Sub_M <result (address)>, <address1>, <address2>
;
; Subtract value at <address2> from value at <Address1>, store in <Result (address)>.
;
; or 
; result = address1 - address2
; 
; or (more exactly) 
; Poke result, Peek(address1) - Peek(address2) 
;-------------------------------------------------------------------------------
.macro mByte_M_eq_M_Sub_M
  .if %0<>3
    .error "mByte_M_eq_M_Sub_M: 3 arguments (result addr, addr1, addr2) required."
  .else
    sec        ; set carry/borrow
    lda %2     ; Low byte of Address
    sbc %3     ; subtract low byte of Value
    sta %1     ; save in Result
	.endif
.endm
 
;-------------------------------------------------------------------------------
;                                                         BYTE_SUB   A
;-------------------------------------------------------------------------------
; mByte_Sub <result (address)>, <argument1>, <argument2>
;
; Subtract value at <argument2> from value at/of <argument1>, 
; store in <Result (address)>.
;
; This provides a wrapper that figures out which math macro to call
; simplifying the choice of M and V.
;
; If argument2 is a value less than 256 it will assume that argument2
; is a V parameter for explicity value.  If it is greater then 256 it
; will assume it is an address.
; 
; This is correct in most cases.  However, if page zero addresses are 
; intended for arguments then the programmer must explicitly
; invoke the M_Sub_M macro.  This macro will only treat argument1 as an 
; address if it equals the result address assuming common use of X=X-Y. 
; And, it is possible this choice may be wrong.  
; 
;-------------------------------------------------------------------------------
.macro mByte_Sub
	.if %0<>3
		.error "mByte_Sub: 3 arguments (result addr, arg1, arg2) required."
	.else
		.if %2>255 .OR %1=%2 ; arg1 = M and allowing for X = X - Y
			.if %3>255 ; arg2 = M
				mByte_M_eq_M_Sub_M %1, %2, %3 ; M = M - M
			.else ; arg2 = V
				mByte_M_eq_M_Sub_V %1, %2, %3 ; M = M - V
			.endif
		.else     ; arg1 =  V
			.if %3>255 ; arg2 = M
				mByte_M_eq_V_Sub_M %1, %2, %3 ; M = V - M
			.else ; arg2 = V
				mByte_M_eq_V_Sub_V %1, %2, %3 ; M = V - V
			.endif
		.endif
	.endif
.endm


;===============================================================================
; 8-BIT MATH - OTHER TRICKS
;===============================================================================
; mByte_M_Abs_M
; mByte_M_Abs_V
; mByte_Abs
;
;===============================================================================

;-------------------------------------------------------------------------------
;                                                         BYTE_M_ABS_M   A
;-------------------------------------------------------------------------------
; mByte_M_Abs_M <result (address)>, <argument1>
;
; Store in result location the Absolute value of the byte at the 
; argument 1 address.
; 
; Like:
; RESULT = ABS(X)
; 
; Or:
; POKE RESULT, ABS(PEEK(X))
;-------------------------------------------------------------------------------

.macro mByte_M_Abs_M
    .if %0<>2
        .error "mByte_M_Abs_M: 2 arguments (result addr, addr1) required."
    .else
        lda %2     ; byte at address
        bpl @abs_plus ; positive
        eor #$FF   ; negative.  Exclusive OR bits
        sta %1     ; save result
        inc %1     ; Two's compliment is +1
        jmp @abs_done ; no branch is reliable after the inc.
@abs_plus
        sta %1     ; save result
@abs_done
.endm

;-------------------------------------------------------------------------------
;                                                         BYTE_M_ABS_V   A
;-------------------------------------------------------------------------------
; mByte_M_Abs_V <result (address)>, <argument1>
;
; Store in result location the Absolute value of the argument 1 byte.
; 
; Like:
; RESULT = ABS(X)
; 
; Or:
; POKE RESULT, ABS(X)
;-------------------------------------------------------------------------------

.macro mByte_M_Abs_V
    .if %0<>2
        .error "mByte_M_Abs_V: 2 arguments (result addr, byte) required."
    .else
        lda #%2     ; byte
        bpl @abs_plus ; positive
        eor #$FF   ; negative.  Exclusive OR bits
        sta %1     ; save result
        inc %1     ; Two's compliment is +1
        jmp @abs_done ; no branch is reliable after the inc.
@abs_plus
        sta %1     ; save result
@abs_done
    .endif
.endm

;-------------------------------------------------------------------------------
;                                                         BYTE_ABS   A
;-------------------------------------------------------------------------------
; mByte_Abs <result (address)>, <argument1>
;
; Store in result location the Absolute value of the argument1.
;
; If the argument1 is the same value as the result it is treated as an address.
;
; If the argument1 is greater than 255 then it is treated as an address.
;
; Otherwise the argument is treated as an explicit byte value.
;
;-------------------------------------------------------------------------------

.macro mByte_Abs
    .if %0<>2
        .error "mByte_Abs: 2 arguments (result addr, argument1) required."
    .else
        .if %2>255 .OR %1=%2 ; arg1 = M 
			mByte_M_Abs_M %1, %2 ; M = Abs(M)
		.else ; arg2 = V
			mByte_M_Abs_V %1, %2 ; M = Abs(V)
		.endif
	.endif
.endm

        
        
        

;===============================================================================
; 16-BIT MATH - ADDITION
;===============================================================================
; 
;===============================================================================

;-------------------------------------------------------------------------------
;                                                        WORD_M_EQ_V_PLUS_V   A
;-------------------------------------------------------------------------------
; mWord_M_eq_V_Add_V <result (address)>, <value1>, <value2>
;
; Add literal <Value2> from literal <Value1>, store in <Result (address)>.
;
; or 
; result = Value1 + Value2
; 
; or (more exactly) 
; Dpoke result, Value1 + Value2
;-------------------------------------------------------------------------------
.macro mWord_M_eq_V_Add_V
  .if %0<>3
    .error "mWord_M_eq_V_Add_V: 3 arguments (result addr, value1, value2) required."
  .else
    clc        ; clear carry/borrow
    lda #<%2   ; Low byte of Value1
    adc #<%3   ; add low byte of Value2
    sta %1     ; save in low byte of Result
    lda #>%2   ; high byte of Value1
    adc #>%3   ; add high byte of Value2
    sta %1+1   ; save in high byte of Result 
	.endif
.endm


;-------------------------------------------------------------------------------
;                                                        WORD_M_EQ_M_PLUS_V   A
;-------------------------------------------------------------------------------
; mWord_M_eq_M_Add_V <result (address)>, <address>, <value>
;
; Add literal <Value> from value at <Address>, store in <Result (address)>.
;
; or 
; result = address + Value
; 
; or (more exactly) 
; Dpoke result, Dpeek(address) + Value 
;-------------------------------------------------------------------------------
.macro mWord_M_eq_M_Add_V
  .if %0<>3
    .error "mWord_M_eq_M_Add_V: 3 arguments (result addr, addr, value) required."
  .else
    clc        ; clear carry/borrow
    lda %2     ; Low byte of Address
    adc #<%3   ; add low byte of Value
    sta %1     ; save in low byte of Result
    lda %2+1   ; high byte of Address
    adc #>%3   ; add high byte of Value
    sta %1+1   ; save in high byte of Result 
	.endif
.endm


;-------------------------------------------------------------------------------
;                                                        WORD_M_EQ_V_PLUS_M   A
;-------------------------------------------------------------------------------
; mWord_M_eq_V_Add_M <result (address)>, <value>, <address>
;
; Add value at <Address> from literal <Value>, store in <Result (address)>.
;
; or 
; result = Value + Address
; 
; or (more exactly) 
; Dpoke result, Value + Dpeek(address)
;-------------------------------------------------------------------------------
.macro mWord_M_eq_V_Add_M
  .if %0<>3
    .error "mWord_M_eq_V_Add_M: 3 arguments (result addr, value, addr) required."
  .else
    clc        ; clear carry/borrow
    lda #<%2   ; Low byte of Value
    adc %3     ; add low byte of Address
    sta %1     ; save in low byte of Result
    lda #>%2   ; high byte of Value
    adc %3+1   ; add high byte of Address
    sta %1+1   ; save in high byte of Result 
	.endif
.endm

;-------------------------------------------------------------------------------
;                                                        WORD_M_EQ_M_PLUS_M   A
;-------------------------------------------------------------------------------
; mWord_M_eq_M_Add_M <result (address)>, <address1>, <address2>
;
; Add value at <address2> from value at <Address1>, store in <Result (address)>.
;
; or 
; result = address1 + address2
; 
; or (more exactly) 
; Dpoke result, Dpeek(address1) + Dpeek(address2) 
;-------------------------------------------------------------------------------
.macro mWord_M_eq_M_Add_M
  .if %0<>3
    .error "mWord_M_eq_M_Add_M: 3 arguments (result addr, addr1, addr2) required."
  .else
    clc        ; clear carry/borrow
    lda %2     ; Low byte of Address
    adc %3     ; add low byte of Value
    sta %1     ; save in low byte of Result
    lda %2+1   ; high byte of Address
    adc %3+1   ; add high byte of Value
    sta %1+1   ; save in high byte of Result 
	.endif
.endm


;-------------------------------------------------------------------------------
;                                                         WORD_ADD   A
;-------------------------------------------------------------------------------
; mWord_Add <result (address)>, <argument1>, <argument2>
;
; Add value at <argument2> to value at/of <argument1>, 
; store in <Result (address)>.
;
; This provides a wrapper that figures out which math macro to call
; simplifying the choice of M and V.
;
; If argument2 is a value less than 256 it will assume that argument2
; is a V parameter for explicity value.  If it is greater then 256 it
; will assume it is an address.
; 
; This is correct in most cases.  However, if page zero addresses are 
; intended for arguments then the programmer must explicitly
; invoke the M_Sub_M macro.  This macro will only treat argument1 as an 
; address if it equals the result address assuming common use of X=X+Y. 
; And, it is possible this choice may be wrong.  
; 
;-------------------------------------------------------------------------------
.macro mWord_Add
	.if %0<>3
		.error "mWord_Add: 3 arguments (result addr, arg1, arg2) required."
	.else
		.if %2>255 .OR %1=%2 ; arg1 = M and allowing for X = X + Y
			.if %3>255 ; arg2 = M
				mWord_M_eq_M_Add_M %1, %2, %3 ; M = M + M
			.else ; arg2 = V
				mWord_M_eq_M_Add_V %1, %2, %3 ; M = M + V
			.endif
		.else     ; arg1 =  V
			.if %3>255 ; arg2 = M
				mWord_M_eq_V_Add_M %1, %2, %3 ; M = V + M
			.else ; arg2 = V
				mWord_M_eq_V_Add_V %1, %2, %3 ; M = V + V
			.endif
		.endif
	.endif
.endm

;===============================================================================
; 16-BIT MATH - SUBTRACTION
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
    sec        ; set carry/borrow
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
    sec        ; set carry/borrow
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
; mWord_M_eq_V_Sub_M <result (address)>, <value>, <address>
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
    sec        ; set carry/borrow
    lda #<%2   ; Low byte of Value
    sbc %3     ; subtract low byte of Address
    sta %1     ; save in low byte of Result
    lda #>%2   ; high byte of Value
    sbc %3+1   ; subtract high byte of Address
    sta %1+1   ; save in high byte of Result 
	.endif
.endm

;-------------------------------------------------------------------------------
;                                                         WORD_M_EQ_M_SUB_M   A
;-------------------------------------------------------------------------------
; mWord_M_eq_M_Sub_M <result (address)>, <address1>, <address2>
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
    sec        ; set carry/borrow
    lda %2     ; Low byte of Address
    sbc %3     ; subtract low byte of Value
    sta %1     ; save in low byte of Result
    lda %2+1   ; high byte of Address
    sbc %3+1   ; subtract high byte of Value
    sta %1+1   ; save in high byte of Result 
	.endif
.endm
 
;-------------------------------------------------------------------------------
;                                                         WORD_SUB   A
;-------------------------------------------------------------------------------
; mWord_Sub <result (address)>, <argument1>, <argument2>
;
; Subtract value at <argument2> from value at/of <argument1>, 
; store in <Result (address)>.
;
; This provides a wrapper that figures out which math macro to call
; simplifying the choice of M and V.
;
; If argument2 is a value less than 256 it will assume that argument2
; is a V parameter for explicity value.  If it is greater then 256 it
; will assume it is an address.
; 
; This is correct in most cases.  However, if page zero addresses are 
; intended for arguments then the programmer must explicitly
; invoke the M_Sub_M macro.  This macro will only treat argument1 as an 
; address if it equals the result address assuming common use of X=X-Y. 
; And, it is possible this choice may be wrong.  
; 
;-------------------------------------------------------------------------------
.macro mWord_Sub
	.if %0<>3
		.error "mWord_Sub: 3 arguments (result addr, arg1, arg2) required."
	.else
		.if %2>255 .OR %1=%2 ; arg1 = M and allowing for X = X - Y
			.if %3>255 ; arg2 = M
				mWord_M_eq_M_Sub_M %1, %2, %3 ; M = M - M
			.else ; arg2 = V
				mWord_M_eq_M_Sub_V %1, %2, %3 ; M = M - V
			.endif
		.else     ; arg1 =  V
			.if %3>255 ; arg2 = M
				mWord_M_eq_V_Sub_M %1, %2, %3 ; M = V - M
			.else ; arg2 = V
				mWord_M_eq_V_Sub_V %1, %2, %3 ; M = V - V
			.endif
		.endif
	.endif
.endm
