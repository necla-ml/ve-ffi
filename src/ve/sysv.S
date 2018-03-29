/* -----------------------------------------------------------------------
   sysv.S - Copyright (c) 2018, NEC Labs America, LLC
   
   NEC VE Foreign Function Interface

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   ``Software''), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

 
   THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
   WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE.
   ----------------------------------------------------------------------- */

#define LIBFFI_ASM	
#include <fficonfig.h>
#include <ffi.h>

#if defined(__PIC__)
#warning "VE sysv.S -fPIC ?"
#else
#warning "VE sysv.S no PIC ?"
#endif

/* XXX these lose for some platforms, I'm sure. */
#define CNAME(x) x

#define ENTRY(x) .balign 16; .globl CNAME(x)
/* .type CNAME(x),%function; CNAME(x): */

#define CNAMESZ(X) X##auto_size

#define S18_TO_33(OP, FP) \
	OP %s18,48(,FP); \
	OP %s19,56(,FP); \
	OP %s20,64(,FP); \
	OP %s21,72(,FP); \
	OP %s22,80(,FP); \
	OP %s23,88(,FP); \
	OP %s24,96(,FP); \
	OP %s25,104(,FP); \
	OP %s26,112(,FP); \
	OP %s27,120(,FP); \
	OP %s28,128(,FP); \
	OP %s29,136(,FP); \
	OP %s30,144(,FP); \
	OP %s31,152(,FP); \
	OP %s32,160(,FP); \
	OP %s33,168(,FP);

/* Use s18..s25 as spill registers for s0..s7 */
#define SAVE_S0_TO_S7 \
	or	%s18,%s0,(0)1; \
	or	%s19,%s1,(0)1; \
	or	%s20,%s2,(0)1; \
	or	%s21,%s3,(0)1; \
	or	%s22,%s4,(0)1; \
	or	%s23,%s5,(0)1; \
	or	%s24,%s6,(0)1; \
	or	%s25,%s7,(0)1
#define RESTORE_S0_TO_S7 \
	or	%s0,%s18,(0)1; \
	or	%s1,%s19,(0)1; \
	or	%s2,%s20,(0)1; \
	or	%s3,%s21,(0)1; \
	or	%s4,%s22,(0)1; \
	or	%s5,%s23,(0)1; \
	or	%s6,%s24,(0)1; \
	or	%s7,%s25,(0)1

/* TXT_ADDRESS( REG, LABEL )
	Purpose:	load .text symbol address into a registerxi
	Clobbers:	%s12, %lr, REG
*/
#ifdef PIC
#define TXT_ADDRESS( REG, LABEL ) \
	lea	%s12, LABEL@PLT_LO(-24); \
	and	%s12,%s12,(32)0; \
	sic	%lr; \
	lea.sl	%s12,LABEL@PLT_HI(%s12,%lr); \
	or	REG,%s12,(0)1
#else
#define TXT_ADDRESS( REG, LABEL ) \
	lea	%s12, LABEL@LO; \
	and	%s12,%s12,(32)0; \
	lea.sl	%s12,LABEL@HI(,%s12); \
	or	REG,%s12,(0)1
#endif
#define DEFINE_STRING( LABEL,STRING ) \
.section .rodata; \
	.balign 8; \
LABEL:	.asciz STRING; \
	.balign 8; \
	.text

#define PRINT_MSG_U64( NAME, LOAD_S1... ) \
	LOAD_S1; \
	DEFINE_STRING( sysv_str_##NAME, #NAME ); \
	TXT_ADDRESS( %s0, sysv_str_##NAME ); \
	bsic	%lr,(,%s28)

#define PRINT_MSG_U64X( NAME, XSTRING, LOAD_S1... ) \
	LOAD_S1; \
	DEFINE_STRING( sysv_str_##NAME, XSTRING ); \
	TXT_ADDRESS( %s0, sysv_str_##NAME ); \
	bsic	%lr,(,%s28)


	.text
#if 0
####### caller's stack frame
#   Locals and Temporaries
# .............
#             | (Arg Stack, if needed for the call)
#    176(%fp) | Parameter Area for callee
# .............
# R S A..|
# e a r  | 168(%fp)   %s33 Callee-saved register
# g v e  |  ...        ...
# i e a  | 48(%fp)    %s18 Callee-saved register
# s      | 40(%fp)    %s17 Linkage Area Register 
# t      | 32(%fp)    %plt %s16 rocedure Linkage Table Register
# e      | 24(%fp)    %got %s15 Global Offset Table Register
# r......| 16(%fp)    %tp  %s14 Thread Pointer Register
#    8(%sp            %lr  %s10 return address (Link Register)
#    0(%sp)                %sp (or caller's $fp)
# ............ Caller Stack Frame (16-byte alignment)
# 
#    ### same, but wrt %sp for callee
# 
# ............ Callee Stack Frame (16-byte alignment)

#  This began with prologue and epilogue code appropriate for
#  functions returning void
#endif
;#ENTRY(ffi_call_SYSV)
#if 0
.section .comment
.balign 16
msg_hello:
	.ident "hello";
.text

.section .rodata
.balign 16
msg_asciz:
	.asciz "hello asciz"
.text

.section .rodata
.balign 16
msg_ascii:
	.asciz "hello ascii\0"
.text

.balign 16
pmsg_asciz:
	.long	msg_asciz
	.long	msg_ascii
#endif
/* it is easier to just define local strings in .text, even if frowned upon */
.text
.balign 8
txt_asciz1:
	.asciz "txt asciz1"
.balign 8
txt_asciz2:
	.asciz "txt asciz2"
.balign 8
str_s0: .asciz "s0"
.balign 8
str_s18_stkreg: .asciz "s18=stkreg"


.balign 16
.globl ffi_call_SYSV
ffi_call_SYSV:
	;# %s0..%s7 parameter passing, caller save
	;# %s0:	ffi_prep_args	void(*)(char* stack, extended_cif *)
	;# %s1:	&ecif		extended_cif *
	;# %s2:	bytes		from cif->bytes
	;# %s3:	flags		from cif->flags
	;# %s4:	flags2		from cif->flags2, if defined(FFI_EXTRA_CIF_FIELDS)
	;# %s5:	rvalue		unsigned *
	;# %s6:	fn		void (*fn)(void)
	;#stack frame .cfi directives are for nice stack debug
	.cfi_startproc
	st	%fp,0x0(,%sp)
	.cfi_def_cfa_offset 0
	.cfi_offset	9,0		# %fp is %s9
	st	%lr,0x8(,%sp)		# %lr~%s10 Link Reg: ret addr
	;				# skip %tp at 0x10(,%sp)
	st	%got,0x18(,%sp)
	st	%plt,0x20(,%sp)
	lea	%got,_GLOBAL_OFFSET_TABLE_@PC_LO(-24) 	#-0x18
	and	%got,%got,(32)0
	sic	%plt			# %plt := IC + 8
	lea.sl	%got,_GLOBAL_OFFSET_TABLE_@PC_HI(%plt,%got)
	or	%fp,0,%sp		# %fp := %sp
	
	.cfi_def_cfa_register	9	# %s9 is the frame points
	S18_TO_33( st, %fp )		# %s18-%s33 Callee-saved

	/* call to ffi_prep_args can clobber s0..s7, so save rvalue and fn */
sysv_save:
#define REG_ffi_prep_args %s18
#define REG_ptr_ecif %s19
#define REG_cif_bytes %s20
#define REG_flags %s21
#define REG_flags2 %s22
#define REG_rvalue %s23
#define REG_fn %s24
	or	%s18,%s0,(0)1	# ffi_prep_args
	or	%s19,%s1,(0)1	# &ecif
	or	%s20,%s2,(0)1	# cif->bytes
	or	%s21,%s3,(0)1	# flags
	or	%s22,%s4,(0)1	# flags2
	or	%s23,%s5,(0)1	# rvalue
	or	%s24,%s6,(0)1	# fn
#if 0 // DEBUG
	;# print current %s0 register: %s27 = addr of print_s0 function
#ifdef PIC
	lea	%s12, print_s0@PLT_LO(-24)
	and	%s12,%s12,(32)0
	sic	%lr
	lea.sl	%s12,print_s0@PLT_HI(%s12,%lr)
#else
	lea	%s12, print_s0@LO
	and	%s12,%s12,(32)0
	lea.sl	%s12,print_s0@HI(,%s12)
#endif
	or	%s27,%s12,(0)1	# save addr of "print_s0" fn
#endif
#if 1 // DEBUG v.2
	TXT_ADDRESS( %s27, print_s0 )
	TXT_ADDRESS( %s28, print_msg_u64 )
	TXT_ADDRESS( %s29, print_msg_float )
	TXT_ADDRESS( %s30, print_msg_double )
#endif
	
	#.globl neg_needed_stack_size
	.set neg_needed_stack_size, -1024 # ???
	lea	%sp, neg_needed_stack_size(,%fp)
	subu.l	%sp, %sp,%s2		# sp := sp - cif->bytes
	/* simple approach:
		64 [reg args area]
	 +	?? [non-reg-args area]
	 = cif->bytes */
	brge.l.t	%sp,%sl,.L_EoP	# sp >= stack limit[long,taken]?
	
	ld	%s61,0x18(,%tp)	# load param area. Host Memory pointer?
	or	%s62,0,%s0	# spill value of %s0
	lea	%s63,0x13b	# syscall "grow" 
	shm.l	%s63,0x0(%s61)	# stored at addr:0. shm.l ~ Store Host Memory long(8byte)
	shm.l	%sl,0x8(%s61)	# old limit at addr:8
	shm.l	%sp,0x10(%s61)	# new limit at addr:16
	monc
	or	%s0,0,%s62	# restore value of $s0
.L_EoP: ;# end prologue

#define FUNCTIONALITY 3
/* 1 : call prep_args
   2 : invoke function
   3 : return value
*/
#if FUNCTIONALITY >= 2
sysv_prep:
	/* (call preg_args, invoke function, handle return values) */

	;#or	%s26,%sp,(0)1	# spill our stack frame pointer

	;# ? %s12 Outer Register:
	;#	"Used for pointing start address of called function"
	or	%s12,%s0,(0)1	# ffi_prep_args(
	lea	%s0,176(,%sp)	#	%s0 = char *stack, 1st in Parameter Area for callee
	;# OUCH WRONG, we execute in current stack frame!!!	st	%sp,0x0(,%s0)	#
	;#or	%s1,0,%s1	#	%s1 = extended_cif* ecif
	or	%s1,%s19,(0)1	#	%s1 = extended_cif* ecif
	bsic	%lr,(,%s12)	# )
	;# ffi_prep_args writes the argument values into the stack
	;# we expect *stack value to end up pointing cif->bytes higher address
	;#  BUT because of <= 8 register-only args, we'll have ffi_prep_args
	;# ALSO (ALWAYS) set up an additional 8*8 bytes blindly slurped into %s0..%s7
	#or	%sp,%s26,(0)1	# restore our stack frame, with *stack values written
#endif

sysv_restore:
#if 0
#if 0
	ld	%s0,-1*8(,%fp); ld	%s1,-2*8(,%fp); ld	%s2,-3*8(,%fp); ld	%s3,-4*8(,%fp);
	ld	%s4,-5*8(,%fp); ld	%s5,-6*8(,%fp); ld	%s6,-7*8(,%fp); ld	%s7,-8*8(,%fp);
#else
	or	%s0,%s18,(0)1	# ffi_prep_args (already called, %s18 is free now)
	or	%s1,%s19,(0)1	# &ecif
	or	%s2,%s20,(0)1	# cif->bytes
	or	%s3,%s21,(0)1	# flags
	or	%s4,%s22,(0)1	# flags2
	or	%s5,%s23,(0)1	# rvalue
	or	%s6,%s24,(0)1	# fn
#endif
#endif
;# s0/18:ffi_prep_args s1/19:&ecif s2/20:bytes s3/21:flags s4/22:flags2 s5/23:rvalue s6/24:fn

#if FUNCTIONALITY >= 2
sysv_invoke:
	/* invoke function ........................ in CURRENT stack frame */
	/* load s0..s7 regs and execute call to fn (%s6-->%s24) */
	/* How?  A simple [slow] way is to set aside a memory area
	         and blind-copy into s0..s7. */
	lea	%s0,176(,%sp)	#	%s0 = char *stack, 1st in Parameter Area for callee
	adds.l	%s1,%s20,(58)1	#	cif->bytes - 64 ??
#if 0 /* no debug */
	adds.l	%s63,%s0,%s1	# %s33 : char* stkreg = stack + cif->bytes - 64
#else /* debug: need spill register */
	adds.l	%s33,%s0,%s1	# %s33 : char* stkreg = stack + cif->bytes - 64
	PRINT_MSG_U64(s33_stkreg,  or %s1,0,%s33 )
	or	%s63,0,%s33
#endif
	ld	%s0,0*8(,%s63)	# mirror stkreg --> %s0..7
	ld	%s1,1*8(,%s63)
	ld	%s2,2*8(,%s63)
	ld	%s3,3*8(,%s63)
	ld	%s4,4*8(,%s63)
	ld	%s5,5*8(,%s63)
	ld	%s6,6*8(,%s63)
	ld	%s7,7*8(,%s63)
sysv_inv2:				/* CALL "fn" */
	
	/* point stack at non-register arg area */
	;#lea	%sp,...
	/* call fn (was in %s6, now in %s24) */
	or	%s12,%s24,(0)1
        bsic	%lr,(,%s12)	# retval (if rvalue!=NULL) is in %s0 (I think)
	st	%s0,-4*8(,%fp)	# spill retval [which couldl be up to 4 registers long]
	st	%s1,-3*8(,%fp)
	st	%s2,-2*8(,%fp)
	st	%s3,-1*8(,%fp)

sysv_dbg:
#if 1 /* debug */
	PRINT_MSG_U64(s0_retval,  or %s1,0,%s0)
	PRINT_MSG_U64(s23_rvalue_ptr,  or %s1,0,%s23)
#endif
#endif /* FUNCTIONALITY >= 2 */
	
#if FUNCTIONALITY >= 3
sysv_retval:
	/* handle return value ............................. */
	/* or	%s0,0,return_value */
	;# (*rvalue) = %s0
#if 0 /* obtain ecif->cif->rtype->type ?? */
	/* I think this would do it:
		or	%s63,0,%s19	# ecif (ptr)
		ld	%s62,0(0,%s63)	# ecif->cif
		ld	%s61,16(0,%s62)	# ecif->cif->rtype
		ld2b.zx	%s60,10(0,%s61)	# ecif->cif->type.type
	*/
#endif
	PRINT_MSG_U64(s21_flags,  or %s1,0,%s21) # %s21 = flags == ecif->cif->rtype->type
	breq.l.nt	%s23,0,.L_BoE	# rvalue == NULL? skip rvalue
	breq.w.t	FFI_TYPE_VOID,%s21, sysv_ret2
.L1.notvoid:
	ld	%s0,-4*8(,%fp)	# retrieve retval [opt]
	ld	%s1,-3*8(,%fp)
	ld	%s2,-2*8(,%fp)
	ld	%s3,-1*8(,%fp)
#if 1
	brne.w.t	FFI_TYPE_FLOAT,%s21, .L1.notfloat
sysv_rfloat:
	ld	%s0,-4*8(,%fp)	# retrieve retval
	stu	%s0,0(,%s23)	# register-float ---> 4-byte float memory
#if 0 /* debug */
	or	%s1,0,%s0	# retval-float
	DEFINE_STRING( str_rfloat, "rfloat" )
	TXT_ADDRESS( %s0, str_rfloat )
	bsic	%lr,(,%s29)	# print_msg_float
	//ldu	%s1,0(,%s23)	# flags ( == ecif->cif->rtype->type
	//DEFINE_STRING( str_rdouble, "rdouble" )
	//TXT_ADDRESS( %s0, str_rdouble )
	//bsic	%lr,(,%s30)	# print_msg_float
#endif
	br.w	sysv_ret1
.L1.notfloat:
#endif
#if 0
	brne.w.t	FFI_TYPE_LONGDOUBLE,%s21,.L1.notldouble
sysv_rldouble:
	ld	%s0,-4*8(,%fp)	# retrieve retval
	ld	%s1,-3*8(,%fp)
	st	%s0,0(,%s23)
	st	%s1,8(,%s23)
	br.w	.L_BoE
.L1.notldouble:
#endif
#if 0
	brne.w.t	FFI_TYPE_COMPLEX,%s21,.L1.notcomplex
sysv_rcomplex: # double complex ... ?
	ld	%s0,-4*8(,%fp)	# retrieve retval
	ld	%s1,-3*8(,%fp)
	st	%s0,0(,%s23)
	st	%s1,8(,%s23)
	br.w	.L_BoE
.L1.notcomplex:
#endif
#if 1
	brne.w.t	FFI_TYPE_STRUCT,%s21,.L1.notstruct
sysv_rstruct:
	PRINT_MSG_U64X( struct_TBD, "*** struct TBD ***", ld %s0,-4*8(,%fp))	# retrieve retval = addr with content
	/* size = ? */
	/* memcpy? for example XXX (wrong) */
	ld	%s0,-4*8(,%fp)	# retrieve retval = addr with content
	ld	%s1,0(,%s0)	# retrieve first 64-bits of struct/union...
	st	%s1,0(,%s23)	# (*rvalue) = %s0  (ok for 8-byte return type)
	addu.l	%s1,8,%s1
	ld	%s0,-4*8(,%fp)	# retrieve retval = addr with content
	br.w	sysv_ret1
.L1.notstruct:
#endif
#if 1
	brne.w.t	FFI_TYPE_POINTER,%s21,.L1.notpointer
sysv_rpointer:
	PRINT_MSG_U64( rpointer,  ld %s0,-4*8(,%fp))	# %s0 = void* ret
	ld	%s0,-4*8(,%fp)	# %s0 = void* ret
	st	%s0,0(,%s23)	# (*rvalue) = ret
	br.w	sysv_ret1
.L1.notpointer:
#endif
	/* ###################### */
	/* <= 8-byte scalar types [except FLOAT] are OK with just: */
	ld	%s0,-4*8(,%fp)	# retrieve retval (1 reg)
	;#ld	%s1,-3*8(,%fp)
	;#ld	%s2,-2*8(,%fp)
	;#ld	%s3,-1*8(,%fp)
	st	%s0,0(,%s23)	# (*rvalue) = %s0  (ok for 8-byte return type)
	/* ###################### */
#if 1 /* debug */
	PRINT_MSG_U64X( u64_rvalue, "*rvalue", ld %s1,0(,%s23))	# print *rvalue (as uint64)
	br.w sysv_ret1
#else
	br.w .L_BoE # reg return value already in place
#endif

sysv_ret4:
	ld	%s3,-1*8(,%fp)
	ld	%s2,-2*8(,%fp)
sysv_ret2:
	ld	%s1,-3*8(,%fp)
sysv_ret1:
	ld	%s0,-4*8(,%fp)	# retrieve retval [change order if incorrect for multibyte]
#endif /* FUNCTIONALITY >= 3 */

sysv_epilogue:
.L_BoE: ;# begin epilogue
	S18_TO_33( ld, %fp )	# Paranoia: %s18..33 belong to caller
	;# Remove the space we pushed for the args
	or	%sp,0,%fp	# callee stack frame -> Gone!
	.cfi_def_cfa	11,8
	ld	%got,0x18(,%sp)
	ld	%plt,0x20(,%sp)
	ld	%lr, 0x8(,%sp)
	ld	%fp, 0x0(,%sp)
sysv_ret:
	b.l	(,%lr)
.L_EoE: #end of epilogue
	.cfi_endproc
#set	.L.1.2auto_size,	0xfffffffffffffe50	# 432 Bytes
	;#.size CNAMESZ(ffi_call_SYSV), .L_EoE - CNAME(ffi_call_SYSV)
	;#.set CNAMESZ(ffi_call_SYSV), .- CNAME(ffi_call_SYSV)
	;#.set ffi_call_SYSV_sz, .-ffi_call_SYSV
	.set ffi_call_SYSV_sz, 0x0  # -ve value, additional stack space for called function
# ------------------------------------------------------------
#if 0
%s12: Outer register, used for pointing start address of call3ed function
absolute direct extern void function()
	function();
		lea %s12,function@LO
		and %s12,%s12,(32)0
		lea.sl %s12,function@HI(,%s12)
		bsic %lr,(,%s12)
PIC direct extern void function();
	function();
		lea %s12,function@PLT_LO(-24)
		and %s12,%s12,(32)0
		sic %s63
		lea.sl %s12,function@PLT_HI(%s12,%s63)
		bsic %lr,(,%s12)
Abs, indirect
	extern void (*ptr)();
	extern void name();
	ptr = name;
		lea %s63,name@LO
		and %s63,%s63,(32)0
		lea.sl %s63,name@HI(,%s63)
		lea %s62,ptr@LO
		and %s62,%s62,(32)0
		lea.sl %s62,ptr@HI(,%s62)
		and %s62,%s62,(32)0
		lea.sl %s62,ptr@HI(,%s62)
		st %s63,(,%s62)
	(*ptr)();
		lea %s61,ptr@LO
		and %s61,%s61,(32)0
		lea.sl %s61,ptr@HI(,%s61)
		ld %s61,(,%s61)
		or %s12,%s61,(0)1
		bsic %lr,(,%s12)
PIC indirect
	extern void (*ptr)();
	extern void name();
	ptr = name;
		lea %s63,name@GOT_LO
		and %s63,%s63,(32)0
		lea.sl %s63,name@GOT_HI(%s63,%got)
		ld %s63,(,%s63)
		lea %s62,ptr@GOT_LO
		and %s62,%s62,(32)0
		lea.sl %s62,ptr@GOT_HI(%s62,%got)
		ld %s62,(,%s62)
		st %s63,(,%s62)
	(*ptr)();
		lea %s61,ptr@GOT_LO
		and %s61,%s61,(32)0
		lea.sl %s61,ptr@GOT_HI(%s61,%got)
		ld %s61,(,%s61)
		ld %s61,(,%s61)
		or %s12,%s61,(0)1
		bsic %lr,(,%s12)
PIC static
	static void function();
		.type function,@function
		function:
	function();
		lea %s12,function@PC_LO(-24)
		and %s12,%s12,(32)0
		sic %s63
		lea.sl %s12,function@PC_HI(%s12,%s63)
		bsic %lr,(,%s12)
#endif
#if 0
.L_epilogue:
	add	r14, r63, r15

	ld.l	r15, 0, r14
	ld.l	r15, 4, r18
	ld.q	r15, 8, r28
	ld.q	r15, 16, r29
	ld.q	r15, 24, r30
	ld.q	r15, 32, r31
	ld.q	r15, 40, r32
	addi.l	r15, 48, r15
	ptabs	r18, tr0
	blink	tr0, r63

.LFE1:
.ffi_call_SYSV_end:
	.size	 CNAME(ffi_call_SYSV),.ffi_call_SYSV_end-CNAME(ffi_call_SYSV)
#endif
#if 0
	.section	.text..SHmedia32,"ax"

	# r2:	ffi_prep_args
	# r3:	&ecif
	# r4:	bytes
	# r5:	flags
	# r6:	flags2
	# r7:	rvalue
	# r8:	fn
	# r15:  %sp
	# r18:  %lp
	# r2-r9: %s0..%s7 parameter passing, caller save

	# This assumes we are using gas.
	.align	5
ENTRY(ffi_call_SYSV)
	# Save registers
.LFB1:
	addi.l	r15, -48, r15
.LCFI0:
	st.q	r15, 40, r32
	st.q	r15, 32, r31
	st.q	r15, 24, r30
	st.q	r15, 16, r29
	st.q	r15, 8, r28
	st.l	r15, 4, r18
	st.l	r15, 0, r14
.LCFI1:
	add.l	r15, r63, r14
.LCFI2:
#	add	r4, r63, r28
	add	r5, r63, r29
	add	r6, r63, r30
	add	r7, r63, r31
	add	r8, r63, r32

	addi	r4, (64 + 7), r4
	andi	r4, ~7, r4
	sub.l	r15, r4, r15

	ptabs/l	r2, tr0
	add	r15, r63, r2
	blink	tr0, r18

	addi	r15, 64, r22
	movi	0, r0
	movi	0, r1
	movi	-1, r23

	pt/l	1f, tr1
	bnei/l	r29, FFI_TYPE_STRUCT, tr1
	ld.l	r15, 0, r19
	addi	r15, 8, r15
	addi	r0, 1, r0
1:

.L_pass:
	andi	r30, 3, r20
	shlri	r30, 2, r30

	pt/l	.L_call_it, tr0
	pt/l	.L_pass_i, tr1
	pt/l	.L_pass_f, tr2

	beqi/l	r20, FFI_TYPE_VOID, tr0
	beqi/l	r20, FFI_TYPE_INT, tr1
	beqi/l	r20, FFI_TYPE_FLOAT, tr2

.L_pass_d:
	addi	r0, 1, r0
	pt/l	3f, tr0
	movi	12, r20
	bge/l	r1, r20, tr0

	pt/l	.L_pop_d, tr1
	pt/l	2f, tr0
	blink	tr1, r63
2:
	addi.l	r15, 8, r15
3:
	pt/l	.L_pass, tr0
	addi	r1, 2, r1
	blink	tr0, r63

.L_pop_d:
	pt/l	.L_pop_d_tbl, tr1
	gettr	tr1, r20
	shlli	r1, 2, r21
	add	r20, r21, r20
	ptabs/l	r20, tr1
	blink	tr1, r63

.L_pop_d_tbl:
	fld.d	r15, 0, dr0
	blink	tr0, r63
	fld.d	r15, 0, dr2
	blink	tr0, r63
	fld.d	r15, 0, dr4
	blink	tr0, r63
	fld.d	r15, 0, dr6
	blink	tr0, r63
	fld.d	r15, 0, dr8
	blink	tr0, r63
	fld.d	r15, 0, dr10
	blink	tr0, r63

.L_pass_f:
	addi	r0, 1, r0
	pt/l	3f, tr0
	movi	12, r20
	bge/l	r1, r20, tr0

	pt/l	.L_pop_f, tr1
	pt/l	2f, tr0
	blink	tr1, r63
2:
	addi.l	r15, 8, r15
3:
	pt/l	.L_pass, tr0
	blink	tr0, r63

.L_pop_f:
	pt/l	.L_pop_f_tbl, tr1
	pt/l	5f, tr2
	gettr	tr1, r20
	bge/l	r23, r63, tr2
	add	r1, r63, r23 
	shlli	r1, 3, r21
	addi	r1, 2, r1
	add	r20, r21, r20
	ptabs/l	r20, tr1
	blink	tr1, r63
5:
	addi	r23, 1, r21
	movi	-1, r23
	shlli	r21, 3, r21
	add	r20, r21, r20
	ptabs/l	r20, tr1
	blink	tr1, r63

.L_pop_f_tbl:
	fld.s	r15, OFS_FLT, fr0
	blink	tr0, r63
	fld.s	r15, OFS_FLT, fr1
	blink	tr0, r63
	fld.s	r15, OFS_FLT, fr2
	blink	tr0, r63
	fld.s	r15, OFS_FLT, fr3
	blink	tr0, r63
	fld.s	r15, OFS_FLT, fr4
	blink	tr0, r63
	fld.s	r15, OFS_FLT, fr5
	blink	tr0, r63
	fld.s	r15, OFS_FLT, fr6
	blink	tr0, r63
	fld.s	r15, OFS_FLT, fr7
	blink	tr0, r63
	fld.s	r15, OFS_FLT, fr8
	blink	tr0, r63
	fld.s	r15, OFS_FLT, fr9
	blink	tr0, r63
	fld.s	r15, OFS_FLT, fr10
	blink	tr0, r63
	fld.s	r15, OFS_FLT, fr11
	blink	tr0, r63

.L_pass_i:
	pt/l	3f, tr0
	movi	8, r20
	bge/l	r0, r20, tr0

	pt/l	.L_pop_i, tr1
	pt/l	2f, tr0
	blink	tr1, r63
2:
	addi.l	r15, 8, r15
3:
	pt/l	.L_pass, tr0
	addi	r0, 1, r0
	blink	tr0, r63

.L_pop_i:
	pt/l	.L_pop_i_tbl, tr1
	gettr	tr1, r20
	shlli	r0, 3, r21
	add	r20, r21, r20
	ptabs/l	r20, tr1
	blink	tr1, r63

.L_pop_i_tbl:
	ld.q	r15, 0, r2
	blink	tr0, r63
	ld.q	r15, 0, r3
	blink	tr0, r63
	ld.q	r15, 0, r4
	blink	tr0, r63
	ld.q	r15, 0, r5
	blink	tr0, r63
	ld.q	r15, 0, r6
	blink	tr0, r63
	ld.q	r15, 0, r7
	blink	tr0, r63
	ld.q	r15, 0, r8
	blink	tr0, r63
	ld.q	r15, 0, r9
	blink	tr0, r63

.L_call_it:
	# call function
	pt/l	1f, tr1
	bnei/l	r29, FFI_TYPE_STRUCT, tr1
	add	r19, r63, r2
1:
	add	r22, r63, r15
	ptabs/l	r32, tr0
	blink	tr0, r18

	pt/l	.L_ret_i, tr0
	pt/l	.L_ret_ll, tr1
	pt/l	.L_ret_d, tr2
	pt/l	.L_ret_f, tr3
	pt/l	.L_epilogue, tr4

	beqi/l	r29, FFI_TYPE_INT, tr0
	beqi/l	r29, FFI_TYPE_UINT32, tr0
	beqi/l	r29, FFI_TYPE_SINT64, tr1
	beqi/l	r29, FFI_TYPE_UINT64, tr1
	beqi/l	r29, FFI_TYPE_DOUBLE, tr2
	beqi/l	r29, FFI_TYPE_FLOAT, tr3

	pt/l	.L_ret_q, tr0
	pt/l	.L_ret_h, tr1

	beqi/l	r29, FFI_TYPE_UINT8, tr0
	beqi/l	r29, FFI_TYPE_UINT16, tr1
	blink	tr4, r63

.L_ret_d:
	fst.d	r31, 0, dr0
	blink	tr4, r63

.L_ret_ll:
	st.q	r31, 0, r2
	blink	tr4, r63

.L_ret_f:
	fst.s	r31, OFS_FLT, fr0
	blink	tr4, r63

.L_ret_q:
	st.b	r31, 0, r2
	blink	tr4, r63

.L_ret_h:
	st.w	r31, 0, r2
	blink	tr4, r63

.L_ret_i:
	st.l	r31, 0, r2
	# Fall

.L_epilogue:
	# Remove the space we pushed for the args
	add	r14, r63, r15

	ld.l	r15, 0, r14
	ld.l	r15, 4, r18
	ld.q	r15, 8, r28
	ld.q	r15, 16, r29
	ld.q	r15, 24, r30
	ld.q	r15, 32, r31
	ld.q	r15, 40, r32
	addi.l	r15, 48, r15
	ptabs	r18, tr0
	blink	tr0, r63

.LFE1:
.ffi_call_SYSV_end:
	.size	 CNAME(ffi_call_SYSV),.ffi_call_SYSV_end-CNAME(ffi_call_SYSV)

	.align	5
ENTRY(ffi_closure_SYSV)
.LFB2:
	addi.l	r15, -136, r15
.LCFI3:
	st.l	r15, 12, r18
	st.l	r15, 8, r14
	st.l	r15, 4, r12
.LCFI4:
	add	r15, r63, r14
.LCFI5:
	/* Stack layout:	
	   ...
	   64 bytes (register parameters)
	   48 bytes (floating register parameters)
	    8 bytes (result)
	    4 bytes (r18)
	    4 bytes (r14)
	    4 bytes (r12)
	    4 bytes (for align)
	   <- new stack pointer
	*/
	fst.d	r14, 24, dr0
	fst.d	r14, 32, dr2
	fst.d	r14, 40, dr4
	fst.d	r14, 48, dr6
	fst.d	r14, 56, dr8
	fst.d	r14, 64, dr10
	st.q	r14, 72, r2
	st.q	r14, 80, r3
	st.q	r14, 88, r4
	st.q	r14, 96, r5
	st.q	r14, 104, r6
	st.q	r14, 112, r7
	st.q	r14, 120, r8
	st.q	r14, 128, r9

	add	r1, r63, r2
	addi	r14, 16, r3
	addi	r14, 72, r4
	addi	r14, 24, r5
	addi	r14, 136, r6
#ifdef PIC
	movi	(((datalabel _GLOBAL_OFFSET_TABLE_-(.LPCS0-.)) >> 16) & 65535), r12
        shori	((datalabel _GLOBAL_OFFSET_TABLE_-(.LPCS0-.)) & 65535), r12
.LPCS0:	ptrel/u r12, tr0
	movi	((ffi_closure_helper_SYSV@GOTPLT) & 65535), r1
	gettr	tr0, r12
	ldx.l	r1, r12, r1
	ptabs	r1, tr0
#else
	pt/l	ffi_closure_helper_SYSV, tr0
#endif
	blink	tr0, r18

	shlli	r2, 1, r1
        movi    (((datalabel .L_table) >> 16) & 65535), r2
        shori   ((datalabel .L_table) & 65535), r2
        ldx.w   r2, r1, r1
        add     r1, r2, r1
	pt/l	.L_case_v, tr1
        ptabs   r1, tr0
        blink   tr0, r63

        .align 2
.L_table:
	.word	.L_case_v - datalabel .L_table	/* FFI_TYPE_VOID */
	.word	.L_case_i - datalabel .L_table	/* FFI_TYPE_INT */
	.word	.L_case_f - datalabel .L_table	/* FFI_TYPE_FLOAT */
	.word	.L_case_d - datalabel .L_table	/* FFI_TYPE_DOUBLE */
	.word	.L_case_d - datalabel .L_table	/* FFI_TYPE_LONGDOUBLE */
	.word	.L_case_uq - datalabel .L_table	/* FFI_TYPE_UINT8 */
	.word	.L_case_q - datalabel .L_table	/* FFI_TYPE_SINT8 */
	.word	.L_case_uh - datalabel .L_table	/* FFI_TYPE_UINT16 */
	.word	.L_case_h - datalabel .L_table	/* FFI_TYPE_SINT16 */
	.word	.L_case_i - datalabel .L_table	/* FFI_TYPE_UINT32 */
	.word	.L_case_i - datalabel .L_table	/* FFI_TYPE_SINT32 */
	.word	.L_case_ll - datalabel .L_table	/* FFI_TYPE_UINT64 */
	.word	.L_case_ll - datalabel .L_table	/* FFI_TYPE_SINT64 */
	.word	.L_case_v - datalabel .L_table	/* FFI_TYPE_STRUCT */
	.word	.L_case_i - datalabel .L_table	/* FFI_TYPE_POINTER */

        .align 2
.L_case_d:
	fld.d	r14, 16, dr0
	blink	tr1, r63
.L_case_f:
	fld.s	r14, 16, fr0
	blink	tr1, r63
.L_case_ll:
	ld.q	r14, 16, r2
	blink	tr1, r63
.L_case_i:
	ld.l	r14, 16, r2
	blink	tr1, r63
.L_case_q:
	ld.b	r14, 16, r2
	blink	tr1, r63
.L_case_uq:
	ld.ub	r14, 16, r2
	blink	tr1, r63
.L_case_h:
	ld.w	r14, 16, r2
	blink	tr1, r63
.L_case_uh:
	ld.uw	r14, 16, r2
	blink	tr1, r63
.L_case_v:
	add.l	r14, r63, r15
	ld.l	r15, 4, r12
	ld.l	r15, 8, r14
	ld.l	r15, 12, r18
	addi.l	r15, 136, r15
	ptabs	r18, tr0
	blink	tr0, r63

.LFE2:
.ffi_closure_SYSV_end:
	.size	 CNAME(ffi_closure_SYSV),.ffi_closure_SYSV_end-CNAME(ffi_closure_SYSV)

#if defined __ELF__ && defined __linux__
	.section	.note.GNU-stack,"",@progbits
#endif

	.section	".eh_frame","aw",@progbits
__FRAME_BEGIN__:
	.4byte	.LECIE1-.LSCIE1	/* Length of Common Information Entry */
.LSCIE1:
	.4byte	0x0	/* CIE Identifier Tag */
	.byte	0x1	/* CIE Version */
#ifdef PIC
	.ascii "zR\0"	/* CIE Augmentation */
#else
	.byte	0x0	/* CIE Augmentation */
#endif
	.uleb128 0x1	/* CIE Code Alignment Factor */
	.sleb128 -4	/* CIE Data Alignment Factor */
	.byte	0x12	/* CIE RA Column */
#ifdef PIC
	.uleb128 0x1	/* Augmentation size */
	.byte	0x10	/* FDE Encoding (pcrel) */
#endif
	.byte	0xc	/* DW_CFA_def_cfa */
	.uleb128 0xf
	.uleb128 0x0
	.align	2
.LECIE1:
.LSFDE1:
	.4byte	datalabel .LEFDE1-datalabel .LASFDE1	/* FDE Length */
.LASFDE1:
	.4byte	datalabel .LASFDE1-datalabel __FRAME_BEGIN__
#ifdef PIC
	.4byte	.LFB1-.	/* FDE initial location */
#else
	.4byte	.LFB1	/* FDE initial location */
#endif
	.4byte	datalabel .LFE1-datalabel .LFB1	/* FDE address range */
#ifdef PIC
	.uleb128 0x0	/* Augmentation size */
#endif
	.byte	0x4	/* DW_CFA_advance_loc4 */
	.4byte	datalabel .LCFI0-datalabel .LFB1
	.byte	0xe	/* DW_CFA_def_cfa_offset */
	.uleb128 0x30
	.byte	0x4	/* DW_CFA_advance_loc4 */
	.4byte	datalabel .LCFI1-datalabel .LCFI0
	.byte   0x8e	/* DW_CFA_offset, column 0xe */
	.uleb128 0xc
	.byte   0x92	/* DW_CFA_offset, column 0x12 */
	.uleb128 0xb
	.byte   0x9c	/* DW_CFA_offset, column 0x1c */
	.uleb128 0xa
	.byte   0x9d	/* DW_CFA_offset, column 0x1d */
	.uleb128 0x8
	.byte   0x9e	/* DW_CFA_offset, column 0x1e */
	.uleb128 0x6
	.byte   0x9f	/* DW_CFA_offset, column 0x1f */
	.uleb128 0x4
	.byte   0xa0	/* DW_CFA_offset, column 0x20 */
	.uleb128 0x2
	.byte	0x4	/* DW_CFA_advance_loc4 */
	.4byte	datalabel .LCFI2-datalabel .LCFI1
	.byte	0xd	/* DW_CFA_def_cfa_register */
	.uleb128 0xe
	.align	2
.LEFDE1:

.LSFDE3:
	.4byte	datalabel .LEFDE3-datalabel .LASFDE3	/* FDE Length */
.LASFDE3:
	.4byte	datalabel .LASFDE3-datalabel __FRAME_BEGIN__
#ifdef PIC
	.4byte	.LFB2-.	/* FDE initial location */
#else
	.4byte	.LFB2	/* FDE initial location */
#endif
	.4byte	datalabel .LFE2-datalabel .LFB2	/* FDE address range */
#ifdef PIC
	.uleb128 0x0	/* Augmentation size */
#endif
	.byte	0x4	/* DW_CFA_advance_loc4 */
	.4byte	datalabel .LCFI3-datalabel .LFB2
	.byte	0xe	/* DW_CFA_def_cfa_offset */
	.uleb128 0x88
	.byte	0x4	/* DW_CFA_advance_loc4 */
	.4byte	datalabel .LCFI4-datalabel .LCFI3
	.byte   0x8c	/* DW_CFA_offset, column 0xc */
	.uleb128 0x21
	.byte   0x8e	/* DW_CFA_offset, column 0xe */
	.uleb128 0x20
	.byte   0x92	/* DW_CFA_offset, column 0x12 */
	.uleb128 0x1f
	.byte	0x4	/* DW_CFA_advance_loc4 */
	.4byte	datalabel .LCFI5-datalabel .LCFI4
	.byte	0xd	/* DW_CFA_def_cfa_register */
	.uleb128 0xe
	.align	2
.LEFDE3:
#endif
