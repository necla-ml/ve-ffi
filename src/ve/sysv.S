/* vim: set sw=12 ts=12 noet */
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
#include <ffi.h>		/* pulls in ve/fffitarget.h */

#if defined(__PIC__)
#warning "VE sysv.S -fPIC ?"
#else
#warning "VE sysv.S no PIC ?"
#endif

#ifndef VE_SYSV_DEBUG /* define in to ve/ffitarget.h, incl via ffi.h */
#define VE_SYSV_DEBUG 1
#endif

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
	/* I think I prefer .macro ... Wish I had docs, though.  "Write and hope" here... */
	/* check with -Wa,-m for macro expansion results (see Makefile for nas example) */
.macro addr_s12 LABEL			# clobbers s12
	lea	%s12, \LABEL@PLT_LO(-24)
	and	%s12,%s12,(32)0
	sic	%lr
	lea.sl	%s12,\LABEL@PLT_HI(%s12,%lr)
.endm
#else
#define TXT_ADDRESS( REG, LABEL ) \
	lea	%s12, LABEL@LO; \
	and	%s12,%s12,(32)0; \
	lea.sl	%s12,LABEL@HI(,%s12); \
	or	REG,%s12,(0)1
.macro addr_s12 LABEL:req
	lea	%s12, \LABEL@LO
	and	%s12,%s12,(32)0
	lea.sl	%s12,\LABEL@HI(,%s12)
.endm
#endif
#define DEFINE_STRING( LABEL,STRING ) \
.section .rodata; \
	.balign 8; \
LABEL:	.asciz STRING; \
	.balign 8; \
	.text

.macro prt_str STRING # clobbers %s12,%s42
	.section .rodata
	.local	Lprt_str\@
Lprt_str\@:	.asciz	"\STRING"
	.balign 	8
	.text
	or	%s42,0,%s0	# spill
	addr_s12 	Lprt_str\@
	or	%s0,0,%s12
	addr_s12	puts
	bsic	%lr,(,%s12)
	or	%s0,0,%s42	# restore
.endm
.macro swap_reg A,B	# A,B MUST be different registers %s0..%s63
	xor	\A,\A,\B
	xor	\B,\A,\A
	xor	\A,\A,\B
.endm


#if 0 /* can I set up a macro to cycle through a set of tmp registers? */
	/* I probably need assembler macro documentation to succeed! */
	/* to do fancy stuff like symbolic reg names, probably easiest
		to switch to m4 */
	.set	TMP_CTR, 40
.altmacro
.macro SCALAR_REG N
	.set	REG,%s\N
.endm
.noaltmacro
.macro TMP_REG_SET CTR
	.set	TMP_REG, %s\CTR
.endm
#define PRIMITVE_CAT(a,...) a ## __VA_ARGS__
#define CAT(a,...) PRIMITIVE_CAT(a, __VA_ARGS__)

#define IIF(c) PRIMITIVE_CAT(IIF_, c)
#define IIF_0(t, ...) __VA_ARGS__
#define IIF_1(t, ...) t

#define INC(X) PRIMITIVE_CAT(INC_,x)
#define INC_0 1
#define INC_1 2
#define INC_2 0
#define NXT() 
	# JUNK .set asdf, INC(0)

#define S(CNT) %s##CNT
#define EVAL(EXPR) EXPR
#define MOD(N,M) EVAL(N%M)
#define NEXT_CYC(n) PRIMITIVE_CAT(CYC_,n)
#define CYC_0 %s40
#define CYC_1 %s41
#define CYC_2 %s42
#define CYC_3 $s40


#define VALUE_TO_STRING(...) #__VA_ARGS__
#define VALUE(...) VALUE_TO_STRING(__VA_ARGS__)
#define VAR_NAME_VALUE(var) #var " = " VALUE(var)
#pragma message  VAR_NAME_VALUE( MOD(5,2) )

#define CHECK_N(x,n,...) n
#define CHECK(...) CHECK_N(__VA_ARGS__, 0,)
#define PROBE(x) x, 1, // CHECK(PROBE(~)) expands to 1, but CHECK(xxx) [single-token] expands to 0
#define PROBE_EQU_42(x) x, 42

.altmacro
.macro TMP_REG_NAME name,ctr
	.set	\name,\ctr		# OK
	#.set	\name&,%s\ctr# BAD
	#.set	\name,\%s\ctr# BAD
	#.set	\name,"%s\ctr"# BAD
	#.set	\name,S(\ctr)	# BAD
.endm
.macro TMP_REG_NAME3 name,reg,ctr
	.set \name,\reg\ctr
.endm
.macro next_tmp, name="last_tmp"
	.set	TMP_CTR, TMP_CTR+1
	.ifge	TMP_CTR - 43
	.set	TMP_CTR, 40
	.endif
	.set	name,%
.endm
	.eqv	RTMP, TMP_REG

.text
.align 16
.globl foo_bar
foo_bar:
	TMP_REG_NAME fu1,40
	adds.l %s0,fu1,(0)1

	TMP_REG_NAME fu2,"%s40"

	#TMP_REG_NAME3 fu2,"%s",40
	#adds.l %s0,fu2,%s0

	adds.l	%s0,%s40,%s0
	#SCALAR_REG %40
	.set	TMP_REG,s40
	adds.l	%s0,%TMP_REG,%s0

	.set FUFU0,"hello" # OK
	.set FUFU1,hello #OK
	# Bad expression .set FUFU3,%s40
	#adds.l %s40,%s40,%s40
	#adds.l FUFU3,FUFU3,FUFU3
	#TMP_REG_SET TMP_CTR
	#and	RTMP,0,(1)0
	#next_tmp
	#TMP_REG_SET	(TMP_CTR)
	#and	RTMP,0,(1)0
#endif
#if 0 /* test code ... */
foo_bar:
	prt_str		"Hello "
	prt_str		"World\n"
	swap_reg %s0,%s1
	swap_reg %s0,%s1
#endif
	
	#CPRINTF( puts, ptr_bogus, "", nop ) # puts auto-supplies the crlf

#if VE_SYSV_DEBUG > 0			/* NB: these might trash %s63 and %s0..%s2 */
#define ASMDBG( STUFF... ) STUFF
#define SYSV_SET_DEBUG_REGS \
	TXT_ADDRESS( %s27, print_s0 ); \
	TXT_ADDRESS( %s28, print_msg_u64 ); \
	TXT_ADDRESS( %s29, print_msg_float ); \
	TXT_ADDRESS( %s30, print_msg_double )
/* NB: PRINT_MSG invokes a function, so regs (like %s0..%s7) may be clobbered */
#define PRINT_MSG_U64( NAME, LOAD_S1... ) \
	LOAD_S1; \
	DEFINE_STRING( sysv_str_##NAME, #NAME ); \
	TXT_ADDRESS( %s0, sysv_str_##NAME ); \
	TXT_ADDRESS( %s12, print_msg_u64 ); \
	bsic	%lr,(,%s12); \
	/*bsic	%lr,(,%s28)*/
#define PRINT_MSG_U64X( NAME, XSTRING, LOAD_S1... ) \
	LOAD_S1; \
	DEFINE_STRING( sysv_str_##NAME, XSTRING ); \
	TXT_ADDRESS( %s0, sysv_str_##NAME ); \
	bsic	%lr,(,%s28)
#define PRINT_MSG_FLOATX( NAME, XSTRING, LOAD_S1... ) \
	LOAD_S1; \
	DEFINE_STRING( sysv_str_##NAME, XSTRING ); \
	TXT_ADDRESS( %s0, sysv_str_##NAME ); \
	bsic	%lr,(,%s29)
#define CPRINTF( CFUNC, NAME, XSTRING, LOAD_S1... ) \
	DEFINE_STRING( sysv_str_##NAME, XSTRING ); \
	TXT_ADDRESS( %s0, sysv_str_##NAME ); \
	LOAD_S1; \
	TXT_ADDRESS( %s63, CFUNC ); \
	bsic	%lr,(,%s63)
#else
#define ASMDBG( STUFF... )
#define SYSV_SET_DEBUG_REGS
#define PRINT_MSG_U64( NAME, LOAD_S1... )
#define PRINT_MSG_U64X( NAME, XSTRING, LOAD_S1... )
#define PRINT_MSG_FLOATX( NAME, XSTRING, LOAD_S1... )
#define CPRINTF( CFUNC, NAME, XSTRING, LOAD_S1... )
#endif


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

.balign 16
.globl ffi_call_SYSV
ffi_call_SYSV:
	;#stack frame .cfi directives are for nice stack debug
	/* [opt] libffi has macros for .cfi stuff */
	.cfi_startproc
	st	%fp,0x0(,%sp)
	.cfi_def_cfa_offset 0
	.cfi_offset	9,0		# %fp is %s9
	st	%lr,0x8(,%sp)		# %lr~%s10 Link Reg: ret addr
	;				# skip %tp at 0x10(,%sp)
	st	%got,0x18(,%sp)
	st	%plt,0x20(,%sp)
#ifdef PIC
	lea	%got,_GLOBAL_OFFSET_TABLE_@PC_LO(-24) 	#-0x18
	and	%got,%got,(32)0
	sic	%plt			# %plt := IC + 8
	lea.sl	%got,_GLOBAL_OFFSET_TABLE_@PC_HI(%plt,%got)
#endif

	or	%fp,0,%sp		# %fp := %sp
	.cfi_def_cfa_register	9	# %s9 is the frame points
	S18_TO_33( st, %fp )		# %s18-%s33 Callee-saved

	/* call to ffi_prep_args can clobber s0..s7, so save rvalue and fn */
sysv_save:
	or	%s18,%s0,(0)1	# ffi_prep_args
	or	%s19,%s1,(0)1	# &ecif
	or	%s20,%s2,(0)1	# cif->bytes
	or	%s21,%s3,(0)1	# flags
	or	%s22,%s4,(0)1	# flags2
	or	%s23,%s5,(0)1	# rvalue
	or	%s24,%s6,(0)1	# fn
	SYSV_SET_DEBUG_REGS

	/* sysv.S only needs 4 8byte slots for temporaries, I think.
	   and probably I can even put those into s18-s33 instead. */
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

/* 1 : call prep_args
   2 : invoke function
   3 : return value
*/
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

;# s0/18:ffi_prep_args s1/19:&ecif s2/20:bytes s3/21:flags s4/22:flags2 s5/23:rvalue s6/24:fn

sysv_invoke:
	/* invoke function ........................ in CURRENT stack frame */
	/* load s0..s7 regs and execute call to fn (%s6-->%s24) */
	/* How?  A simple [slow] way is to set aside a memory area
	         and blind-copy into s0..s7. */
	#PRINT_MSG_U64(stack, lea %s0,176(,%sp));
	lea	%s0,176(,%sp)	#	%s0 = char *stack, 1st in Parameter Area for callee
	adds.l	%s1,%s20,(58)1	#	cif->bytes - 64 (isn't there a better syntax here?)
#if VE_SYSV_DEBUG > 0 /* debug: need spill register */
	adds.l	%s33,%s0,%s1	# %s33 : char* stkreg = stack + cif->bytes - 64
	PRINT_MSG_U64X(s33_stkreg, "s33_stkreg", or %s1,0,%s33 )
	CPRINTF( print_ptr, ptr_s33_stkreg, " s33_stkreg@", or %s1,0,%s33 )
	CPRINTF( puts, ptr_bogus, "", nop ) # puts auto-supplies the crlf
	or	%s63,0,%s33
#else /* no debug */
	adds.l	%s63,%s0,%s1	# %s33 : char* stkreg = stack + cif->bytes - 64
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
	bsic	%lr,(,%s12)		# retval (if rvalue!=NULL) is in %s0
	st	%s0,-4*8(,%fp)	# spill retval [which couldl be up to 4 registers long]
	st	%s1,-3*8(,%fp)
	st	%s2,-2*8(,%fp)
	st	%s3,-1*8(,%fp)

sysv_dbg:
	PRINT_MSG_U64(s0_retval,       or %s1,0,%s0)
	PRINT_MSG_U64(s1_retval,       ld %s1,-3*8(,%fp))
	PRINT_MSG_U64(s23_rvalue_ptr,  or %s1,0,%s23)
	
sysv_retval:
	/* handle return value ............................. */
	/* or	%s0,0,return_value */
	;# (*rvalue) = %s0
	PRINT_MSG_U64(s21_flags,  or %s1,0,%s21) # %s21 = flags == ecif->cif->rtype->type
	breq.l.nt	%s23,0,.L_BoE	# rvalue == NULL? skip rvalue
	breq.w.t	FFI_TYPE_VOID,%s21, sysv_ret2
.L1.notvoid:
	ld	%s0,-4*8(,%fp)	# retrieve retval [opt]
	ld	%s1,-3*8(,%fp)
	ld	%s2,-2*8(,%fp)
	ld	%s3,-1*8(,%fp)
	brne.w.t	FFI_TYPE_FLOAT,%s21, .L1.notfloat
sysv_rfloat:
	PRINT_MSG_FLOATX( sysv_rfloat, "rfloat", ld %s0,-4*8(,%fp) )
	ld	%s0,-4*8(,%fp)	# retrieve retval
	stu	%s0,0(,%s23)	# SPECIAL register-float ---> 4-byte float memory
	br.w	.L_BoE
.L1.notfloat:
	brne.w.t	FFI_TYPE_LONGDOUBLE,%s21,.L1.notldouble
sysv_rldouble: /* %s0 <---> higher mem address */
	ld	%s0,-4*8(8,%fp)	# retrieve retval
	ld	%s1,-4*8(,%fp)
	st	%s0,0(,%s23)
	st	%s1,8(,%s23)
	br.w	.L_BoE
.L1.notldouble:
	brne.w.t	FFI_TYPE_COMPLEX,%s21,.L1.notcomplex
sysv_rcomplex: # ?? complex ... ?
/*
	BUG: when enabling printout, I lose float complex correctness ???
*/
#if 1 /* differentiate the 3 complex types by rtype->size == 8, 16, or 32 */
	CPRINTF( puts, sysv_rcomplex, "sysv_rcomplex", nop ) # puts auto-supplies the crlf
	/* first, find the size of this particular complex return type */
	or	%s33,0,%s19	# ecif (ptr)
	CPRINTF( print_ptr, ptr_ecif, "\n\tsysv.S rcomplex: ecif@", or %s1,0,%s33 )
	ld	%s32,0(0,%s33)	# ecif->cif, OK
	CPRINTF( print_ptr, ptr_cif, " cif@", or %s1,0,%s32 )
	ld	%s33,16(0,%s32)	# ecif->cif->rtype
	CPRINTF( print_ptr, ptr_rtype, " rtype@", or %s1,0,%s33 )
	lea	%s32,0(,%s33)	# %s32 --> return type structure
	CPRINTF( print_u64, complex_size, " .sz=", ld %s1,0(,%s32) )
	CPRINTF( print_u64, complex_align, " .al=", ld2b.zx %s1,0(8,%s32) )
	CPRINTF( print_u64, complex_type, " .ty=", ld2b.zx %s1,0(10,%s32) )
#else /* quiet */
	or	%s33,0,%s19	# ecif (ptr)
	ld	%s32,0(0,%s33)	# ecif->cif, OK
	ld	%s33,16(0,%s32)	# ecif->cif->rtype
	lea	%s32,0(,%s33)	# %s32 --> return type structure
#endif
	
	ld	%s0,0(,%s32)	# retrieve size of the complex type
	/* 3 cases, differentiated by size/ alignment ??? */
	brne.l.t	8, %s0 ,.L1.notfloatcomplex
	CPRINTF( print_u64, rfloatcomplex, "rfloatcomplex stu %s0,1, sz=", ld2b.zx %s1,0(10,%s32) )
	/* store in rvalue as packed float complex[2] : [z8,a4] */
	ld	%s0,-4*8(,%fp)	# retrieve retval
	ld	%s1,-3*8(,%fp)
	stu	%s0,0(,%s23)	# in mem as float complex[s8:a4]
	stu	%s1,0(4,%s23)
	br.w	.L_BoE
.L1.notfloatcomplex:
	brne.l.t	16,%s0,.L1.notdoublecomplex
	//CPRINTF( puts, rdoublecomplex, "rdoublecomplex st %s0,1", nop ) # puts auto-supplies the crlf
	#CPRINTF( print_u64, rdoublecomplex, "rdoublecomplex stu %s0,1, sz=", ld2b.zx %s1,0(10,%s32) )
	ld	%s0,-4*8(,%fp)	# retrieve retval
	ld	%s1,-3*8(,%fp)
	st	%s0,0(,%s23)
	st	%s1,0(8,%s23)
	br.w	.L_BoE
.L1.notdoublecomplex: /* long double complex */
	#CPRINTF( puts, rldoublecomplex, "rldoublecomplex st %s0,1", nop ) # puts auto-supplies the crlf
	ld	%s0,-4*8(,%fp)	# retrieve retval [opt]
	ld	%s1,-3*8(,%fp)
	ld	%s2,-2*8(,%fp)
	ld	%s3,-1*8(,%fp)
	st	%s1,0(,%s23)	# invert 0/1, 2/3 ?
	st	%s0,0(8,%s23)
	st	%s3,0(16,%s23)
	st	%s2,0(24,%s23)
.L1.notcomplex:

#if VE_SYSV_DEBUG > 0 /* sysv_rstruct is a no-op !!! */
	brne.w.t	FFI_TYPE_STRUCT,%s21,.L1.notstruct
sysv_rstruct:	/* debug symbol [for gdb] */
	/* Note: unlike the compiler-generated code (at -O0 or -O1 or -O2 I think),
		which does a double struct copy,

		I pass along the "hidden arg 0" ptr directly to 'fn'

		This means I do NOT need to deal with the struct-copy here
		(I hope) ... and FFI_TYPE_STRUCT return values can be
		handled just like any other pointer-return */
	PRINT_MSG_U64(rstruct, ld %s0,-4*8(,%fp))	# retrieve retval = addr with content
	br.w	sysv_ret1
.L1.notstruct:
#else	/* NB we do not need to store anything into *rvalue */
	breq.w.t	FFI_TYPE_STRUCT,%s21,sysv_ret1
#endif

	brne.w.t	FFI_TYPE_POINTER,%s21,.L1.notpointer
sysv_rpointer:
	PRINT_MSG_U64( rpointer,  ld %s0,-4*8(,%fp))	# %s0 = void* ret
	ld	%s0,-4*8(,%fp)	# %s0 = void* ret
	st	%s0,0(,%s23)	# (*rvalue) = ret
	br.w	.L_BoE
.L1.notpointer:
	brne.w.t	FFI_TYPE_DOUBLE,%s21,.L1.notdouble
sysv_rdouble:
	/* identical to default case, except for possible debug */
	ld	%s0,-4*8(,%fp)	# %s0 = void* ret
	st	%s0,0(,%s23)	# (*rvalue) = ret
	br.w	.L_BoE
.L1.notdouble:


	/* #################### DEFAULT HANDLING #################### */
	/* <= 8-byte scalar types [except FLOAT] can use this         */
	PRINT_MSG_U64X( u64_rvalue, "*rvalue", ld %s1,-4*8(,%fp))
	ld	%s0,-4*8(,%fp)	# retrieve retval (1 reg)
	st	%s0,0(,%s23)	# (*rvalue) = %s0
	br.w	.L_BoE # reg return value already in place
	/* ########################################################## */


sysv_ret4:
	ld	%s3,-1*8(,%fp)
	ld	%s2,-2*8(,%fp)
sysv_ret2:
	ld	%s1,-3*8(,%fp)
sysv_ret1:
	ld	%s0,-4*8(,%fp)	# retrieve retval [change order if incorrect for multibyte]

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
	.size	ffi_call_SYSV, . - ffi_call_SYSV
/*
# This is invoked via a "trampoline", which is a runtime
# generated set of instructions.  The trampoline passes us
# 1. trampoline address in %s0
# 2. [if REFERENCE return] ptr to return content %s1
# The trampoline executes a jump [branch] instruction
# to ffi_closure_ve  [or ffi_closure_ve_struct]
*/
#define CLOSURE_FS 8 /*closure frame size*/
/* adapting x86/ffi64.c
 * closure is the WRITABLE address returned by ffi_closure_alloc
 *   [ this is what we have to set up in this call ]
 * fun( ffi_cif* cif, void *ret, void **args, void *user_data )
 *   is the function called by the closure.
 * user_data is passed, uninterpreted, to your closure function.
 * codeloc is the READ+EXEC pointer to *closure -- unused here
 *   [ user casts this to appropriate fn-call type ]
 */
/*
 * Where do I store %lr  (closure return address)
 *
 * closure("Hi")
 * ... Reg/Ref args [%s0<--"Hi",]
 * ... %s12 <-- addr of trampoline
 * ... bsic %lr,(,%s12)
 * --> trampoline executes!
 * _______________________________
 *   --> ffi_closure_ve (sysv.S)
 * operating IN CALLER STACK FRAME
 * fp -------   alloca -----------
 *    |          32 (rval)
 *    |         +64 (reg_args)
 *    |         +48 (*_inner parms)  |*_inner (ffi.c)
 *    |         ---                  |( ffi_cif *cif,
 *    |  alloca(144)                 |  void(*fun)(ffi_cif*,void*ret,
 *    |                              |     void**args,void*user_data),
 *    |                              |  void *user_data,
 *    |                              |  void*rvalue,
 *    |                              |  struct register_args *reg_args,
 *    |                              |  char *argp );
 *    |------------------------------|__________________
 *    |%s0:"Hi"(mem unset)           |
 * 176|--------+-- argp -------------| argp
 *    |(RSA)    \   retaddr+flg[16]  | (for ffi_closure_ve)
 *    |(retaddr) \  rvalue[32]       | rvalue
 *   0|(calleefp) \ reg_args[64]     | reg_args
 *  sp|--------    \_inner args[6*8] | %s0..%s5
 *              176\------------------------------
 *                 |(RSA)            | *_inner
 *                 |(retaddr)        | prologue
 *                0|(calleefp)       | sets these    | *_inner
 *               sp|----------//   fp------------    | now invokes
 *                                   | locals        | (*fun)     
 *                                   |-----------    |_____________
 *                                   | alloca Parm   | fun args &
 *                                   | Area of 'fun' | regs set up
 *                                   | args-->"Hi",  | by *_inner
 *                                   | etc.          |
 *                             sp+176|-----------    |-------------
 *                                   |(RSA)          | fun prologue
 *                                   |(retaddr,fp)   | sets these
 *                                 sp|-----------  fp|-------------
 *                              _________________    |
 *                             /  Unwind: ret   |    | etc.
 *                            /    rvalue? flags|  sp|-------------
 *  _________________________/
 *    Unwind: rvalue        
 *            -> reg        
 */
/* Closure Frame: size and offsets
 *   0. sp..sp+176 reserved [not yet set!] for fp, ret addr, and RSA
 *      *_inner WILL push %fp,%lr,%got,%plt,etc onto stack during prologue
 *   1. alloca for:
 *      a. return addr (+ 8-byte align, so %sp has 16-byte alignment)
 *      b. space for rvalue (4 * 8byte)
 *      c. store all possible reg args of 'fun' (8 * 8byte)
 *      d. space for %s0..%s5 args to *_inner (6 * 8byte)
 *         [even for REGISTER args, uninitialized arg area is given]
 *   We don't want the overhead of full stack frame creation.
 *   ffi closure offsets are wrt our new %sp -= ffi_closure_ALLOCA
 */
#if 0 // nas does not lot like expressions very much
#define ffi_closure_OFS_PARMS	176
#define ffi_closure_OFS_G       (ffi_closure_OFS_PARMS + 6*8)
#define ffi_closure_OFS_RVALUE  (ffi_closure_OFS_G + 8*8)
#define ffi_closure_OFS_RETADDR (ffi_closure_OFS_RVALUE + 4*8)
#define ffi_closure_OFS_ARGP    (ffi_closure_OFS_RETADDR + 8 + 8/*flg*/)
#define ffi_closure_ALLOCA      (ffi_closure_OFS_ARGP - ffi_closure_OFS_G)
#else
.set ffi_closure_OFS_PARMS   , 176
.set ffi_closure_OFS_G       , (ffi_closure_OFS_PARMS + 6*8)
.set ffi_closure_OFS_RVALUE  , (ffi_closure_OFS_G + 8*8)
.set ffi_closure_OFS_RETADDR , (ffi_closure_OFS_RVALUE + 4*8)
.set ffi_closure_OFS_RTYPE   , (ffi_closure_OFS_RETADDR + 8)
.set ffi_closure_OFS_ARGP    , (ffi_closure_OFS_RETADDR + 8 + 8/*flags*/)
#.set ffi_closure_ALLOCA      , (ffi_closure_OFS_ARGP - ffi_closure_OFS_G)
/* 48 + 64 + 32 + 8+8 = 160 bytes for alloca */
#define ffi_closure_ALLOCA 160
#endif

/* -ve offset of rvalue within the red zone after deallocating the frame.  */
#define ffi_closure_RED_RVALUE  (ffi_closure_OFS_RVALUE - ffi_closure_FS)

.globl ffi_closure_ve_struct
.globl ffi_closure_ve
.balign 16
ffi_closure_ve_struct:
	or	%s60,1,(1)0
	br.w	.L_ffi_closure_s60
.balign 16
ffi_closure_ve:
	or	%s60,0,(1)0
	# %s60 flags our entry-point
.L_ffi_closure_s60:
	.cfi_startproc
#if 0 /* set up a real stack frame --- arg offset would all change !! */
	st %fp,0x0(,%sp)
	.cfi_def_cfa_offfset 0
	.cfi_offfset	9,0
	st %lr, 0x8(,%sp)
	or %fp,0,%sp
#endif

	/* We are still in the caller's stack-frame.
	 * Parameter area for callee is at 176(%fp).
	 * For Aurora this begins with space for REGISTER args.
	 *
	 * We are preparing to call a function of form
	 * void (*fun)(ffi_cif*cif, void*ret, void**args, void*user_data),
	 *
	 * But first we need to invoke ffi_closure_ve_inner (ffi.c)
	 * Where do we stash register args in the meantime?
	 * We do something like alloca would do, and stash all possible
	 * arg-registers for later use.
	 * ffi_closure_unix64_inner(ffi_cif *cif,
	 *		 void (*fun)(ffi_cif*, void*, void**, void*),
	 *		 void *user_data,
	 *		 void *rvalue,
	 *		 struct register_args *reg_args,
	 *		 char *argp)
	 */
.L_closure_alloca:
	lea	%sp, -ffi_closure_ALLOCA(,%sp)		# as Data: sext(D,64)
	brge.l.t	%sp,%sl,.L_closure_space	# s63 >= stack limit[long,taken]?
	# readjust stack limit?
	ld	%s61,0x18(,%tp)	# param area: Host Memory ptr
	or	%s62,0,%s0	# spill value of %s0
	lea	%s63,0x13b	# syscall "grow" 
	shm.l	%s63,0x0(%s61)	# StoreHostMem addr:0
	shm.l	%sl,0x8(%s61)	# old limit at addr:8
	shm.l	%sp,0x10(%s61)	# new limit at addr:16
	monc			# system function
	or	%s0,0,%s62	# restore value of %s0
	# ... CLOSURE_ALLOCA bytes below %sp are usable ...
.L_closure_space:
	# I **want** to not have a stack frame (use caller's frame)
	# What needs to be done to have gdb not get confused? (FIXME)
	#st	%fp, 0(,%sp)
	#st	%lr, 8(,%sp) # not enough.

	/* stow final return addr (%lr), and entry point indicator (%s60)
	 *    Trampoline does not touch caller's %lr return address ! */
	# Turns out that %s60 is not used as such.  Could use it for
	# other rvalue help, or just get rid of it from trampoline area
	st	%lr,  ffi_closure_OFS_RETADDR+0x00(,%sp)	# caller's ret addr
	st	%s60, ffi_closure_OFS_RETADDR+0x08(,%sp)	# 0/1: REFERENCE rvalue?

	/* OFS_PARMS: leave 6*8 mem space [uninitialized]
	 * to mirror %s0..%s5 args that we'll pass to *_inner. */
	/* General Register Save: reg_args will be 5th arg to *_inner */
	/* We save all, and *_inner will sort out whatever register args
	 * are actually useful (and where memory args actually begin) */
	st	%s0,ffi_closure_OFS_G+0x00(,%sp)
	st	%s1,ffi_closure_OFS_G+0x08(,%sp)
	st	%s2,ffi_closure_OFS_G+0x10(,%sp)
	st	%s3,ffi_closure_OFS_G+0x18(,%sp)
	st	%s4,ffi_closure_OFS_G+0x20(,%sp)
	st	%s5,ffi_closure_OFS_G+0x28(,%sp)
	st	%s6,ffi_closure_OFS_G+0x30(,%sp)
	st	%s7,ffi_closure_OFS_G+0x38(,%sp)
	# OFS_RVALUE: 4*8 bytes reserved for possible rvalue

	# now unpack the *_inner arguments:
	/* Trampoline jumps into ffi_closure_ve with
	 *     instruction (bsic    %s12, (,%s12),
	 * so  %s12 = addr just after tramp[]
	 *     (TRAMPOLINE_BYTES past the actual trampoline)
	 * %s1 --> where cif, fun, and user_data are stored
	 */
	ld	%s0, 0(,%s12)			/* Load cif */
	ld	%s1, 8(,%s12)			/* Load fun */
	ld	%s2, 16(,%s12)			/* Load user_data */
	/* we no longer need trampoline area */

//ffi_closure_do:
	lea	%s3, ffi_closure_OFS_RVALUE(,%sp)/* Load rvalue (4 byte max) */
	lea	%s4, ffi_closure_OFS_G(,%sp)	/* Load reg_args (64 bytes) */
	lea	%s5, ffi_closure_OFS_ARGP(,%sp)	/* Load argp */


	addr_s12 ffi_closure_inner
	bsic	%lr,(,%s12)


//ffi_closure_rvalue:
	// just use scratch registers... no calls allowed
	and	%s6, %s0,(58)0		# rtype->type from LSB (FFI_TYPE_FOO)
	srl	%s7, %s0,8		# rtype->size in next byte

	lea	%s1, ffi_closure_OFS_PARMS(,%sp)
	lea	%s4, ffi_closure_OFS_RVALUE(,%sp)	# pre-tramp ret addr
	ld	%lr, ffi_closure_OFS_RETADDR(,%sp)	# rvalue mem ptr
//ffi_cl_rvalue2:
# Following branch turned out not useful for VE
#	ld	%s5, ffi_closure_OFS_RETADDR+0x08(,%sp) # 0/1: REFERENCE rvalue?
#	breq.l.t	%s5,0,.L_closure_REGISTER
#.L_closure_REFERENCE:
#	# TODO
#
#.L_closure_REGISTER: # rvalue could be up to long double complex (4 registers)
	breq.w.nt	FFI_TYPE_FLOAT,%s6, ffi_cl_rF0
	#brne.w.t	FFI_TYPE_FLOAT,%s6, .L_cl_notFLOAT
	#ffi_cl_rFLOAT:
	#ldu	%s0, ffi_closure_OFS_RVALUE+0x00(,%sp)
	#br.w	ffi_closure_ret
	#.L_cl_notFLOAT:
	breq.w.nt	FFI_TYPE_LONGDOUBLE,%s6, ffi_cl_rSwap01
	brne.w.t	FFI_TYPE_COMPLEX,%s6, .L_cl_notCOMPLEX
//ffi_cl_rcomplex:
	breq.l.nt	8, %s7 ,ffi_cl_rF01_packed
	#brne.l.nt	8, %s7 ,.L_cl_notFLOATCOMPLEX
	#ldu	%s0, ffi_closure_OFS_RVALUE+0x00(,%sp)
	#ldu	%s1, ffi_closure_OFS_RVALUE+0x04(,%sp)
	#br.w	ffi_closure_ret
	.L_cl_notFLOATCOMPLEX:
	breq.l.t	16, %s7 ,ffi_cl_r01
	br.w	ffi_cl_r0123

	.L_cl_notCOMPLEX:
	br.w	ffi_cl_r0123

ffi_cl_rSwap01: /* %s0 <---> higher mem address */
	ld	%s0, ffi_closure_OFS_RVALUE+0x08(,%sp)	# SWAP 0<->1
	ld	%s1, ffi_closure_OFS_RVALUE+0x00(,%sp)
	br.w	ffi_closure_ret
ffi_cl_rF01_packed:
	ldu	%s1, ffi_closure_OFS_RVALUE+0x04(,%sp)
ffi_cl_rF0:
	ldu	%s0, ffi_closure_OFS_RVALUE+0x00(,%sp)
	br.w	ffi_closure_ret
	# rvalue processing (return value ---> registers, maybe)
	#   MAYBE bump up %sp early so we can "b.l (,%lr)"
	#         instead of "br.w ffi_closure_ret
ffi_cl_r0123:
	ld	%s3, ffi_closure_OFS_RVALUE+0x18(,%sp)
	ld	%s2, ffi_closure_OFS_RVALUE+0x10(,%sp)
ffi_cl_r01:
	ld	%s1, ffi_closure_OFS_RVALUE+0x08(,%sp)
ffi_cl_r0:
	ld	%s0, ffi_closure_OFS_RVALUE+0x00(,%sp)
ffi_closure_ret:
	ld	%lr, ffi_closure_OFS_RETADDR(,%sp)	# pre-tramp ret addr
	lea	%sp, ffi_closure_ALLOCA(,%sp)		# remove alloca space
	b.l	(,%lr)					# return (pre-tramp)
	.cfi_endproc
	.size	ffi_closure_ve, . - ffi_closure_ve
//.globl ffi_closure_ret
//.globl ffi_closure_rvalue
//.globl ffi_closure_do

ffi_closure_vexx:
	;#stack frame .cfi directives are for nice stack debug
	/* [opt] libffi has macros for .cfi stuff */
	.cfi_startproc
	st	%fp,0x0(,%sp)
	.cfi_def_cfa_offset 0
	.cfi_offset	9,0		# %fp is %s9
	st	%lr,0x8(,%sp)		# %lr~%s10 Link Reg: ret addr
	;				# skip %tp at 0x10(,%sp)
	st	%got,0x18(,%sp)
	st	%plt,0x20(,%sp)
#ifdef PIC
	lea	%got,_GLOBAL_OFFSET_TABLE_@PC_LO(-24) 	#-0x18
	and	%got,%got,(32)0
	sic	%plt			# %plt := IC + 8
	lea.sl	%got,_GLOBAL_OFFSET_TABLE_@PC_HI(%plt,%got)
#endif
	or	%fp,0,%sp		# %fp := %sp
	
	#.cfi_def_cfa_register	9	# %s9 is the frame points
	S18_TO_33( st, %fp )		# %s18-%s33 Callee-saved

	# Save trampoline data into a stack frame
	lea	%sp, -16(,%fp)
	brge.l.t	%sp,%sl,.L_fEoP	# sp >= stack limit[long,taken]?
	#
	ld	%s61,0x18(,%tp)	# param area: Host Memory ptr
	or	%s62,0,%s0	# spill value of %s0
	lea	%s63,0x13b	# syscall "grow" 
	shm.l	%s63,0x0(%s61)	# StoreHostMem addr:0
	shm.l	%sl,0x8(%s61)	# old limit at addr:8
	shm.l	%sp,0x10(%s61)	# new limit at addr:16
	monc
	or	%s0,0,%s62	# restore value of %s0
	# ... 
.L_fEoP: ;# end prologue
	/* YOUR CODE HERE */
.L_fBoE: ;# begin epilogue
	#S18_TO_33( ld, %fp )	# Paranoia: %s18..33 belong to caller
	;# Remove the space we pushed for the args
	or	%sp,0,%fp	# callee stack frame -> Gone!
	#.cfi_def_cfa	11,8
	ld	%got,0x18(,%sp)
	ld	%plt,0x20(,%sp)
	ld	%lr, 0x8(,%sp)
	ld	%fp, 0x0(,%sp)
	b.l	(,%lr)
.L_fEoE: #end of epilogue
	.cfi_endproc

.balign 16
.globl ffi_example_tramp_MINIMAL
ffi_x86style_tramp:
	sic	%s12
	lea	%s0,-8(,%s12)	# %s0 = closure = tramp addr
	lea	%s12,16(,%s12)  # %s12 = addr of asm fn
        b.l	(,%s12)		# tramp[4]: branch to absolute addr
	# *((UINT64*)tramp)[4] = addr of asm fn
negative_eight:
	.8byte -8
	adds.l	%s0,0,(61)1
ffi_x86style_tramp2:
	sic	%s12
	lea	%s0,-8(,%s12)	# %s0 = closure = tramp addr
	lea	%s12,16(,%s12)  # %s12 = addr of asm fn
        b.l	(,%s12)		# tramp[4]: branch to absolute addr
	# *((UINT64*)tramp)[4] = addr of asm fn

ffi_example_tramp_MINIMAL:			# vvvvvvvvvvv
	lea	%s12, 0x33cc33dd		# dd 33 cc 33 00 00 0c 06
	and	%s12,%s12,(32)0			# 00 00 00 00 60 8c 0c 44
	lea.sl	%s12, 0x33aa33bb(,%s12)		# bb 33 aa 33 8c 00 8c 06
						# ^^^^^^^^^^^ absolute fn addr
	bsic	%s12,(,%s12)		# jmp, but with %s12 = tramp const data
	# could have flags, etc. here by increasing TRAMPOLINE_SIZE
	.8byte 0x4400440044004400 # <-- %s12 tramp[4] ffi_cit *cif
	.8byte 0x5500550055005500 # tramp[5] void (*fun)(ffic_cif*,void*,void**,void*)
	.8byte 0x6600660066006600 # tramp[6] void* user_data
ffi_example_tramp_MINIMALxx:			# vvvvvvvvvvv
	lea	%s12, 0x33cc33dd		# dd 33 cc 33 00 00 0c 06
	and	%s12,%s12,(32)0			# 00 00 00 00 60 8c 0c 44
	lea.sl	%s12, 0x33aa33bb(,%s12)		# bb 33 aa 33 8c 00 8c 06
						# ^^^^^^^^^^^ absolute fn addr
	bsic	%s0,(,%s12)		# jmp, but with %s0 = tramp const data
	# oops, cannot really use %s0, because it might have function args!
	.8byte 0x6600660066006600 # tramp[4] closure ptr
	.8byte 0x7700770077007700 # tramp[5] [opt] struct return addr
ffi_example_tramp_REGISTER:
	sic %s12		  # tramp[0]: point s2 --> trampoline+8
        lea %s0, 5*8-8(,%s12)	  # tramp[1]: load closure tramp[5] arg1 --> s0
        lea %s12, 4*8-8(,%s12)    # tramp[2]: load tramp absolute addr (reuse %s12)
        b.l (,%s12)		  # tramp[3]: branch to absolute addr
	.8byte 0x4400440044004400 # tramp[4] addr of asm routine to jump to
	.8byte 0x5500550055005500 # tramp[5] %s0 closure ptr
	.8byte 0x0000000000000000 # tramp[6] unused
	.8byte 0x0000000000000000 # tramp[7] unused
ffi_example_tramp_REFERENCE:
	sic %s12		  # tramp[0]: point s2 --> trampoline+8
        lea %s0, 6*8-8(,%s12)	  # tramp[1]: load closure tramp[5] arg1 --> s0
        lea %s1, 7*8-8(,%s12)	  # tramp[2]: load closure tramp[7] arg2 --> s1
        lea %s12, 5*8-8(,%s12)    # tramp[3]: tramp[7] asm addr --> s12
        b.l (,%s12)		  # tramp[4]: branch to asm addr
	.8byte 0x5500550055005500 # tramp[5] addr of asm routine to jump to
	.8byte 0x6600660066006600 # tramp[6] %s0 closure ptr
	.8byte 0x7700770077007700 # tramp[7] %s1 struct return addr
ffi_example_tramp_REG_common:
	# can put things at common offset...
	sic %s12		  # tramp[0]: point s2 --> trampoline+8
        lea %s0, 6*8-8(,%s12)	  # tramp[1]: load closure tramp[5] arg1 --> s0
        lea %s12, 5*8-8(,%s12)    # tramp[2]: tramp[7] asm addr --> s12
        b.l (,%s12)		  # tramp[3]: branch to asm addr
	.8byte 0x0000000000000000 # tramp[4] unused
	.8byte 0x5500550055005500 # tramp[5] addr of asm routine to jump to
	.8byte 0x6600660066006600 # tramp[6] %s0 closure ptr
	.8byte 0x0000000000000000 # tramp[7] [unused] struct return addr
# Does PIC require a trampoline with relative branch?
ffi_lea_tramp_xxx:
	#This trampolin is 80 bytes long, but no reading of instruction memory
	# following 3 are equiv patterns
	# for loading 64-byte "immediate" data into a register
	lea	%s12,%lo(0x11aa11bb11cc11dd)		#--> dd 11 cc 11 00 00 0c 06
	and	%s12,%s12,(32)0	# zero any sign-extension -> 00 00 00 00 60 8c 0c 44
	lea.sl	%s0,%hi(0x11aa11bb11cc11dd)(,%s12)	#--> bb 11 aa 11 8c 00 80 06
	# But above is **deprecate**			#                     %s0

	# Preferred ( loading %s1 only if REFERNECE return type (o.w. can skip!)
	lea	%s12, 0x22aa22bb22cc22dd@LO
	and	%s12,%s12,(32)0				#                     %s1
	lea.sl	%s1, 0x22aa22bb22cc22dd@HI(,%s12)	#--> bb 22 aa 22 8c 00 81 06

	# for trampoling creation:
	lea	%s12, 0x33cc33dd		# dd 33 cc 33 00 00 0c 06
	and	%s12,%s12,(32)0
	lea.sl	%s12, 0x33aa33bb(,%s12)		# bb 33 aa 33 8c 00 8c 06
	b.l	(,%s12)
	// or maybe
	//b.l	0x33bb33bb(%s12)

	
# --------------------------- END --------- no useful code follows ------------
#if 0
VE ABI notes (these examples from Ch. 8 docs seem good)

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

#endif
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
#if 0
sysv_restore:
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
#if 0 /* sysv_retval: obtain ecif->cif->rtype->type ?? */
	/* I think this would do it:
		or	%s63,0,%s19	# ecif (ptr)
		ld	%s62,0(0,%s63)	# ecif->cif
		ld	%s61,16(0,%s62)	# ecif->cif->rtype
		ld2b.zx	%s60,10(0,%s61)	# ecif->cif->type.type
	   but we are passed this value (in %s21), yay.
	*/
#endif
#if 0 /* sysv_rstruct: original thoughts, akin to compiler double-copy approach */
	/* size = ? */
	/* memcpy? for example XXX (wrong) */
	PRINT_MSG_U64(rstruct, ld %s0,-4*8(,%fp))	# retrieve retval = addr with content
	ld	%s0,-4*8(,%fp)	# retrieve retval = addr with content
	ld	%s1,0(,%s0)	# retrieve first 64-bits of struct/union...
	st	%s1,0(,%s23)	# (*rvalue) = %s0  (ok for 8-byte return type)
	addu.l	%s1,8,%s1
	br.w	sysv_ret1	# return 1 register (our hidden-arg1 ptr-to-struct, again)
#endif
#if 0
#set	.L.1.2auto_size,	0xfffffffffffffe50	# 432 Bytes
	;#.size CNAMESZ(ffi_call_SYSV), .L_EoE - CNAME(ffi_call_SYSV)
	;#.set CNAMESZ(ffi_call_SYSV), .- CNAME(ffi_call_SYSV)
	;#.set ffi_call_SYSV_sz, .-ffi_call_SYSV
	;#.set ffi_call_SYSV_sz, 0x0  # -ve value, additional stack space for called function
#endif


