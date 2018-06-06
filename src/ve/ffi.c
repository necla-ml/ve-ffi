/* -----------------------------------------------------------------------
   ffi.c - Copyright (c) 2003, 2004, 2006, 2007, 2012 Kaz Kojima
           Copyright (c) 2008 Anthony Green

   NEC VE Aurora (c) 2018 NEC Labs America, LLC
   VE ABI support


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

#include <ffi.h>
#include <ffi_common.h>

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h> /* intptr_t */

#include <math.h>       /* fabs */
#include <complex.h>
#include <assert.h>

#ifndef VE_DEBUG_LEVEL /* now set in ve/ffitarget.h */
#define VE_DEBUG_LEVEL   5
#endif

#define FIRST_ARG_BYTE  176 /* 22*8: fp, ret | %tp %got %plt %s17 16*%s18..33 */
#define ROUND_UP(v, a)  (((size_t)(v) + (a) - 1) & ~((a) - 1))

#define debug(lvl, x...) do { if (lvl <= VE_DEBUG_LEVEL) { printf(x); fflush(stdout); } } while (0)

#define NGREGARG 8
//#define NFREGARG 12

#if 0
// OH NO.  ffi_type_double are GLOBAL CONSTANTS provided by libffi -- we must not changed them
#if defined(FFI_TARGET_SPECIFIC_VARIADIC)
#define VARIADIC(FFI_TYPE) (((FFI_TYPE) & FFI_TARGET_SPECIFIC_VARIADIC) == FFI_TARGET_SPECIFIC_VARIADIC)
#define FFI_TYPE_MASK ((FFI_TARGET_SPECIFIC_VARIADIC>>1)-1)
#define BASE(FFI_TYPE) ((FFI_TYPE) & FFI_TYPE_MASK)
#else
#define VARIADIC(FFI_TYPE) 0
#define BASE(FFI_TYPE) (FFI_TYPE)
#endif
#endif

void foo() {;}
UINT64 howto_srl(UINT64 x){
    return x >> 8;
}
#include "stdarg.h"
int add_variadic(int count, ...){
    int ret;
    va_list args;
    va_start(args,count);
    for(int i=0; i<count; ++i)
        ret += va_arg(args,int);
    return ret;
}
void call_variadic(){
    int sum = add_variadic(3,1,2,3);
    asm volatile("nop\n");
    fprintf(stdout,"sum is %d\n", sum);
    fflush(stdout);
}


void print_s0(UINT64 s0){
    debug(2, " print_s0: %lu = %ld = 0x%lx ", (long unsigned)s0,
            (long int)s0, (long unsigned)s0);
}
void print_s0_example(UINT64 s0){
    print_s0(s0);
    if( s0 != 2 ) goto done;
    s0 = s0 + s0;
done:
    print_s0(s0);
}
void print_ptr(void const* const msg, void* ptr){
    debug(2, " %s:0x%p ", (char const*)msg, ptr);
}
void print_u64(void const* const msg, long unsigned u64){
    debug(2, " %s:%lu ", (char const*)msg, u64);
}
void print_msg_u64(void const* const s0_msg, UINT64 const s1_u64){
    debug(2, " %s:0x%lx,%lu,%ld ", (char const*)s0_msg, (long unsigned)s1_u64,
            (long int)s1_u64, (long unsigned)s1_u64);
    //debug(2, " %s:0x%lx,%lu,%ld,%f ", (char const*)s0_msg, (long unsigned)s1_u64,
    //        (long int)s1_u64, (long unsigned)s1_u64, *(float*)(void*)&s1_u64);
}
void print_msg_u64_example(void const* const s0_msg, UINT64 const s1_u64){
    print_msg_u64((char const*)s0_msg, s1_u64);
}
void print_msg_float(void const* const s0_msg, float const s1_f){
    debug(2, " %s:%f ", (char const*)s0_msg, s1_f);
}
void print_msg_double(void const* const s0_msg, double const s1_f){
    debug(2, " %s:%f ", (char const*)s0_msg, s1_f);
}

ffi_cif ffi_cif_example;
const int ffi_cif_offset_rtype = (char*)&ffi_cif_example.rtype - (char*)&ffi_cif_example;
ffi_type ffi_type_example;
const int ffi_type_offset_type = (char*)&ffi_type_example.type - (char*)&ffi_type_example;
void prt_ecif_cif_rtype_type( extended_cif *ecif ){
    printf("ecif->rtype_type=%lu\n", (long unsigned)ecif->cif->rtype->type);
    union { uint64_t u64; float f[2]; } val;
    val.u64 = 0UL;
    val.f[0] = 1.499f;
    float x = val.f[0];
    printf(" val.f[0]=%f", (double)x);
}

typedef enum ve_argclass {
    VE_REGISTER = 0,
    VE_REFERENCE = 1,
    VE_BOTH
} Argclass;

static Argclass argclass (ffi_type const *arg)
{
    Argclass ret = VE_REGISTER; /* most things */
#if 1
    if( arg->type == FFI_TYPE_STRUCT )
        ret = VE_REFERENCE;
#else
    if (BASE(arg->type) == FFI_TYPE_STRUCT){ /* or union or array */
        ret = VE_REFERENCE;
    }
    if( VARIADIC(arg->type) ){
        ret = VE_BOTH;
    }
#endif
    return ret;
}
static char const* argclass_names[3] = {"REG","REF","BOTH"};

/* ffi_type is {size_t size; u16 alignment; u16 type; _ffi_type **elements;} */
static char const* ffi_type_str( ffi_type const* const t ){
    char * ret = "??";
    switch(t->type){
        case( FFI_TYPE_VOID       ): ret = "void"; /*0*/ break;
        case( FFI_TYPE_INT        ): ret = "int"; /*1*/ break;
        case( FFI_TYPE_FLOAT      ): ret = "float"; /*2*/ break;
        case( FFI_TYPE_DOUBLE     ): ret = "double"; /*3*/ break;
        case( FFI_TYPE_LONGDOUBLE ): ret = "Ldouble"; /*4*/ break;
        case( FFI_TYPE_UINT8      ): ret = "u8"; /*5*/ break;
        case( FFI_TYPE_SINT8      ): ret = "s8"; /*6*/ break;
        case( FFI_TYPE_UINT16     ): ret = "u16"; /*7*/ break;
        case( FFI_TYPE_SINT16     ): ret = "s16"; /*8*/ break;
        case( FFI_TYPE_UINT32     ): ret = "u32"; /*9*/ break;
        case( FFI_TYPE_SINT32     ): ret = "s32"; /*10*/ break;
        case( FFI_TYPE_UINT64     ): ret = "u64"; /*11*/ break;
        case( FFI_TYPE_SINT64     ): ret = "s64"; /*12*/ break;
        case( FFI_TYPE_STRUCT     ): ret = "struct"; /*13*/ break;
        case( FFI_TYPE_POINTER    ): ret = "ptr"; /*14*/ break;
        case( FFI_TYPE_COMPLEX    )
            : 
                if(t->size==8) ret = "fcomplex";
                else if(t->size==32) ret = "ldcomplex";
                else ret = "complex";
                break;
        default: ret = "Huh"; break;
    }
    return ret;
}

static char buf_avalue[1024];
static char const* ffi_avalue_str( ffi_type const* const t, void* avalue ){
    char *buf = &buf_avalue[0];
    int rem_len = 1024;
#define DPRINT(...) do \
    { \
        int n = snprintf(buf, rem_len, __VA_ARGS__); \
        if( n > rem_len ){ rem_len = 0; } \
        else { buf+=n; rem_len-=n; } \
    } while(0)
    //if(VARIADIC(t->type)) DPRINT("V");
    switch(t->type){
        case( FFI_TYPE_VOID       ): DPRINT("void"); break;
        case( FFI_TYPE_INT        ): DPRINT("%lu",*(int*)avalue); break;
        case( FFI_TYPE_FLOAT      ): DPRINT("%f",*(((float*)avalue) /*+ 1*/)); break; /* NB */
        case( FFI_TYPE_DOUBLE     ): DPRINT("%f",*(double*)avalue); break;
        case( FFI_TYPE_LONGDOUBLE ): DPRINT("%f",(double)*(long double*)avalue); break; /* Hmm. %Lf did not work properly */
        case( FFI_TYPE_UINT8      ): DPRINT("%lu",(long unsigned) *(UINT8*)avalue); break;
        case( FFI_TYPE_SINT8      ): DPRINT("%ld",(long   signed) *(SINT8*)avalue); break;
        case( FFI_TYPE_UINT16     ): DPRINT("%lu",(long unsigned) *(UINT16*)avalue); break;
        case( FFI_TYPE_SINT16     ): DPRINT("%ld",(long   signed) *(SINT16*)avalue); break;
        case( FFI_TYPE_UINT32     ): DPRINT("%lu",(long unsigned) *(UINT32*)avalue); break;
        case( FFI_TYPE_SINT32     ): DPRINT("%ld",(long   signed) *(SINT32*)avalue); break;
        case( FFI_TYPE_UINT64     ): DPRINT("%lu",(long unsigned) *(UINT64*)avalue); break;
        case( FFI_TYPE_SINT64     ): DPRINT("%ld",(long   signed) *(SINT64*)avalue); break;
        case( FFI_TYPE_STRUCT     ): DPRINT("struct@%p",(void*)avalue); break;
#if VE_POINTER_BY_VALUE
        case( FFI_TYPE_POINTER    ): DPRINT("ptr@%p",**(void***)avalue); break;
#else
        case( FFI_TYPE_POINTER    ): DPRINT("ptr@%p",(void*)avalue); break;
#endif
        case( FFI_TYPE_COMPLEX    ):
                                     if(t->size==8) { /* float complex */
                                         float complex c=*(float complex*)avalue;
                                         double r=crealf(c), i=cimagf(c);
                                         DPRINT("fq%f%s%f",r,(i<0?"-i*":"+i*"),fabs(i));
                                     }else if(t->size==16) { /* double complex */
                                         double complex c=*(double complex*)avalue;
                                         double r=creal(c), i=cimag(c);
                                         DPRINT("q%f%s%f",r,(i<0?"-i*":"+i*"),fabs(i));
                                     }else if(t->size==32) { /* long double complex */
                                         long double complex c=*(long double complex*)avalue;
                                         double r=creall(c), i=cimagl(c);
                                         DPRINT("ldq%f%s%f",r,(i<0?"-i*":"+i*"),fabs(i));
                                     }else{
                                         FFI_ASSERT("? foo complex ?"==NULL);
                                     }
                                     break;
        default: DPRINT("?%p",avalue); break;
    }
    return &buf_avalue[0];
}
static inline void* plus_bytes(void* ptr, int bytes){
    return (uint8_t*)ptr + bytes;
}
/* long double align 16 is only exception to normal stack alignment
 * of 8.  zerofill is for debug compile/gdb (may not happen) */
static void* align16_zerofill (void *stkarg){
    if(0){
        debug(4,"-Z");
        // align_up: (value+align-1) & ~(align-1)
        stkarg = (void*)((((UINT64)stkarg) + 15UL) & ~0x0fUL);
    }else{ // longer version...
        if( ((UINT64)stkarg) & 0x0fUL ){
            debug(4,"-");
            FFI_ASSERT( ((UINT64)stkarg & 0x07UL) == 0UL );
            while( (UINT64)stkarg & 0x0fUL ){
                debug(4,"Z");
                *(UINT64*)stkarg = 0UL; // for gdb
                stkarg = ((UINT64*)stkarg + 1);
            }
        }
    }
    return stkarg;
}
/** Usually register values are stored at same base address as \c regvalue,
 * but 'float' values when expanded to 8-byte register length are stored
 * in the 4 "MSB" bytes, which are 4 bytes after \c regvalue.
 *
 * Aurora int types are stored "at" \c regvalue (ie "LSB" part of 8-byte reg)
 */
static char const* ffi_regvalue_str( ffi_type const* const t, void* regvalue ){
    return ffi_avalue_str (t, plus_bytes(regvalue, (t->type==FFI_TYPE_FLOAT? 4: 0)));
}

static char buf_avalues[1024];
static char const* ffi_avalues_str( ffi_cif const* cif, void **avalue ){
    char *buf = &buf_avalues[0];
    int rem_len = 1024;
#define DPRINT(...) do \
    { \
        int n = snprintf(buf, rem_len, __VA_ARGS__); \
        if( n > rem_len ){ rem_len = 0; } \
        else { buf+=n; rem_len-=n; } \
    } while(0)
    if(cif == NULL) DPRINT("cif==NULL,avalues=%p???",avalue);
    else if(avalue == NULL && cif->nargs > 0) DPRINT("avalues=NULL???");
    else if(cif->nargs <= 0 ) DPRINT("avalues={}");
    else if(cif->arg_types==NULL && cif->nargs > 0) DPRINT("arg_types==NULL,avalues=%p???",avalue);
    else{
        DPRINT("avalues=");
        for(int i=0; i<cif->nargs; ++i)
            DPRINT("%s%s", (i==0?"{":","), ffi_avalue_str(cif->arg_types[i], avalue[i]));
        DPRINT("}");
    }
    //DPRINT("\n");
    return &buf_avalues[0];
}

static char buf_type[1024];
/* use argpos 0 for return value, or non-variadic arg */
static char const* ffi_type_detail( ffi_type const* const t, unsigned argpos, unsigned nfixedargs ){
    char *buf = &buf_type[0];
    int rem_len = 1024;
#define DPRINT(...) do \
    { \
        int n = snprintf(buf, rem_len, __VA_ARGS__); \
        if( n > rem_len ){ rem_len = 0; } \
        else { buf+=n; rem_len-=n; } \
    } while(0)
    DPRINT("%s(s%llu,a%u)%s", ffi_type_str(t), (unsigned long long)t->size
            , (unsigned)t->alignment,
            (argpos < nfixedargs? argclass_names[argclass(t)]: "BOTH"));
    return &buf_type[0];
#undef DPRINT
}

static char buf_cif[1024];
static char const* ffi_cif_str( ffi_cif const* cif){
    char *buf = &buf_cif[0];
    int rem_len = 1024;
#define DPRINT(...) do \
    { \
        int n = snprintf(buf, rem_len, __VA_ARGS__); \
        if( n > rem_len ){ rem_len = 0; } \
        else { buf+=n; rem_len-=n; } \
    } while(0)
    DPRINT("ffi_cif@%p={abi:%d, nargs:%u, arg_types:", (void*)cif, cif->abi, cif->nargs);
    if(cif->arg_types==NULL) DPRINT("NULL");
    else if(cif->nargs <= 0) DPRINT("{}");
    else{
        /* ffi_type **arg_types */
        for(int i=0; i<cif->nargs; ++i)
            DPRINT("%s%s", (i==0?"{":","), ffi_type_detail(cif->arg_types[i],i,cif->nfixedargs));
        DPRINT("}");
    }
    DPRINT("\n\t\t, rtype=");
    if(cif->rtype==NULL) DPRINT("NULL");
    else DPRINT("%p[%s]", (void*)cif->rtype, ffi_type_detail(cif->rtype,0U,1U));
    DPRINT(", bytes:%u, flags:%u"
#if defined(FFI_EXTRA_CIF_FIELDS)
            //" [,extra]"
            ", flags2:%lx,var%u"
#endif /*FFI_EXTRA_CIF_FIELDS*/
            ") ",
            cif->bytes, cif->flags
#if defined(FFI_EXTRA_CIF_FIELDS)
            , (long unsigned)cif->flags2
#endif
            );
#if defined(FFI_EXTRA_CIF_FIELDS)
    if(cif->flags2 == 0){
        DPRINT("flags2:{no reg args}");
    }else{
        UINT64 flg = cif->flags2;
        int greg;
        DPRINT("flags2:");
        for( greg=0; greg<NGREGARG; ++greg ){
            SINT8 reginfo = flg&0xff;
            flg >>= 8;
            if( reginfo == (SINT8)0xff ) DPRINT("x");
            else if( reginfo == (SINT8)0x80 ) DPRINT("R");
            else if( reginfo < 0 ) DPRINT("?");
            else DPRINT("%u",(unsigned)(UINT8)reginfo);
            /* reginfo>0 holds an arg# in [0,127] */
        }
    }
    if(cif->nfixedargs < 999U) DPRINT(",varargs>=%u",cif->nfixedargs);
#endif
    return &buf_cif[0];
#undef DPRINT
}

/* NEC VE has a downward growing stack, which looks like this:

###### caller's stack frame
  Locals and Temporaries
.............
            | (Arg Stack, if needed for the call)
   176(%fp) | Parameter Area for callee
.............
R S A..|
e a r  | 168(%fp)   %s33Callee-saved register
g v e  |  ...        ...
i e a  | 48(%fp)    %s18 Callee-saved register
s      | 40(%fp)    %s17 Linkage Area Register
t      | 32(%fp)    %plt Procedure Linkage Table Register
e      | 24(%fp)    %got Global Offset Table Register
r......| 16(%fp)    %tp Thread Pointer Register
   8(%sp                return address
   0(%sp)               %sp (or caller's $fp)
............ Caller Stack Frame (16-byte alignment)

   ### same, but wrt %sp for callee

............ Callee Stack Frame (16-byte alignment)

   Scalar types:
     Memory:
       bool, int types up to 64-bit occupy 1,2,4,8 bytes in memory
       ptr is 8byte unsigned
       float, double, long double are 4,8,16 bytes
       memory byte alignment == size of base type
       undefined-fill structs to alignment of largest type
       long double stores top bits in higher then lower mem address
   Scalars are promoted to 64-bit registers %s0..%s7 AND/OR memory by:
     sign-extend/zero-fill signed/unsigned integers toward MSB
     LSBs of float are set to zero
     %s7..7 are filled left-to-right order

   Arguments are passed in registers AND/OR memory Parameter Area.
   The first parameters are stuck into %s0..%s7, consecutively
   Exception long double, long double _Imaginary, long double _Complex
   will skip beginning on an odd-numbered register.

   Arguments are of 3 classes:
     REGISTER  (fit in 64-bit %s0..7)
       - all scalar types 8 bytes or less (incl float, double _Imaginary)
       - long double, complex etc. are split across register
         realHI,realLO, imagHI,imagLO in order %s0..%s7 and mem hi-->low addr
     REFERENCE passed and returned by memory
       - struct, union, array
       - %s0..7 store address of memory image in Parameter Area
     BOTH      passed in registers and memory
       - variadic or no prototype

   Arguments passed via memory are promoted in the same manner as they
   are for register storage (i.e. typically 8-byte alignment)
   structs/unions/arrays ??? satisfy the alignmen of most-aligned member
   (i.e. could be 1,2,4,8,16 alignment)
   */

/* VE ret type can use argclass and deal with VE_REGISTER or VE_REFERENCE.
VE_REFERENCE: %s0 holds [lowest] addr of ret value (usually 176(%fp)
>=176(%fp) in callee-frame holds the struct data.
VE_REGISTER returns use %s0..%s7, stored same way as for calling.

NEC VE ABI 0.10 does not support small-struct optimizations. */


/* ffi_prep_args is called by the assembly routine once stack space
   has been allocated for the function's arguments.

   \p stack points to parameter area in caller's stack frame.

   We organize the stack as follows, for simplicity:

   low addr   --------------------> high addr
   stack args area   | 64 bytes reg args area (for %s0..%s7)

  FIXME ncc already provides reg args area at beginning of args memory,
   reorder above arrangement!

   ecif->cif->bytes must be size of non-reg stack args + 64,
   as prepared by ffi_prep_cif_machdep(ffi_cif *cif)
*/

void ffi_prep_args(char *stack, extended_cif* ecif)
{
    register unsigned int i;
    register unsigned int avn;
    register void **p_argv;
    char *stkarg;
#if ! defined(FFI_EXTRA_CIF_FIELDS)
#error "NEC VE relies on machdep having deduce which args go in registers"
#endif
    char *stkreg; /* for now, last 64 bytes of ecif->cif->bytes are register arg mirrors */
    char* stkreg_beg;
    char* stkreg_end;
    register unsigned int regn;
    register ffi_type **p_arg;


    /* ecif = { ffi_cif*, void*rvalue, void**avalue } is used in callback from assembly */
    debug(1, "%s %s: stack = %p, ecif = %p, bytes = %u, rvalue=%p, avalue=%p\n      ecif->cif=%s\n",
            "BEGIN", __FUNCTION__, stack, ecif, ecif->cif->bytes, ecif->rvalue, ecif->avalue,
            ffi_cif_str(ecif->cif));
    if(ecif->avalue != NULL) debug(1, "      %s\n", ffi_avalues_str(ecif->cif,ecif->avalue));

    stkarg = stack;                           /* non-register args area */
    stkreg = stack + ecif->cif->bytes - 64;   /* register mirror area %s0..%s7 */
    stkreg_beg = stkreg;        /* overrun check:  stkarg <= stkreg_beg */
    stkreg_end = stkreg + 64;   /* overrun check:  stkreg <= stkreg_end */
    /* above shows that -64 as "adds" of M value (58)1 */
    debug(3,"stkreg=%p .. + [cif->bytes=%lu] + [-64=0x%x] = %p\n",
            (void*)stack,(long unsigned)ecif->cif->bytes,
            -64, (void*)stkreg);
    regn = 0;
    UINT64 regflags = ecif->cif->flags2;
    SINT8  reginfo = regflags&0xFF;
#define STKREG_NEXT do { \
    if(regn < NGREGARG){ \
        /*debug(4," regf:%lx,",(unsigned long)regflags);*/ \
        stkreg += sizeof(UINT64); ++regn; \
        regflags >>= 8; reginfo = regflags&0xFF; \
        /*debug(4,"->%lx regi=%x ", (unsigned long)regflags, (unsigned)reginfo);*/ \
        debug(4," ri%x ",(unsigned)(UINT8)reginfo); \
        for( ; regn<NGREGARG && reginfo<0; ++regn, \
                regflags>>=8, reginfo=regflags&0xFF ){ \
            debug(4,"X"); \
            *(UINT64*)stkreg = 0UL; \
            stkreg+=sizeof(UINT64); \
        } \
    }else{ \
        reginfo = 0xff; /*reginfo < 0 : arg not in register */ \
    } \
}while(0)


#if 1 /* !defined(FFI_NO_STRUCTS) */
    /* debug(1," HELLO "); */
    /* When callee returns an aggregate (VE_REFERENCE), the caller:
       - provides return memory on its stack and set %s0 to caller-176%(sp).
       and callee also set %s0 to the address of the returned struct.

       In effect, a returned struct becomes a hidden "first argument".

       Now does libffi stick this "hidden arg" into arg_types?
       If not, how is ffi_cif.rtype used? */
    if (ecif->cif->rtype->type == FFI_TYPE_STRUCT)
    {
        FFI_ASSERT( regn == 0 && reginfo < 0 );
        FFI_ASSERT( ecif->rvalue != NULL );
        *(UINT64*)stkreg = (UINT64)ecif->rvalue;
        /* %s0 points at return-struct mem (user-supplied, or from alloca in machdep) */
        STKREG_NEXT;
        /* in principle the ptr could be register-only, as docs hint, but actually... */
        *(UINT64*)stkarg = (UINT64)ecif->rvalue;
        stkarg += 8;
        debug(2,"struct[s%lua%u]->stkreg+stkarg as ptr %p\n",
                (long unsigned)ecif->cif->rtype->size, ecif->cif->rtype->alignment,
                ecif->rvalue);
    }
#endif

    avn = ecif->cif->nargs;
    p_argv = ecif->avalue;

    debug(3," prep_args: ");
    for (i = 0, p_arg = ecif->cif->arg_types; i < avn; i++, p_arg++, p_argv++)
    {
        size_t z;
        int align;

        int type = (*p_arg)->type;
        debug(4,"\n   a%u[%s]%s ",(unsigned)i,
                ffi_type_detail(*p_arg,i,ecif->cif->nfixedargs),
                ffi_avalue_str(*p_arg,*p_argv));

        Argclass cls = argclass(*p_arg); /* VE_REGISTER/REFERENCE/BOTH */
        if(i >= ecif->cif->nfixedargs){
            cls == VE_BOTH;
        }
        FFI_ASSERT( cls == VE_REGISTER || cls == VE_BOTH || type == FFI_TYPE_STRUCT );
        debug(4,"%s",(cls==VE_REGISTER?"REG":cls==VE_REFERENCE?"REF":"BOTH"));

        z = (*p_arg)->size;
        align = (*p_arg)->alignment;

        /* these types are passed as single-register (if possible) */
        union uarg_t {
            UINT64 r1;
            UINT64 u[4];
            float f[4];
            double d;
            double d2[2];
            long double ld;
            long double ld2[2];
#if FFI_TARGET_HAS_COMPLEX_TYPE
            float complex fq;
            double complex q;
            long double complex ldq;
#endif
        } val;
        val.r1 = 0UL;   /* for gdb debug */
        val.u[1] = 0UL;
        val.u[2] = 0UL;
        val.u[3] = 0UL;

        switch (type)
        {

            case FFI_TYPE_SINT8:
                *(SINT64*) &val.r1 = (SINT64) *(SINT8 *)(*p_argv);
                break;

            case FFI_TYPE_UINT8:
                *(UINT64 *) &val.r1 = (UINT64) *(UINT8 *)(*p_argv);
                break;

            case FFI_TYPE_SINT16:
                *(SINT64 *) &val.r1 = (SINT64) *(SINT16 *)(*p_argv);
                break;

            case FFI_TYPE_UINT16:
                *(UINT64 *) &val.r1 = (UINT64) *(UINT16 *)(*p_argv);
                break;

            case FFI_TYPE_INT:
            case FFI_TYPE_SINT32:
                *(SINT64*) &val.r1 = (SINT64) *(SINT32 *)(*p_argv);
                break;

            case FFI_TYPE_UINT32:
                *(UINT64*) &val.r1 = (UINT64) *(UINT32 *)(*p_argv);
                break;

            case FFI_TYPE_SINT64:
                *(SINT64*) &val.r1 = *(SINT64 *)(*p_argv);
                break;

            case FFI_TYPE_UINT64:
                *(UINT64*) &val.r1 = *(UINT64 *)(*p_argv);
                break;

            case FFI_TYPE_FLOAT:
                val.r1   = 0UL; /* [OPT] zero the other half-register */
                val.f[1] = *(float*)(*p_argv);
                break;

            case FFI_TYPE_DOUBLE:
                val.d = *(double *)(*p_argv);
                break;

#if 1 /* !defined(FFI_NO_STRUCTS) */
            case FFI_TYPE_STRUCT:
                FFI_ASSERT( type == FFI_TYPE_STRUCT ); /* VE has no special small-struct handling */
                FFI_ASSERT( cls == VE_REFERENCE );
                /* Let's hope that the struct MEMORY content has already been set up, and all we
                   need to do is the pointer-pushing DIRECTLY TO THE avalues[] pointer */
                debug(5,"\nSTRUCT-arg pointer: *p_argv=%p ", *(void**)p_argv);
                *(SINT64*) &val.r1 = (SINT64) *(void**)p_argv;
                debug(5," *0x%lx = %d ", (unsigned long)val.r1, *(SINT32*)(void*)val.r1);
                break;
#endif
            case FFI_TYPE_POINTER:
                debug(5," *p_argv=%p ", *(void**)p_argv);
#if VE_POINTER_BY_VALUE /* easier, but nonstandard according to libffi testsuite? */
                *(SINT64*) &val.r1 = (SINT64) *(void**)p_argv;
#else
                *(SINT64*) &val.r1 = (SINT64) **(void***)p_argv;
#endif
                debug(5," ptr 0x%lx ", (unsigned long)val.r1);
                break;
            case FFI_TYPE_LONGDOUBLE:
                *(long double*)&val.ld = *(long double*)(*p_argv);
                break;
            case FFI_TYPE_COMPLEX:
                /*
                 *
                 * NOTE
                 *
                 *   The memory order can't be tested until ncc-1.0.8 bugs
                 *   are fixed, but old ABI docs suggest:
                 *
                 *   Register order: upper [lower] real, then upper [lower] imag
                 *                   %s0, %s1, ...
                 *
                 *   Memory order:   upper ~ higher address
                 *
                 *   For long double complex,
                 *      Register: %s0 %s1  %s2 %s3  would be stored in
                 *      Memory:   %s1 %s0  %s3 %s2
                 *                --real-- --imag--
                 *      with alignment 16 / begin in even-numbered register
                 *
                 */
                if( z == 8 ){ /* z==8 (align==4) : float complex */
                    {
                        float re = ((float*)*p_argv)[0];
                        float im = ((float*)*p_argv)[1];
                        debug(4,"z8(%f,%f) ",(double)re,(double)im);
                    }
                    val.u[0] = 0UL;
                    val.u[1] = 0UL;
                    val.f[1] = *  (float*)(*p_argv);      /* real */
                    val.f[3] = * ((float*)(*p_argv) + 1); /* imag */
                    debug(5," re:%f im:%f",(double)val.f[1],(double)val.f[3]);
                }else if( z == 16 ){ /* z==16 (align==8) : double complex */
                    {
                        double re = *( ((double*)*p_argv) + 0);
                        double im = *( ((double*)*p_argv) + 1);
                        debug(4,"(%f,%f) ",(double)re,(double)im);
                    }
                    val.d2[0] = * ((double*)(*p_argv) + 0); /* real */
                    val.d2[1] = * ((double*)(*p_argv) + 1); /* imag */
                }else if( z == 32 ){ /* z==32 (align==16) : long double complex */
                    {
                        long double re = *( ((long double*)*p_argv) + 0);
                        long double im = *( ((long double*)*p_argv) + 1);
                        debug(4,"ldq(%f,%f) ",(double)re,(double)im);
                    }
                    val.ld2[0] = * ((long double*)(*p_argv) + 0); /* real */
                    val.ld2[1] = * ((long double*)(*p_argv) + 1); /* imag */
                }else{
                    FFI_ASSERT("\n\tprep_args FFI_TYPE_COMPLEX TBD" == NULL);
                }
                break;

            default:
                debug(0,"type=%u\n",type);
                FFI_ASSERT("unhandled small type in ffi_prep_args" == NULL);
        }
        debug(4, " i%uri%xz%u ",(unsigned)i,(unsigned)reginfo,(unsigned)z);
        if(0){ ;
        }else if(type==FFI_TYPE_COMPLEX && z==16){ /* z==8,16,32 to distinguish 3 cases */
            FFI_ASSERT( align <= 8 );
            /* float/double complex do not need to skip registers (align<=8) */
            double re,im;
            re = *( ((double*)*p_argv) + 0);
            im = *( ((double*)*p_argv) + 1);
            debug(4,"%016lx|%016lx ", (unsigned long)val.u[0], (unsigned long)val.u[1]);
            debug(4,"(%f,%f) ",re,im);
            char const* part[2]={"re","im"};
            for(int p=0; p<2; ++p){
                if( reginfo == i ){
                    /* register area set %s0..7 from low to high addr */
                    debug(4," %s%%s%u",part[p],(unsigned)regn);
                    *(UINT64*)stkreg = val.u[p]; /* p=0,1 for real,imag */
                    debug(4, "@%p->dq:%s=%g",(void*)stkreg, part[p], *(double*)stkreg);
                    STKREG_NEXT;
                    *(UINT64*)stkarg = 0UL; /* for gdb */
                }
                if(reginfo != i || cls == VE_BOTH){
                    *(UINT64*)stkarg = val.u[p]; /* p=0,1 for real,imag */
                    debug(4, " M@%p->dq:%s=%g",(void*)stkarg, part[p], *((double*)stkarg));
                }
                stkarg+=8;
            }
        }else if(type==FFI_TYPE_COMPLEX && z==8){ /* z==8,16,32 to distinguish 3 cases */
            FFI_ASSERT( align <= 8 );
            /* float/double complex do not need to skip registers (align<=8) */
            float re,im;
            re = *( ((float*)*p_argv) + 0);
            im = *( ((float*)*p_argv) + 1);
            debug(4,"%016lx|%016lx ", (unsigned long)val.u[0], (unsigned long)val.u[1]);
            debug(4,"(%f,%f) ",re,im);
            char const* part[2]={"re","im"};
            for(int p=0; p<2; ++p){
                if( reginfo == i ){
                    /* register area set %s0..7 from low to high addr */
                    debug(4," %s%%s%u",part[p],(unsigned)regn);
                    *(UINT64*)stkreg = val.u[p]; /* p=0,1 for real,imag */
                    debug(4, "@%p->dq:%s=%g",(void*)stkreg, part[p], (double)*(float*)stkreg);
                    STKREG_NEXT;
                    //FFI_ASSERT( reginfo == i );
                    // no longer true if can split real/imag across %s7 !!!
                    *(UINT64*)stkarg = 0UL; /* for gdb */
                }
                if(reginfo != i || cls == VE_BOTH){
                    *(UINT64*)stkarg = val.u[p]; /* p=0,1 for real,imag */
                    debug(4, " M@%p->dq:%s=%g",(void*)stkarg, part[p], (double)*((float*)stkarg));
                }
                stkarg+=8;
            }
        }else if(type==FFI_TYPE_COMPLEX && z==32){ /* long double complex */
            FFI_ASSERT( align == 16 );
            long double re,im;
            re = *( ((long double*)*p_argv) + 0);
            im = *( ((long double*)*p_argv) + 1);
            debug(4,"%016lx|%016lx ", (unsigned long)val.u[0], (unsigned long)val.u[1]);
            debug(4,"(%f,%f) ",(double)re,(double)im);
            char const* part[2]={"re","im"};
            for(int p=0; p<2; ++p){
                if( reginfo == i ){
                    /* register area set %s0..7 from low to high addr */
                    debug(4," %s%%s%u%%s%u",part[p],(unsigned)regn,(unsigned)(regn+1));
                    char *pmem = stkreg;
                    /* Q: is there an endian issue for long double mem <--> reg? */
                    /* A: YES --> these look like they are in wrong order :) */
                    *(UINT64*)stkreg = val.u[2*p+1]; /* p=0,1 for real,imag */
                    STKREG_NEXT;
                    FFI_ASSERT( reginfo == i ); /* can't split a long double (GUESS) */
                    *(UINT64*)stkreg = val.u[2*p+0]; /* p=0,1 for real,imag */
                    STKREG_NEXT;
                    debug(4, "@%p->dq:%s=%g",(void*)pmem, part[p], (double)*(long double*)pmem);
                    //FFI_ASSERT( reginfo == i );
                    // no longer true if can split real/imag across %s7 !!!
                    stkarg = align16_zerofill(stkarg);
                    debug(4, " M@%p->0",(void*)stkarg, part[p]);
                    *(UINT64*)(stkarg+0) = 0UL; /* for gdb */
                    *(UINT64*)(stkarg+8) = 0UL; /* for gdb */
                }
                if(reginfo != i || cls == VE_BOTH){
                    debug(4," stkarg");
                    FFI_ASSERT(align == 16);    /* long double foo is only type with align 16 */
                    stkarg = align16_zerofill(stkarg);
                    // long double version:
                    //*(long double*)stkarg = val.ld;
                    //*(UINT64*)stkarg = val.u[1];
                    //*((UINT64*)stkarg+1) = val.u[0];
                    //debug(4, "@%p->ld:%g ",(void*)stkarg,(double)*(long double*)stkarg);
                    // Here:
                    *(UINT64*)(void*)(stkarg+0) = val.u[2*p+0]; /* p=0,1 for real,imag */
                    *(UINT64*)(void*)(stkarg+8) = val.u[2*p+1]; /* p=0,1 for real,imag */
                    debug(4, " M@%p->dq:%s=%g",(void*)stkarg, part[p], (double)*((long double*)stkarg));
                }
                stkarg += 16;
            }
        }else if( type == FFI_TYPE_LONGDOUBLE ){
            debug(4,"%016lx|%016lx ", (unsigned long)val.u[0], (unsigned long)val.u[1]);
            if( reginfo == i ){
                /* register area set %s0..7 from low to high addr */
                debug(4," %%s%u|%%s%u ",(unsigned)regn,(unsigned)(regn+1));
                *(UINT64*)stkreg = val.u[1]; /* lower reg ~ HIGHER mem addr */
                STKREG_NEXT;
                FFI_ASSERT( reginfo == i );
                *(UINT64*)stkreg = val.u[0];
                STKREG_NEXT;
            }
            if(reginfo != i || cls == VE_BOTH){
                FFI_ASSERT(align == 16);
                /* store in arg space ... mem to mem "normal" copy */
                /* BUT: may need to skip if alignment is high (like stkreg) */
                debug(4," stkarg");
                stkarg = align16_zerofill(stkarg);
                *(long double*)stkarg = val.ld;
                //*(UINT64*)stkarg = val.u[1];
                //*((UINT64*)stkarg+1) = val.u[0];
                debug(4, "@%p->ld:%g ",(void*)stkarg,(double)*(long double*)stkarg);
            }
            stkarg += 2*8;
        }else if(z <= 8 || type==FFI_TYPE_STRUCT){
            FFI_ASSERT(align <= 16);
            debug(4,"%016lx ", (unsigned long)val.u[0]);
            if( reginfo == i ){ /* is this a register value? */
                *(UINT64*)stkreg = val.r1;
                //debug(3," r%lu", (long unsigned)val.r1);
                debug(4," r@%p=%s=%lx", (void*)stkreg, ffi_regvalue_str(*p_arg,(void*)&val),
                        (unsigned long)val.r1);
                STKREG_NEXT;
                //if( cls == VE_REGISTER )
                //    continue; /* ONLY in register */
                FFI_ASSERT( stkreg <= stkreg_end );
                *(UINT64*)stkarg = 0UL; /* for gdb */
            }
            if(reginfo != i || cls == VE_BOTH){
                /* store in arg space ... */
                *(UINT64*)stkarg = val.r1;
                //debug(3," stk%lu", (unsigned long)val.r1);
                debug(4," stkarg@%p->%s=%s", (void*)stkarg,
                        ffi_regvalue_str(*p_arg,(void*)&val.r1),
                        ffi_regvalue_str(*p_arg,(void*)stkarg));
            }
            stkarg += 8;
            FFI_ASSERT( stkarg <= stkreg_beg );
        }else{
            debug(4," UNHANDLED ARG TYPE \n");
            FFI_ASSERT("Unhandled type" == NULL);
        }
#if 0
        {
            int n = (z + sizeof (UINT64) - 1) / sizeof (UINT64);
            memcpy (stkarg, *p_argv, z);
            stkarg += n * sizeof (UINT64);
        }
#endif
    }
    debug(3,"\n");

    debug(1, "%s %s: stack = %p, ecif = %p, bytes = %u, rvalue=%p, avalue=%p\n      ecif->cif=%s\n",
            "END", __FUNCTION__, stack, ecif, ecif->cif->bytes, ecif->rvalue, ecif->avalue,
            ffi_cif_str(ecif->cif));
    if(ecif->avalue != NULL) debug(2, "      %s\n", ffi_avalues_str(ecif->cif,ecif->avalue));
    return;
}

/* VE varargs support:
 *    REGISTER can only be applied to args < nfixedargs
 *    aftwards, the varargs are of class BOTH (store in
 *    register AND in memory area).  However if the varargs
 *    would go into %s4 (say), the memory is the 4th scalar
 *    in the normally-empty register mirror area.
 *    (undocumented?)
 */
ffi_status ffi_prep_cif_machdep_var(ffi_cif *cif,
				    unsigned int nfixedargs,
				    unsigned int ntotalargs)
{
  debug(1," varargs:%u/%u cif->nargs=%u\n",nfixedargs,ntotalargs,cif->nargs);
  cif->nfixedargs = nfixedargs; /* nicer debug print */
  ffi_status status = ffi_prep_cif_machdep (cif);
  cif->nfixedargs = nfixedargs;
#if 0
  for(unsigned i=nfixedargs; i<cif->nargs; ++i){
      debug(1," varg%u!",i);
      cif->arg_types[i]->type |= FFI_TARGET_SPECIFIC_VARIADIC;
  }
  for(unsigned i=0U; i<cif->nargs; ++i)
      debug(1," %x",cif->arg_types[i]->type);
  for(unsigned i=0U; i<cif->nargs; ++i)
      debug(1," %x",(void*)cif->arg_types[i]);
  debug(1,"%s","\n");
#endif
  debug(1, "%s %s: cif=%s\n", "END", __FUNCTION__, ffi_cif_str(cif));
  return status;
}

/* Perform machine dependent cif processing.
 * ffi_prep_args        is called for every invocation, while
 * ffi_prep_cif_machdep is called just once.
 * So here we set flags that simplify setting up the stack in ffi_prep_args.
 * 1. flags2 is used to remember which args get passed in registers
 * 2. TODO (flags3?   For a register arg in flags2, is it a BOTH arg?)
 * */
ffi_status ffi_prep_cif_machdep(ffi_cif *cif)
{
    UINT8 i, cif_nargs=(UINT8)cif->nargs;
    int size, type;
    int n;
    UINT8 greg=0;
    UINT64 flags2 = 0UL;
    //int fpair = -1;
#if defined(FFI_EXTRA_CIF_FIELDS)
    //int j=0;
    //int m;
    cif->flags2 = 0UL;
    /* arg# < this ==> "normal" register/mem decision */
    /* arg# > nfixeedargs ==> "BOTH" treatment (in reg AND in reg-mirror arg area) */
    cif->nfixedargs = (unsigned) -1;
#endif

    //debug(2, "BASE mask is %lx\n", (unsigned)FFI_TYPE_MASK);
    //debug(2," ld=%x, BASE(ld)=%x\n",(unsigned)FFI_TYPE_LONGDOUBLE,(unsigned)BASE(FFI_TYPE_LONGDOUBLE));
    //debug(2," ld=%x, BASE(ld)=%x\n",(unsigned)(FFI_TYPE_LONGDOUBLE+32),(unsigned)BASE(FFI_TYPE_LONGDOUBLE+32));
    //FFI_ASSERT(BASE(   FFI_TYPE_LONGDOUBLE) == FFI_TYPE_LONGDOUBLE);
    //FFI_ASSERT(BASE(32+FFI_TYPE_LONGDOUBLE) == FFI_TYPE_LONGDOUBLE);
    //FFI_ASSERT(!VARIADIC(   FFI_TYPE_LONGDOUBLE));
    //FFI_ASSERT( VARIADIC(32+FFI_TYPE_LONGDOUBLE));
    debug(2,"ve: ffi_prep_cif_machdep(ffi_cif*) %s (cif->bytes=%lu)\n      %s\n",
            "BEGINS", (unsigned long)cif->bytes, ffi_cif_str(cif));

    /* VE_REGISTER --> 0
       return value in %s0.., no regs implicated for the call */
    /* VE_REFERENCE --> 1
       caller provides space on stack and passes address in %s0 as if
       this were a "hidden" first argument to the callee */
    /* greg = argclass(cif->rtype); */  /* too tricky, and want to set flags2! */
    if( argclass(cif->rtype) == VE_REFERENCE ){
        FFI_ASSERT( cif->rtype->type == FFI_TYPE_STRUCT );
        /* use %s0 as a pointer register for return value */
        flags2 = 0x80;  /* first arg 0x80 ==> return value pointer */
        greg=1;
        debug(3," %s:%lx:%u","ret_REF",flags2,(unsigned)greg);
    }

    /* ? which args get passed in register */
    /* C limits # argumnents to a function to 127 */
    /* So for 8 register-args, we can have a 1-bit flag + 7-bit arg# */
    /* The 7-bit arg#s must be increasing. (if not treat as "skip" reg) */
    /* Multi-register args repeat the flags value */
    /* If the flag bit is set, this could mean "skip" (or for %s0 "retval ptr") */
    /* If two consecutive "skip" flags occur, there will be no more register args */
    for (i = 0; i < cif_nargs; ++i)
    {
        int regs = 1;
        ffi_type* ffi_type_i = (cif->arg_types)[i];
        debug(4,"\ni=%d[%s],greg=%d,",(int)i,
                ffi_type_detail(ffi_type_i,i,cif->nfixedargs),
                (int)greg);
        int klas = argclass(ffi_type_i);

        //if(greg >= NGREGARG) break; // wrong - even if non-reg, may need to modify cif->bytes !!!
        type = ffi_type_i->type;
        size = ffi_type_i->size;
        /* if( klas == VE_REGISTER ) ... we now handle structs largely like pointers */
        if(1) {
            int sk=0; /* skip one 8-byte unit? */
            switch(type){
                case FFI_TYPE_LONGDOUBLE: /* also for long double _Imaginary */
                    if(greg >= NGREGARG) break;
                    FFI_ASSERT( ffi_type_i->alignment > 8 );
                    regs=2; /* and start on even reg */
                    if( greg + (greg&1) + regs <= NGREGARG ){ /* have enough regs! */
                        if( greg&1 ){
                            flags2 += 0xffLU<<(greg++*8); /* "skip" register (zero?) */
                            debug(3," ld%s:%lx++g%u","SKIP",flags2,(unsigned)greg);
                        }
                        flags2 += (UINT64)i<<(greg++*8); /* upper part */
                        flags2 += (UINT64)i<<(greg++*8); /* lower part */
                        debug(3," ld%s:%lx++g%u","REG",flags2,(unsigned)greg);
                    }else{
                        /* "If there are no registers available for arguments,
                         * the **remaining** arguments are passed by the
                         * parameter area on the stack */
                        for( ; greg<NGREGARG; ++greg ) flags2 += 0xffLU<<(greg*8); /* "skip" */
                        greg = NGREGARG;
                        debug(3," %s:%lx++g%u","Done",flags2,(unsigned)greg);
                    }
                    break;
                case FFI_TYPE_COMPLEX:
                    FFI_ASSERT( size==8 || size==16 || size==32 );
                    n = size / 8;
                    //debug(5," Hello complex%u ",(unsigned)n);
                    if( size == 8 ){     /* float ... */
                        cif->bytes += 8; /* real, imag each in separate scalar reg */
                        debug(5, " cb+8=%u ",(unsigned)cif->bytes);
                        n = 2;
                    }else if( size == 32 ){
                        /* long double complex must begin on even register */
                        FFI_ASSERT( ffi_type_i->alignment > 8 );
                        sk = greg&1;
                    }
                    if(greg >= NGREGARG) break; /* we've done cif->bytes update! */
                    /* I suspect long double complex might be splittable (like double) */
                    if (sk + greg >= NGREGARG){
                        debug(3," qNOREGS(need %u) ",(unsigned)n);
                        break;
#if 0 /* the following disallows complex begin partially in register and partially in mem */
                    }else if (sk + greg + n > NGREGARG){ /* overflow, do not pass in reg */
                        for( ; greg<NGREGARG; ++greg ) flags2 += 0xffLU<<(greg*8); /* "skip" */
                        greg = NGREGARG;
                        debug(3," q%s:%u","DONE",flags2,(unsigned)greg);
#endif /* but it seems complex CAN split with real in register and imaginary in mem !!! */
                    }else{
                        for(int s=0; s<sk; ++s){
                            flags2 += 0xffLU<<(greg++*8);
                            debug(3," [Q%u]%s:%lx ++g%u",size,"SKIP",flags2,(unsigned)greg);
                        }
                        n += greg;  /* end register# for this arg */
                        if( n > NGREGARG ) {
                            n = NGREGARG; /* NEW and surprising */
                            /* may have additional restrictions for long double complex! */
                        }
                        do{
                            flags2 += (UINT64)i<<(greg++*8); /* this greg holds arg 'i' */
                            debug(3," [Q%u]%s:%lx ++g%u",size,"REG",flags2,(unsigned)greg);
                        }while( greg < n );
                    }
                    break;
                case FFI_TYPE_STRUCT:
                    debug(5," Hello struct ");
                    FFI_ASSERT(klas == VE_REFERENCE);
                    if (size < sizeof (UINT64))
                        cif->bytes += sizeof (UINT64) - size; /* probably not necessary */
                    //n = (size + sizeof (UINT64) - 1) / sizeof (UINT64);
                    if(greg >= NGREGARG) break; /* we've done cif->bytes update! */
                    n = 1; /* one register/stack locn, for the pointer */
                    /* NOTE this is now like the default case but for n=1 */
                    if (greg >= NGREGARG){
                        debug(3," [S]cont:n=%u ",(unsigned)n);
                        break;
                    }else if (greg + n > NGREGARG){ /* overflow, do not pass in reg */
                        for( ; greg<NGREGARG; ++greg ) flags2 += 0xffLU<<(greg*8); /* "skip" */
                        greg = NGREGARG;
                        debug(3," [S]%s:%lx++g%u","DONE",flags2,(unsigned)greg);
                    }else{
                        n += greg;  /* end register# for this arg */
                        do{
                            flags2 += (UINT64)i<<(greg++*8); /* this greg holds arg 'i' */
                            debug(3," [S]%s:%lx++g%u","REG",flags2,(unsigned)greg);
                        }while( greg < n );
                    }
                    break;
                default:
                    FFI_ASSERT(klas != VE_REFERENCE);
                    FFI_ASSERT(klas == VE_REGISTER); /* size must be <=8, or 16, or 32 */
                    if (size < sizeof (UINT64)){
                        /* The default cif->bytes seems to be OK for VE (no fixup required ?) */
                        ;//cif->bytes += sizeof (UINT64) - size;
                    }else{
                        FFI_ASSERT( size == 8 ); /* treat larger ones case-by-case */
                    }
                    n = (size + sizeof (UINT64) - 1) / sizeof (UINT64); /* 1 except for long specials types */
                    if (greg >= NGREGARG){
                        debug(3," cont:n=%u ",(unsigned)n);
                        break; // actually continue would be fine [ in all greg>= tests ]
                    }else if (greg + n > NGREGARG){ /* overflow, do not pass in reg */
                        for( ; greg<NGREGARG; ++greg ) flags2 += 0xffLU<<(greg*8); /* "skip" */
                        greg = NGREGARG;
                        debug(3," [D]%s:%lx++g%u","DONE",flags2,(unsigned)greg);
                    }else{
                        n += greg;  /* end register# for this arg */
                        do{
                            flags2 += (UINT64)i<<(greg++*8); /* this greg holds arg 'i' */
                            debug(3," [D]%s:%lx++g%u","REG",flags2,(unsigned)greg);
                        }while( greg < n );
                    }
#if 0 && defined(FFI_EXTRA_CIF_FIELDS) /* this was for some other chip */
                    for (m = 0; m < n; m++)
                        cif->flags2 += FFI_TYPE_INT << (2 * j++);
#endif /*FFI_EXTRA_CIF_FIELDS*/
                    break;
            }
        }
    }

    if( greg < NGREGARG ){
        for( ; greg<NGREGARG; ++greg ){
            flags2 += (0xffLU<<(greg*8)); /* "skip" */
        }
        debug(3," %s:%lx:%u","DoNe",flags2,(unsigned)greg);
    }
    debug(3,"\n");
#if defined(FFI_EXTRA_CIF_FIELDS)
    cif->flags2 = flags2;
#endif /*FFI_EXTRA_CIF_FIELDS*/


    /* Set the return type flag XXX CHECKME */
    switch (cif->rtype->type)
    {
#if 1 /* !defined(FFI_NO_STRUCTS) */
        case FFI_TYPE_STRUCT: /* there is no special handling for small-structs*/
            /* cif->bytes += cif->type->size; */
            /* are structs on stack, or really on "temporary space"? */
            /* I'll try alloca for the space, and the ptr-->%s0 */
#if 0
   9:/usr/uhome/aurora/4gi/nlabhpg/kruus/vt/src/asm-examples/Size1_fn_void.c ****       Size1 s = ext_Size1_fn_void();
          .loc    1 9 0
          adds.l  %s63,%fp,(60)1        # %s63 = %fp - 16 = -16(,%fp)
          st      %s63,176(0,%sp)       # 1st "hidden" arg points to -16(,%fp)
          st      %s0,-24(,%fp)   # spill (our own hidden Size1* destination arg)
          or      %s0,0,%s63            # and hidden ptr arg occupies %s0
          lea     %s12,ext_Size1_fn_void@PLT_LO(-24)
          and     %s12,%s12,(32)0
          sic     %lr
          lea.sl  %s12,ext_Size1_fn_void@PLT_HI(%s12,%lr)
          bsic    %lr,(,%s12)     # ext_Size1_fn_void
          ld1b.sx %s63,-16(0,%fp) # Size1_fn_void.__unnamed.0.c0
  # line 10
  10:/usr/uhome/aurora/4gi/nlabhpg/kruus/vt/src/asm-examples/Size1_fn_void.c ****       return s;
          .loc    1 10 0
          ld      %s62,-24(,%fp)  # restore (hidden Size1* dest arg)
          st1b    %s63,0(0,%s62)  # memcpy 1b struct that we got into our destination arg
#endif
#endif
        case FFI_TYPE_VOID:
        case FFI_TYPE_FLOAT:
        case FFI_TYPE_DOUBLE:
        case FFI_TYPE_SINT64:
        case FFI_TYPE_UINT64:
            cif->flags = cif->rtype->type;
            break;

        default:
            cif->flags = cif->rtype->type; /* ?? */
            break;
    }
    FFI_ASSERT( cif->flags == cif->rtype->type );

    debug(4," [reg area 64 b]:cif_bytes=%u",(unsigned)cif->bytes);
    cif->bytes += 64; /* ALWAYS provide a register area (fixed size, for now) */
    // FIXME I later found (by disassembly) that the VE provides at the
    // beginning of arg space mirroring all register args -- so REGISTER
    // args are passed in register ** and ** are backed with uninitialized
    // stack space --- so the register args should naturally be mirrored
    // right into the BEGINNING of the stack area.
    debug(4,"-->%u",(unsigned)cif->bytes);

    /* VE ABI 0.32 requires stack frame size & alignment of 16 */
    cif->bytes = FFI_ALIGN( cif->bytes, 16 );
    debug(4,"-->[align16]%u",(unsigned)cif->bytes);

    debug(2,"ve: ffi_prep_cif_machdep(ffi_cif*) %s (cif->bytes=%lu)\n      %s\n",
            "ENDS", (unsigned long)cif->bytes, ffi_cif_str(cif));
    return FFI_OK;
}

/*@-declundef@*/
/*@-exportheader@*/
extern void ffi_call_SYSV(void (*)(char *, extended_cif *),
			  /*@out@*/ extended_cif *,
			  unsigned, unsigned,
#if defined(FFI_EXTRA_CIF_FIELDS)
              long unsigned, /*flags2*/
#endif /*FFI_EXTRA_CIF_FIELDS*/
			  /*@out@*/ unsigned *,
			  void (*fn)(void));
/*@=declundef@*/
/*@=exportheader@*/

void ffi_call_SYSV_example(void (*prep_args)(char *, extended_cif * ),
			  /*@out@*/ extended_cif *ecif,
			  unsigned bytes, unsigned flags,
#if defined(FFI_EXTRA_CIF_FIELDS)
              long unsigned flags2,
#endif /*FFI_EXTRA_CIF_FIELDS*/
			  /*@out@*/ unsigned *rvalue,
			  void (*fn)(void))
{
    debug(2,"ve: ffi_call_SYSV_example ('C' stub)\n");
    char* fn_stack = (char*)alloca(bytes);
    prep_args( fn_stack, ecif );
    /* asm copy mem reg area into registers */
    /* invoke fn in current stack frame ? (This is why we need assembler) */
}
void ffi_call_example(/*@dependent@*/ ffi_cif *cif,
	      void (*fn)(void),
	      /*@out@*/ void *rvalue,
	      /*@dependent@*/ void **avalue)
{
    extended_cif ecif;
    //UINT64 trvalue;     /* temporary rvalue */

    /* argument values */
    ecif.cif = cif;
    if( cif->nargs == 0 ) ecif.avalue = NULL; /* avalue is ignored */
    else ecif.avalue = avalue;
    /* return value */
    ecif.rvalue = rvalue;
    ffi_call_SYSV_example(ffi_prep_args, &ecif, cif->bytes, cif->flags,
#if defined(FFI_EXTRA_CIF_FIELDS)
            cif->flags2,
#endif /*FFI_EXTRA_CIF_FIELDS*/
            ecif.rvalue, fn);

    debug(2,"ve: ffi_call_example(ffi_cif*, void(*fn)(), void*rvalue, void**avalue)) ENDS %s\n      cif = %s\n      fn = %p, rvalue = %p, avalue=%p\n      %s",
            "(ffi_call_example)",
            ffi_cif_str(cif), fn, rvalue, avalue, ffi_avalues_str(cif,avalue));
}


void ffi_call(/*@dependent@*/ ffi_cif *cif,
	      void (*fn)(void),
	      /*@out@*/ void *rvalue,
	      /*@dependent@*/ void **avalue)
{
#define FFI_USE_ASSEMBLER 1
    debug(2,"ve: %s ffi_call(ffi_cif*, void(*fn)(), void*rvalue, void**avalue)) %s\n",
            "BEG",(FFI_USE_ASSEMBLER? "(sysv.S)":"(ffi_call_SYSV_example)"));
    debug(2,"cif = %s\n", ffi_cif_str(cif));
    debug(2, "fn = %p, rvalue = %p, avalue=%p\n", fn, rvalue, avalue);
    if(cif->nargs){
        FFI_ASSERT( avalue != NULL );
        debug(2, "avalue[0]=%p\n", *avalue);
#if 1 // debug
        if( cif->nargs && cif->arg_types[0]->type == FFI_TYPE_COMPLEX ){
            debug(5,"complex:arg_types[0] = %p\n",(void*)cif->arg_types[0]);
            ffi_type *t = cif->arg_types[0];
            debug(5,"ffi_call,1st arg complex sz:%u al:%u",
                    (unsigned)t->size, (unsigned)t->alignment);
            if(t->size == 32){
                debug(5,"going to load ldq\n");
                long double complex ldq = *(long double complex*)avalue[0];
                debug(5,"loaded ldq\n");
                long double re = ((long double*)avalue[0])[0];
                long double im = ((long double*)avalue[0])[1];
                debug(5," long double complex [0,1]=(%g,%g)",re,im);
                double r = creall(ldq); double i = cimagl(ldq);
                debug(5," long double complex creall,cimagl=(%g,%g)",r,i);
            }
            debug(5,"\n");
        }
#endif // debug
    }
    extended_cif ecif;
    UINT64 trvalue;     /* temporary rvalue */

    /* argument values */
    ecif.cif = cif;
    if( cif->nargs == 0 ) ecif.avalue = NULL; /* avalue is ignored */
    else ecif.avalue = avalue;

    /* return value */
    if( cif->rtype->type == FFI_TYPE_VOID )
        ecif.rvalue = &trvalue; /* rvalue is ignored */
#if !FFI_NOSTRUCTS
    /* If the return value is a struct and we don't have a */
    /* return value address then we need to make one	   */
    /* VE: this adress could be muxed into the arg passing area ? */
    else if (rvalue == NULL
            && (cif->rtype->type == FFI_TYPE_STRUCT)
            && cif->rtype->size > 8){
        ecif.rvalue = alloca(cif->rtype->size);
        debug(2,"struct! alloca(s%lua%u)-->ecif.rvalue=%p\n", (long unsigned)cif->rtype->size,
                (unsigned)cif->rtype->alignment, ecif.rvalue);
    }
#endif
    else{
        ecif.rvalue = rvalue; /* pointer to a chunk of memory that will hold the result
                                 of the function call (min 8==register size bytes).
                                 Caller must ensure correct alignment. */
    }
    if(avalue != NULL) debug(2,"      %s\n", ffi_avalues_str(cif,avalue));

    switch (cif->abi)
    {
        case FFI_SYSV:
#if FFI_USE_ASSEMBLER /* try out the sysv.S assembler code */
            ffi_call_SYSV(ffi_prep_args, &ecif, cif->bytes, cif->flags,
#if defined(FFI_EXTRA_CIF_FIELDS)
                    cif->flags2,
#endif /*FFI_EXTRA_CIF_FIELDS*/
                    ecif.rvalue, fn);
#else
            ffi_call_SYSV_example(ffi_prep_args, &ecif, cif->bytes, cif->flags,
#if defined(FFI_EXTRA_CIF_FIELDS)
                    cif->flags2,
#endif /*FFI_EXTRA_CIF_FIELDS*/
                    ecif.rvalue, fn);
#endif
            debug(3,"\n  rvalue[%s] : %p --> %s",
                    ffi_type_detail(cif->rtype,0U,1U),
                    (void*)ecif.rvalue,
                    ffi_avalue_str(cif->rtype,ecif.rvalue));
            break;
        default:
            FFI_ASSERT(0);
            break;
    }
    debug(3,"\n");

#if 0
    if (rvalue
            && cif->rtype->type == FFI_TYPE_STRUCT
            && return_type (cif->rtype) != FFI_TYPE_STRUCT)
        memcpy (rvalue, &trvalue, cif->rtype->size);
#endif
    debug(3,"ve: %s ffi_call(ffi_cif*, void(*fn)(), void*rvalue, void**avalue)) %s\n      cif = %s\n      fn = %p, rvalue = %p, avalue=%p\n      %s\n\n",
            "ENDS",(FFI_USE_ASSEMBLER? "(sysv.S)":"(ffi_call_SYSV_example)"),
            ffi_cif_str(cif), fn, rvalue, avalue, ffi_avalues_str(cif,avalue));
}

#if FFI_CLOSURES
extern void ffi_closure_ve (void);
extern void ffi_closure_ve_struct (void);
//extern void __ic_invalidate (void *line);
struct register_args
{
  /* Registers for argument passing.  */
  UINT64 gpr[NGREGARG];
};


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
 *    |(RSA)    \   retaddr+pad[16]  | (for ffi_closure_ve)
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
ffi_status
ffi_prep_closure_loc (ffi_closure *closure,
		      ffi_cif *cif,
		      void (*fun)(ffi_cif*cif, void*ret, void**args, void*user_data),
		      void *user_data,
		      void *codeloc)
{
    /* typedef struct {
     *   char tramp[FFI_TRAMPOLINE_SIZE];
     *   ffi_cif   *cif;
     *   void     (*fun)(ffi_cif*,void*,void**,void*);
     *   void      *user_data;
     * } ffi_closure;
     * 5d8:   00 00 00 00     bsic    %s12,0x0(0,%s12)
     * 5dc:   8c 00 0c 08
     * */

    static const unsigned char trampoline[32] = {
        // 0: lea %s12, asm_closure_foo@LO
        //-------------------- LO: 0x33aa33bb33cc33dd@LO
        0xdd, 0x33, 0xcc, 0x33, 0x00, 0x00, 0x0c, 0x06,
        // 8: and     %s12,%s12,(32)0
        0x00, 0x00, 0x00, 0x00, 0x60, 0x8c, 0x0c, 0x44,
        // 16: lea.sl  %s12, asm_closure_foo@HI
        //-------------------- HI: 0x33aa33bb33cc33dd@HI
        0xbb, 0x33, 0xaa, 0x33, 0x8c, 0x00, 0x8c, 0x06,
        // 32: bsic    %s12, (,%s12)
        // NOTE: must not modify caller's %lr return address
        0x00, 0x00, 0x00, 0x00, 0x8c, 0x00, 0x0c, 0x08
    };
    void (*dest)(void);
    char *tramp = closure->tramp;

    if (cif->abi != FFI_SYSV)
        return FFI_BAD_ABI;

    Argclass retclass = argclass(cif->rtype);
    assert(retclass == VE_REGISTER || retclass == VE_REFERENCE);
    if( VE_REGISTER )
        dest = ffi_closure_ve;
    else
        dest = ffi_closure_ve_struct;
    // NB: could put this into a flags variable within cif.

    memcpy(tramp, trampoline, sizeof(trampoline));
    *(UINT32 *)(tramp + 0)  =/* LO */(UINT32)(uintptr_t)dest;
    *(UINT32 *)(tramp + 16) =/* HI */(UINT32)((uintptr_t)dest>>32);

    // asm_FOO
    closure->cif = cif;
    closure->fun = fun;
    closure->user_data = user_data;

    /* Flush the icache.  How on VE? */
    //asm volatile ("ocbwb %0,0; synco; icbi %1,0; synci" : : "r" (tramp), "r"(codeloc));
    return FFI_OK;
}


int FFI_HIDDEN
ffi_closure_inner(ffi_cif *cif,
			 void (*fun)(ffi_cif*, void*, void**, void*),
			 void *user_data,
			 void *rvalue,
			 struct register_args *reg_args,
			 char *argp)
{
    /* cif->flags are set by machdep, above, for rtype */
    /* cif->flags2 are set by machdep, above, for register-arg status */
    char* const argp0 = argp;
    void **avalue;
    ffi_type **arg_types;
    long i, avn;
    int gprcount; //, ngpr; //ssecount, nsse;
    int flags;

    // oops: I follow x86 in providing a separate reg_args area (FIXME)
    //argp+=64;
    // Better: copy the asm-saved registers into argp, so they join up
    //         directly with the non-reg argp.  Easier to deal with
    //         contiguours reg and non-reg memory [avoid split arg issues]
    FFI_ASSERT( sizeof(struct register_args) == 8*8 );
    debug(2, " reg_args@%p:\n", &reg_args->gpr[0]);
    for(unsigned i=0U; i<8U; i+=4U){
        debug(2," 0x%08lx 0x%08lx 0x%08lx 0x%08lx\n",
                reg_args->gpr[i+0], reg_args->gpr[i+1],
                reg_args->gpr[i+2], reg_args->gpr[i+3]);
    }
    debug(2, "\nargp0@%p:\n",(void const*)argp0);
    for(unsigned i=0U; i<16U; i+=4U){
        debug(2," 0x%08lx 0x%08lx 0x%08lx 0x%08lx\n",
                *(UINT64*)(argp+i*32+ 0), *(UINT64*)(argp+i*32+ 8),
                *(UINT64*)(argp+i*32+16), *(UINT64*)(argp+i*32+24));
    }
    debug(2,"\n");
    memcpy( argp, &reg_args->gpr[0], sizeof(struct register_args) );
    // DESTROY the original args pointer (it can be re-used for ret-val processing)
    reg_args = (struct register_args*)(void*)argp;

    //argp += sizeof(struct register_args);
    // Now our avalues memory looks like [ register args | non-register args ]
    // and we can point into this area WITH objects extending naturally past
    // register args into non-register area.

    debug(2, " reg_args[%ld]:%p ", (SINT64)((char*)rvalue-(char*)reg_args), (void*)reg_args);
    debug(2, " rvalue:%p rvalue--argp:%ldb", (void*)rvalue, (SINT64)((char*)argp-(char*)rvalue));
    debug(2, " argp@%p\n", (void*)argp);

    gprcount /*= ssecount*/ = 0;
    UINT64 flags2 = cif->flags2;
    // VE docs class of rtype is never BOTH
    //if( argclass(cif->rtype) == VE_REFERENCE )
    if( cif->rtype->type == FFI_TYPE_STRUCT ) /* equiv? */
    //if( (flags2&0xFF) == 0x80 ) /* equiv. */
    {
        FFI_ASSERT( cif->rtype->type == FFI_TYPE_STRUCT );
        FFI_ASSERT( argclass(cif->rtype) == VE_REFERENCE );
        FFI_ASSERT( (flags2&0xFF) == 0x80 );
        /* use %s0 as a pointer register for return value */
        void *r = (void *)(uintptr_t)reg_args->gpr[gprcount++];
        //*(void **)rvalue = r;
        rvalue = r;
        //flags = (sizeof(void *) == 4 ? UNIX64_RET_UINT32 : UNIX64_RET_INT64);
    }else{
        FFI_ASSERT( cif->rtype->type != FFI_TYPE_STRUCT );
        FFI_ASSERT( argclass(cif->rtype) != VE_REFERENCE );
        FFI_ASSERT( (flags2&0xFF) != 0x80 );
    }
    avn = cif->nargs;
    flags = cif->flags;
    avalue = alloca(avn * sizeof(void *));
    arg_types = cif->arg_types;
    // VE: we are loaded user args into avalue[].  If BOTH, then let's
    //     just take it from register and ignore the memory copy
    for (i = 0; i < avn; ++i)
    {
        //enum x86_64_reg_class classes[MAX_CLASSES];
        //n = examine_argument (arg_types[i], classes, 0, &ngpr, &nsse);
        ffi_type* const type = arg_types[i];
        size_t const UNITS_PER_WORD = 8;
        size_t const z = type->size;
        long align = type->alignment;
        size_t n=0;
        //Argclass clas = argclass(type);
        if( type->type != FFI_TYPE_STRUCT ){
            n = (z + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
        }
        debug(2," i%dsz%lun%ug%dt%d",(int)i,(unsigned long)z,(unsigned)n,
                (int)gprcount,(int)type->type);
        fflush(stdout);
        foo(); /* breakpoint */
        if (n == 0 ){ // ve struct passed as ptr, which can be in reg
            debug(3," n==0 "); fflush(stdout);
            FFI_ASSERT( argclass(type) == VE_REFERENCE );
            //if (align < 8){ align = 8; }
            //argp = (void *) FFI_ALIGN (argp, align);
            if( gprcount < NGREGARG ){
                // VE caller passes "pointer to struct" in register
                // *** libfii always assumes avalues points "at the object" ***
                // reg_args: VE stores a POINTER-TO-MEM here.
                // this pointer-to-mem must end up in avalues[i].
                //if (align < 8){ align = 8; }
                //argp = (void *) FFI_ALIGN (argp, align);
                // p ((Int*)(**args))->x within I_III_simulator is correct,
                // but it should work as Int a = *(Int*)(*args++)
                avalue[i] = (void*)reg_args->gpr[gprcount];
                debug(2," a%ds%d&R[%d]=0x%lx", (int)align, (int)z, (int)gprcount,
                    (long)&reg_args->gpr[gprcount]);
                //int* pint = *(int*)avalue[i];
                //debug(2," *%d", pint[0]);
                ++gprcount; argp += 8;
            }else{
                argp = (void *) FFI_ALIGN (argp, 8); // alignment of void*
                debug(2," argp%d=%p",(int)(argp-argp0),(void*)argp);
                debug(2," *argp%d=%p",(int)(argp-argp0),*(void**)argp);
                avalue[i] = *(void**)argp;
                debug(2," a%ds%d&A[%d]=%p", (int)align, (int)z,
                        (int)(argp-argp0), *(void**)argp); fflush(stdout);
                argp += sizeof(void*); 
                debug(2," -->argp%d", (int)(argp-argp0));
            }
            debug(2,"%s","\n");
        }else if(type->type == FFI_TYPE_COMPLEX){ /* {float/double/long double} complex */
            FFI_ASSERT(n>0);
            /* VE passes complex as though they were two separate REGISTER scalars */
            UINT64 *pre, *pim;
            n >>= 1;    /* 1|2|4 becomes 0|1|2 */
            FFI_ASSERT(n==0 || n==1 || n==2 );
            int const gbump = (n<=1? 1: 2);
            int const abump = gbump*8;
            if( n==0 ){ /* 2 packed floats, passed as 2 separate REGISTER args */
                if(gprcount < NGREGARG) { pre = &reg_args->gpr[gprcount]; gprcount+=gbump; argp+=abump;}
                else                    { pre = (UINT64*)argp; argp+=abump; }
                if(gprcount < NGREGARG) { pim = &reg_args->gpr[gprcount]; gprcount+=gbump; argp+=abump;}
                else                    { pim = (UINT64*)argp; argp+=abump; }
                *pre = (*pim & 0xffffFFFF00000000UL) | (*pre >> 32);
                avalue[i] = (void*)pre;
            }else if( n==1 ){
                if(gprcount < NGREGARG) { pre = &reg_args->gpr[gprcount]; gprcount+=1; argp+=8;}
                else                    { pre = (UINT64*)argp; argp+=8; }
                if(gprcount < NGREGARG) { pim = &reg_args->gpr[gprcount]; gprcount+=1; argp+=8;}
                else                    { pim = (UINT64*)argp; argp+=8; }
                // these must occupy consecutive memory locations
                // (no problem if reg_args memory is directly before argp area)
                if(pim != pre + 1){
                    FFI_ASSERT("TBD: split double complex args --> single mem region"==NULL);
                }
                avalue[i] = (void*)pre;
            }else{
                FFI_ASSERT( n==2 && type->size==32 );
                if((gprcount & 1)){ ++gprcount; argp+=8; }
                if(gprcount<NGREGARG){pre = &reg_args->gpr[gprcount]; gprcount+=2; argp+=16;}
                else                 {pre = (UINT64*)argp; argp+=16;}
                if(gprcount<NGREGARG){pim = &reg_args->gpr[gprcount]; gprcount+=2; argp+=16;}
                else                 {pim =(UINT64*) argp; argp+=16;}
                // these must occupy consecutive memory locations
                if(pim != pre + 1){
                    FFI_ASSERT("TBD: split longdouble complex args --> single mem region"==NULL);
                }
                avalue[i] = (void*)pre;
            }
        }else if (gprcount + n > NGREGARG){
            FFI_ASSERT(n>0);
            // IF n==0 ALLOCA ???
            /* Stack args *always* at least 8 byte aligned. */
            if (align < 8){ align = 8; }
            /* Pass this argument in memory.  */
            long argp_a = (argp-argp0);
            argp = (void *) FFI_ALIGN (argp, align);
            if( type->type == FFI_TYPE_FLOAT ){
                argp+=4; // single floats are held in
                //          little-endian MSB, unlike ints
            }
            if( (argp-argp0) != argp_a ){
                debug(2," a%dargp%ld->%ld",(long)argp_a,(long)(argp-argp0));
            }
            debug(2," *argp@%p=%s",(void*)argp,ffi_avalue_str(type,argp));

            avalue[i] = argp;
            debug(2," a%lds%luA[%d]", (long)align, (long unsigned)z, (int)(argp-argp0));
            argp += z; // ok for struct?
            debug(2," -->argp%d", (int)(argp-argp0));
            debug(2,"%s","\n");
        }else if (n == 1) { /* The argument might use a single register. */
            /* we can use arg address directly. */
            // NO FFI_ASSERT( align >= 8 );
            debug(3," n==1 "); fflush(stdout);
            avalue[i] = &reg_args->gpr[gprcount];
            if(type->type == FFI_TYPE_FLOAT){
                /* swap float from MSBs to LSBs */
                reg_args->gpr[gprcount] >>= 32;
                debug(2," flt%d=%f",(int)gprcount,
                        *(float*)(&reg_args->gpr[gprcount]));
                debug(2," flt%d'=%f",(int)gprcount,
                        *((float*)(&reg_args->gpr[gprcount])+1));
            }
            debug(2," *argp@%p=%s",(void*)argp,ffi_avalue_str(type,argp));
            debug(2," a%ds%d&R[%d]=0x%lx\n", align, (int)z, gprcount,
                    &reg_args->gpr[gprcount]);
            gprcount += n;
            argp += n*8;
        }else{ /* o/w alloc space to make them consecutive. */
            /* n==2 or 4? we can use arg address directly. */
            FFI_ASSERT( align >= 8 );
            FFI_ASSERT( n==2 || n==4 );
            // long scalars must begin on even-numbered reg/mem
            if((gprcount & 1)){ ++gprcount; argp+=8; }
            debug(2,"%s","++g");
#if 0
           C// On VE regargs can always be alloa'ed? Why this?
            char *a = alloca (n*8);
            FFI_ASSERT(((uintptr_t)a & 0x0F) == 0U); //align 16
            unsigned int j;
            avalue[i] = a;
            for (j = 0; j < n; j++, a += 8)
            {
                memcpy (a, &reg_args->gpr[gprcount++], 8);
            }
#else
            avalue[i] = &reg_args->gpr[gprcount];
            debug(2," *argp@%p=%s",(void*)argp,ffi_avalue_str(type,argp));
            if(type->type == FFI_TYPE_LONGDOUBLE){
                UINT64 const tmp = reg_args->gpr[gprcount];
                reg_args->gpr[gprcount] = reg_args->gpr[gprcount+1];
                reg_args->gpr[gprcount+1] = tmp;
            }
            debug(2," a%ds%d&R[%d..%d]\n", align, z, gprcount,
                    gprcount+n-1);
            gprcount += n; argp += n*8;
#endif
        }
    }

    /* Tell assembly how to perform return type promotions.  */
    /*debug(2, "_inner flags=rtype?=%d\n",flags);*/
    FFI_ASSERT( cif->flags == cif->rtype->type );
    if( cif->rtype->type == FFI_TYPE_COMPLEX ){
        int rsz = cif->rtype->size;
        debug(2," COMPLEX sz=%d flags=0x%lx",rsz,(UINT64)flags);
        flags |= (cif->rtype->size << 8); /* 8,16,32 */
        debug(2," -> 0x%lx\n",(UINT64)flags);
    }
    /* Invoke the closure.  */
    fun (cif, rvalue, avalue, user_data);


    return flags;
}

#if 0 // sh approach (?)
/* Basically the trampoline invokes ffi_closure_SYSV, and on
 * entry, r3 holds the address of the closure.
 * After storing the registers that could possibly contain
 * parameters to be passed into the stack frame and setting
 * up space for a return value, ffi_closure_SYSV invokes the
 * following helper function to do most of the work.
 */

int
ffi_closure_helper_SYSV (ffi_closure *closure, UINT64 *rvalue,
			 UINT64 *pgr, UINT64 *pfr, UINT64 *pst)
{
    void **avalue;
    ffi_type **p_arg;
    int i, avn;
    int greg; //, freg;
    ffi_cif *cif;
    //int fpair = -1;

    cif = closure->cif;
    avalue = alloca (cif->nargs * sizeof (void *));

    /* Copy the caller's structure return value address so that the closure
       returns the data directly to the caller.  */
    if (return_type (cif->rtype) == FFI_TYPE_STRUCT) {
        rvalue = (UINT64 *) *pgr;
        greg = 1;
    }else
        greg = 0;

    //freg = 0;
    cif = closure->cif;
    avn = cif->nargs;

    /* Grab addresses of the arguments from the stack frame.  */
    for (i = 0, p_arg = cif->arg_types; i < avn; i++, p_arg++)
    {
        size_t z;
        void *p;

        z = (*p_arg)->size;
        if (z < sizeof (UINT32)) {
            p = pgr + greg++;
            switch ((*p_arg)->type)
            {
                case FFI_TYPE_SINT8:
                case FFI_TYPE_UINT8:
                case FFI_TYPE_SINT16:
                case FFI_TYPE_UINT16:
                case FFI_TYPE_STRUCT:
                    avalue[i] = p;
                    break;

                default:
                    FFI_ASSERT(0);
            }
        }else if (z == sizeof (UINT32)){
#if 0
            if ((*p_arg)->type == FFI_TYPE_FLOAT){
                if (freg < NFREGARG - 1){
                    if (fpair >= 0){
                        avalue[i] = (UINT32 *) pfr + fpair;
                        fpair = -1;
                    }else{
                        fpair = freg;
                        avalue[i] = (UINT32 *) pfr + (1 ^ freg);
                        freg += 2;
                    }
                }else
                    avalue[i] = pgr + greg;
            }else
#endif
                avalue[i] = pgr + greg;
            greg++;
        }else if ((*p_arg)->type == FFI_TYPE_DOUBLE){
            //if (freg + 1 >= NFREGARG)
            //    avalue[i] = pgr + greg;
            //else{
                avalue[i] = pfr + (freg >> 1);
                freg += 2;
            //}
            greg++;
        }else{
            int n = (z + sizeof (UINT64) - 1) / sizeof (UINT64);

            avalue[i] = pgr + greg;
            greg += n;
        }
    }

    (closure->fun) (cif, rvalue, avalue, closure->user_data);

    /* Tell ffi_closure_SYSV how to perform return type promotions.  */
    return return_type (cif->rtype);
}
#endif // sh approach ?
#endif /* FFI_CLOSURES */
/* vim: set ts=4 sw=4 et ai: */
