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

#define DEBUG_LEVEL   0
#define FIRST_ARG_BYTE  176 /* 22*8: fp, ret | %tp %got %plt %s17 16*%s18..33 */
#define ROUND_UP(v, a)  (((size_t)(v) + (a) - 1) & ~((a) - 1))

#define debug(lvl, x...) do { if (lvl <= DEBUG_LEVEL) { printf(x); } } while (0)

#define NGREGARG 8
//#define NFREGARG 12

/* ffi_type is {size_t size; u16 alignment; u16 type; _ffi_type **elements;} */
char const* ffi_type_str( ffi_type const* const t ){
    char * ret = "??";
    switch(t->type){
        case( FFI_TYPE_VOID       ): ret = "void"; /*0*/
        case( FFI_TYPE_INT        ): ret = "int"; /*1*/
        case( FFI_TYPE_FLOAT      ): ret = "float"; /*2*/
        case( FFI_TYPE_DOUBLE     ): ret = "double"; /*3*/
        case( FFI_TYPE_LONGDOUBLE ): ret = "Ldouble"; /*4*/
        case( FFI_TYPE_UINT8      ): ret = "u8"; /*5*/
        case( FFI_TYPE_SINT8      ): ret = "s8"; /*6*/
        case( FFI_TYPE_UINT16     ): ret = "u16"; /*7*/
        case( FFI_TYPE_SINT16     ): ret = "s16"; /*8*/
        case( FFI_TYPE_UINT32     ): ret = "u32"; /*9*/
        case( FFI_TYPE_SINT32     ): ret = "s32"; /*10*/
        case( FFI_TYPE_UINT64     ): ret = "u64"; /*11*/
        case( FFI_TYPE_SINT64     ): ret = "s64"; /*12*/
        case( FFI_TYPE_STRUCT     ): ret = "struct"; /*13*/
        case( FFI_TYPE_POINTER    ): ret = "ptr"; /*14*/
        case( FFI_TYPE_COMPLEX    ): ret = "complex"; /*15*/
        default: ;
    }
    return ret;
}

static char buf_type[1024];
char const* ffi_type_detail( ffi_type const* const t ){
    char *buf = &buf_type[0];
    int rem_len = 1024;
#define DPRINT(...) do \
    { \
        int n = snprintf(buf, rem_len, __VA_ARGS__); \
        if( n > rem_len ){ rem_len = 0; } \
        else { buf+=n; rem_len-=n; } \
    } while(0)
    DPRINT("%s(s%llu,a%u)", ffi_type_str(t), (unsigned long long)t->size, (unsigned)t->alignment);
    return &buf_type[0];
#undef DPRINT
}

static char buf_cif[1024];
char const* ffi_cif_str( ffi_cif const* cif){
    char *buf = &buf_cif[0];
    int rem_len = 1024;
#define DPRINT(...) do \
    { \
        int n = snprintf(buf, rem_len, __VA_ARGS__); \
        if( n > rem_len ){ rem_len = 0; } \
        else { buf+=n; rem_len-=n; } \
    } while(0)
    DPRINT("ffi_cif={abi:%d, nargs:%u, arg_types:", cif->abi, cif->nargs);
    if(cif->arg_types==NULL) DPRINT("NULL");
    else if(cif->nargs <= 0) DPRINT("{}");
    else{
        /* ffi_type **arg_types */
        for(int i=0; i<cif->nargs; ++i)
            DPRINT((i==0?"{":","),ffi_type_detail(cif->arg_types[i]));
        DPRINT("}");
    }
    DPRINT(", rtype=");
    if(cif->rtype==NULL) DPRINT("NULL");
    else DPRINT("%p[%s]", (void*)cif->rtype, ffi_type_detail(cif->rtype));
    DPRINT(", bytes:%u, flags:%u"
#if defined(FFI_EXTRA_CIF_FIELDS)
            " [,extra]"
#endif /*FFI_EXTRA_CIF_FIELDS*/
            ") ",
            cif->bytes, cif->flags);
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

typedef enum ve_argclass {
    VE_REGISTER = 0,
    VE_REFERENCE = 1,
    VE_BOTH
} Argclass;

static Argclass argclass (ffi_type *arg)
{
    Argclass ret = VE_REGISTER; /* most things */

    if (arg->type == FFI_TYPE_STRUCT) /* or union or array */
        ret = VE_REFERENCE;

#if defined FFI_TARGET_SPECIFIC_VARIADIC
    else if( arg->type == FFI_TARGET_SPECIFIC_VARIADIC /* where defined? */
            /* || prototype-less ?? */
           ) ret = VE_BOTH;
#endif

    return ret;
}

/* VE ret type can use argclass and deal with VE_REGISTER or VE_REFERENCE.
VE_REFERENCE: %s0 holds [lowest] addr of ret value (usually 176(%fp)
>=176(%fp) in callee-frame holds the struct data.
VE_REGISTER returns use %s0..%s7, stored same way as for calling.

NEC VE ABI 0.10 does not support small-struct optimizations. */


/* ffi_prep_args is called by the assembly routine once stack space
   has been allocated for the function's arguments */

void ffi_prep_args(char *stack, extended_cif *ecif)
{
    register unsigned int i;
    register unsigned int avn;
    register void **p_argv;
    register char *argp;
    register ffi_type **p_arg;

    /* ecif = { ffi_cif*, void*rvalue, void**avalue } is used in callback from assembly */
    debug(1, "%s: stack = %p, ecif = %p, bytes = %u, rvalue=%p, avalue=%p\n", __FUNCTION__,
            stack, ecif, ecif->cif->bytes, ecif->rvalue, ecif->avalue);

    argp = stack;

#if !defined(FFI_NO_STRUCTS)
    /* When callee returns an aggregate (VE_REFERENCE), the caller:
       - povides return memory on its stack and set %s0 to caller-176%(sp).
       and callee also set %s0 to the address of the returned struct.

       In effect, a returned struct becomes a hidden "first argument".
       
       Now does libffi stick this "hidden arg" into arg_types?
       If not, how is ffi_cif.rtype used? */
    if (ecif->cif->rtype->type == FFI_TYPE_STRUCT)
    {
        /* wrong for NEC VE (struct might be larger */
        // size_t z = ecif->rsize; // ??
	    // how ??? memcpy (argp, *p_argv, z); /* TODO: handle struct alignment > 8 */
        argp += sizeof (UINT64);
    }
#endif

    avn = ecif->cif->nargs;
    p_argv = ecif->avalue;

    for (i = 0, p_arg = ecif->cif->arg_types; i < avn; i++, p_arg++, p_argv++)
    {
        size_t z;
        int align;

        int type = (*p_arg)->type;

        Argclass cls = argclass(*p_arg); /* VE_REGISTER/REFERENCE/BOTH */
        if( cls == VE_REGISTER
                /* and there are enough registers? who checks? maybe machdep? */
          ){
            continue;
        }

        z = (*p_arg)->size;
        align = (*p_arg)->alignment;
        if (z < sizeof (UINT64))
        {
            switch (type)
            {
                case FFI_TYPE_SINT8:
                    *(SINT64 *) argp = (SINT64) *(SINT8 *)(*p_argv);
                    break;

                case FFI_TYPE_UINT8:
                    *(UINT64 *) argp = (UINT64) *(UINT8 *)(*p_argv);
                    break;

                case FFI_TYPE_SINT16:
                    *(SINT64 *) argp = (SINT64) *(SINT16 *)(*p_argv);
                    break;

                case FFI_TYPE_UINT16:
                    *(UINT64 *) argp = (UINT64) *(UINT16 *)(*p_argv);
                    break;
                case FFI_TYPE_STRUCT:
#if defined(FFI_NO_STRUCTS)
                    FFI_ASSERT(NULL=="not expecting any structs");
#else
                    debug(3,"prep_args: NEC VE does not optimize small structs (no-op)");
                    //memcpy (argp, *p_argv, z); /* TODO: handle struct alignment > 8 */
#endif
                    break;
                default:
                    FFI_ASSERT(0);
            }
            argp += sizeof (UINT64);
        }
      else if (z == sizeof (UINT32) && align == sizeof (UINT32))
      {
          switch ((*p_arg)->type)
          {
              case FFI_TYPE_INT:
              case FFI_TYPE_SINT32:
                  *(SINT64 *) argp = (SINT64) *(SINT32 *) (*p_argv);
                  break;

              case FFI_TYPE_FLOAT:
              case FFI_TYPE_POINTER:
              case FFI_TYPE_UINT32:
#if !defined(FFI_NO_STRUCTS) 
              case FFI_TYPE_STRUCT:
#endif
                  *(UINT64 *) argp = (UINT64) *(UINT32 *) (*p_argv);
                  break;

              default:
                  FFI_ASSERT(0);
                  break;
          }
          argp += sizeof (UINT64);
      }
        else if (z == sizeof (UINT64)
                && align == (int)sizeof (UINT64)
                && ((intptr_t) *p_argv & (sizeof (UINT64) - 1)) == 0)
        {
            *(UINT64 *) argp = *(UINT64 *) (*p_argv);
            argp += sizeof (UINT64);
        }
        else
        {
            int n = (z + sizeof (UINT64) - 1) / sizeof (UINT64);

            memcpy (argp, *p_argv, z);
            argp += n * sizeof (UINT64);
        }
    }

    return;
}

/* Perform machine dependent cif processing */
ffi_status ffi_prep_cif_machdep(ffi_cif *cif)
{
    int i;
    int size, type;
    int n;
    int greg;
    //int fpair = -1;
#if defined(FFI_EXTRA_CIF_FIELDS)
    int j=0;
    int m;
#endif

    debug(2,"ve: ffi_prep_cif_machdep(ffi_cif*) BEGIN\n%s\n", ffi_cif_str(cif));
    /*greg = (argclass(cif->rtype) == VE_REFERENCE ? 1 : 0);*/
    greg = argclass(cif->rtype);
    /* VE_REGISTER --> 0
       return value in %s0.., no regs implicated for the call */
    /* VE_REFERENCE --> 1
       caller provides space on stack and passes address in %s0 as if
       this were a "hidden" first argument to the callee */
    FFI_ASSERT(greg != VE_BOTH);
#if defined(FFI_EXTRA_CIF_FIELDS)
    cif->flags2 = 0;
#endif /*FFI_EXTRA_CIF_FIELDS*/

    /* ? which args get passed in register */
    for (i = 0; i < cif->nargs; ++i)
    {
        type = (cif->arg_types)[i]->type;
        switch (type)
        {
            case FFI_TYPE_LONGDOUBLE:
                if(greg + (greg%2) + 2 >= NGREGARG) /* 2 gregs & must start in even greg */
                    debug(1,"ve: ffi_prep_cif_machdep(ffi_cif*) long double TBD");
#ifdef FFI_DEBUG
                ffi_stop_here();
#endif
                break;

            default:
                size = (cif->arg_types)[i]->size;
                if (size < sizeof (UINT64))
                    cif->bytes += sizeof (UINT64) - size;
                n = (size + sizeof (UINT64) - 1) / sizeof (UINT64);
                if (greg >= NGREGARG)
                    continue;
                else if (greg + n - 1 >= NGREGARG)
                    greg = NGREGARG;
                else
                    greg += n;
#if defined(FFI_EXTRA_CIF_FIELDS)
                for (m = 0; m < n; m++)
                    cif->flags2 += FFI_TYPE_INT << (2 * j++);
#endif /*FFI_EXTRA_CIF_FIELDS*/
                break;
        }
    }

    /* Set the return type flag */
    switch (cif->rtype->type)
    {
#if defined(FFI_NO_STRUCTS) && !FFI_NO_STRUCTS
        case FFI_TYPE_STRUCT: /* there is no special handling for small-structs*/
            cif->bytes += 8;  /* one pointer ?? */
#endif
        case FFI_TYPE_VOID:
        case FFI_TYPE_FLOAT:
        case FFI_TYPE_DOUBLE:
        case FFI_TYPE_SINT64:
        case FFI_TYPE_UINT64:
            cif->flags = cif->rtype->type;
            break;

        default:
            cif->flags = FFI_TYPE_INT;
            break;
    }

    /* VE ABI 0.32 requires stack frame size & alignment of 16 */
    FFI_ALIGN( cif->bytes, 16 );
    debug(2,"ve: ffi_prep_cif_machdep(ffi_cif*) DONE (cif->bytes=%lu)\n%s\n",
            (unsigned long)cif->bytes, ffi_cif_str(cif));
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

void ffi_call(/*@dependent@*/ ffi_cif *cif, 
	      void (*fn)(void), 
	      /*@out@*/ void *rvalue, 
	      /*@dependent@*/ void **avalue)
{
    /* We might not need an extended_cif at all */
    debug(2,"ve: ffi_call(ffi_cif*, void(*fn)(), void*rvalue, void**avalue)) BEGINS\n cif = %s\n fn = 0x%lx\n rvalue = 0x%lx\n avalue=0x%lx\n", ffi_cif_str(cif), (long)fn, (long)rvalue, (long(avalue));
    extended_cif ecif;
    UINT64 trvalue;     /* temporary rvalue */

    /* argument values */
    ecif.cif = cif;
    if( cif->nargs == 0 ) ecif.avalue = NULL; /* avalues is ignored */
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
            && cif->rtype->size > 8)
        ecif.rvalue = alloca(cif->rtype->size);
#endif
    else{
        ecif.rvalue = rvalue; /* pointer to a chunk of memory that will hold the result
                                 of the function call (min 8==register size bytes).
                                 Caller must ensure correct alignment. */
    }

    switch (cif->abi) 
    {
        case FFI_SYSV:
            ffi_call_SYSV(ffi_prep_args, &ecif, cif->bytes, cif->flags,
#if defined(FFI_EXTRA_CIF_FIELDS)
                    cif->flags2,
#endif /*FFI_EXTRA_CIF_FIELDS*/
                    ecif.rvalue, fn);
            break;
        default:
            FFI_ASSERT(0);
            break;
    }

#if 0
    if (rvalue
            && cif->rtype->type == FFI_TYPE_STRUCT
            && return_type (cif->rtype) != FFI_TYPE_STRUCT)
        memcpy (rvalue, &trvalue, cif->rtype->size);
#endif
    debug(2,"ve: ffi_call(ffi_cif*, void(*fn)(), void*rvalue, void**avalue)) ENDS\n cif = %s\n fn = 0x%lx\n rvalue = 0x%lx\n avalue=0x%lx\n", ffi_cif_str(cif), (long)fn, (long)rvalue, (long(avalue));
}

#if FFI_CLOSURES
#error "NEC VE closures TBD"
extern void ffi_closure_SYSV (void);
extern void __ic_invalidate (void *line);

ffi_status
ffi_prep_closure_loc (ffi_closure *closure,
		      ffi_cif *cif,
		      void (*fun)(ffi_cif*, void*, void**, void*),
		      void *user_data,
		      void *codeloc)
{
  unsigned int *tramp;

  if (cif->abi != FFI_SYSV)
    return FFI_BAD_ABI;

  tramp = (unsigned int *) &closure->tramp[0];
  /* Since ffi_closure is an aligned object, the ffi trampoline is
     called as an SHcompact code.  Sigh.
     SHcompact part:
     mova @(1,pc),r0; add #1,r0; jmp @r0; nop;
     SHmedia part:
     movi fnaddr >> 16,r1; shori fnaddr,r1; ptabs/l r1,tr0
     movi cxt >> 16,r1; shori cxt,r1; blink tr0,r63  */
#ifdef __LITTLE_ENDIAN__
  tramp[0] = 0x7001c701;
  tramp[1] = 0x0009402b;
#else
  tramp[0] = 0xc7017001;
  tramp[1] = 0x402b0009;
#endif
  tramp[2] = 0xcc000010 | (((UINT32) ffi_closure_SYSV) >> 16) << 10;
  tramp[3] = 0xc8000010 | (((UINT32) ffi_closure_SYSV) & 0xffff) << 10;
  tramp[4] = 0x6bf10600;
  tramp[5] = 0xcc000010 | (((UINT32) codeloc) >> 16) << 10;
  tramp[6] = 0xc8000010 | (((UINT32) codeloc) & 0xffff) << 10;
  tramp[7] = 0x4401fff0;

  closure->cif = cif;
  closure->fun = fun;
  closure->user_data = user_data;

  /* Flush the icache.  */
  asm volatile ("ocbwb %0,0; synco; icbi %1,0; synci" : : "r" (tramp),
		"r"(codeloc));

  return FFI_OK;
}

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
  int greg, freg;
  ffi_cif *cif;
  //int fpair = -1;

  cif = closure->cif;
  avalue = alloca (cif->nargs * sizeof (void *));

  /* Copy the caller's structure return value address so that the closure
     returns the data directly to the caller.  */
  if (return_type (cif->rtype) == FFI_TYPE_STRUCT)
    {
      rvalue = (UINT64 *) *pgr;
      greg = 1;
    }
  else
    greg = 0;

  freg = 0;
  cif = closure->cif;
  avn = cif->nargs;

  /* Grab the addresses of the arguments from the stack frame.  */
  for (i = 0, p_arg = cif->arg_types; i < avn; i++, p_arg++)
    {
      size_t z;
      void *p;

      z = (*p_arg)->size;
      if (z < sizeof (UINT32))
	{
	  p = pgr + greg++;

	  switch ((*p_arg)->type)
	    {
	    case FFI_TYPE_SINT8:
	    case FFI_TYPE_UINT8:
	    case FFI_TYPE_SINT16:
	    case FFI_TYPE_UINT16:
	    case FFI_TYPE_STRUCT:
#ifdef __LITTLE_ENDIAN__
	      avalue[i] = p;
#else
	      avalue[i] = ((char *) p) + sizeof (UINT32) - z;
#endif
	      break;

	    default:
	      FFI_ASSERT(0);
	    }
	}
      else if (z == sizeof (UINT32))
	{
	  if ((*p_arg)->type == FFI_TYPE_FLOAT)
	    {
	      if (freg < NFREGARG - 1)
		{
		  if (fpair >= 0)
		    {
		      avalue[i] = (UINT32 *) pfr + fpair;
		      fpair = -1;
		    }
		  else
		    {
#ifdef __LITTLE_ENDIAN__
		      fpair = freg;
		      avalue[i] = (UINT32 *) pfr + (1 ^ freg);
#else
		      fpair = 1 ^ freg;
		      avalue[i] = (UINT32 *) pfr + freg;
#endif
		      freg += 2;
		    }
		}
	      else
#ifdef __LITTLE_ENDIAN__
		avalue[i] = pgr + greg;
#else
		avalue[i] = (UINT32 *) (pgr + greg) + 1;
#endif
	    }
	  else
#ifdef __LITTLE_ENDIAN__
	    avalue[i] = pgr + greg;
#else
	    avalue[i] = (UINT32 *) (pgr + greg) + 1;
#endif
	  greg++;
	}
      else if ((*p_arg)->type == FFI_TYPE_DOUBLE)
	{
	  if (freg + 1 >= NFREGARG)
	    avalue[i] = pgr + greg;
	  else
	    {
	      avalue[i] = pfr + (freg >> 1);
	      freg += 2;
	    }
	  greg++;
	}
      else
	{
	  int n = (z + sizeof (UINT64) - 1) / sizeof (UINT64);

	  avalue[i] = pgr + greg;
	  greg += n;
	}
    }

  (closure->fun) (cif, rvalue, avalue, closure->user_data);

  /* Tell ffi_closure_SYSV how to perform return type promotions.  */
  return return_type (cif->rtype);
}
#endif /* FFI_CLOSURES */
/* vim: set ts=4 sw=4 et ai: */
