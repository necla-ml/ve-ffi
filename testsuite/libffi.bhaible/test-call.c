/**
  Copyright 1993 Bill Triggs <Bill.Triggs@inrialpes.fr>
  Copyright 1995-2017 Bruno Haible <bruno@clisp.org>

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
**/

/* { dg-do run } */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ffi.h>
#include "alignof.h"
#ifdef VARIADIC_SUPPORT
#include "stdarg.h"
#endif

#include "testcases.c"

/* libffi testsuite local changes -------------------------------- */
#ifdef DGTEST
/* Redefine exit(1) as a test failure */
#define exit(V) (void)((V) ? (abort(), 1) : exit(0))
int count = 0;
char rbuf1[2048];
char rbuf2[2048];
#define FPRINTF(STREAM,FMT,ARGS...)			\
  { fprintf(STREAM, FMT, ##ARGS) ;			\
    switch (count++)					\
      {							\
        case 0:						\
        case 1:						\
	  sprintf(&rbuf1[strlen(rbuf1)], FMT, ##ARGS);	\
	  break ;					\
        case 2:						\
	  sprintf(rbuf2, FMT, ##ARGS);	                \
	  break;					\
        case 3:						\
	  sprintf(&rbuf2[strlen(rbuf2)], FMT, ##ARGS);	\
	  if (strcmp (rbuf1, rbuf2)) abort();		\
	  break;					\
      }							\
  }
#else
#define FPRINTF fprintf
#endif
/* --------------------------------------------------------------- */

/* Definitions that ought to be part of libffi. */
static ffi_type ffi_type_char;
#define ffi_type_slonglong ffi_type_sint64
#define ffi_type_ulonglong ffi_type_uint64

/* libffi does not support arrays inside structs. */
#define SKIP_EXTRA_STRUCTS

#define FFI_PREP_CIF(cif,argtypes,rettype) \
  if (ffi_prep_cif(&(cif),FFI_DEFAULT_ABI,sizeof(argtypes)/sizeof(argtypes[0]),&rettype,argtypes) != FFI_OK) abort()
#define FFI_PREP_CIF_VAR(cif,nfixedargs,argtypes,rettype) \
  if (ffi_prep_cif_var(&(cif),FFI_DEFAULT_ABI,nfixedargs,sizeof(argtypes)/sizeof(argtypes[0]),&rettype,argtypes) != FFI_OK) abort()
#define FFI_PREP_CIF_NOARGS(cif,rettype) \
  if (ffi_prep_cif(&(cif),FFI_DEFAULT_ABI,0,&rettype,NULL) != FFI_OK) abort()
#define FFI_CALL(cif,fn,args,retaddr) \
  ffi_call(&(cif),(void(*)(void))(fn),retaddr,args)

long clear_traces_i (long a, long b, long c, long d, long e, long f, long g, long h,
                     long i, long j, long k, long l, long m, long n, long o, long p)
{ return 0; }
float clear_traces_f (float a, float b, float c, float d, float e, float f, float g,
                      float h, float i, float j, float k, float l, float m, float n,
                      float o, float p)
{ return 0.0; }
double clear_traces_d (double a, double b, double c, double d, double e, double f, double g,
                       double h, double i, double j, double k, double l, double m, double n,
                       double o, double p)
{ return 0.0; }
J clear_traces_J (void)
{ J j; j.l1 = j.l2 = 0; return j; }
void clear_traces (void)
{ clear_traces_i(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  clear_traces_f(0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0);
  clear_traces_d(0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0);
  clear_traces_J();
}

void
  void_tests (void)
{
#if (!defined(DGTEST)) || DGTEST == 1
  v_v();
  clear_traces();
  {
    ffi_cif cif;
    FFI_PREP_CIF_NOARGS(cif,ffi_type_void);
    {
      FFI_CALL(cif,v_v,NULL,NULL);
    }
  }
#endif
  return;
}
void
  int_tests (void)
{
  int ir;
#if (!defined(DGTEST)) || DGTEST == 2
  ir = i_v();
  FPRINTF(out,"->%d\n",ir);
  fflush(out);
  ir = 0; clear_traces();
  {
    ffi_cif cif;
    FFI_PREP_CIF_NOARGS(cif,ffi_type_sint);
    {
      FFI_CALL(cif,i_v,NULL,&ir);
    }
  }
  FPRINTF(out,"->%d\n",ir);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 3
  ir = i_i(i1);
  FPRINTF(out,"->%d\n",ir);
  fflush(out);
  ir = 0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_sint };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_sint);
    {
      /*const*/ void* args[] = { &i1 };
      FFI_CALL(cif,i_i,args,&ir);
    }
  }
  FPRINTF(out,"->%d\n",ir);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 4
  ir = i_i2(i1,i2);
  FPRINTF(out,"->%d\n",ir);
  fflush(out);
  ir = 0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_sint, &ffi_type_sint };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_sint);
    {
      /*const*/ void* args[] = { &i1, &i2 };
      FFI_CALL(cif,i_i2,args,&ir);
    }
  }
  FPRINTF(out,"->%d\n",ir);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 5
  ir = i_i4(i1,i2,i3,i4);
  FPRINTF(out,"->%d\n",ir);
  fflush(out);
  ir = 0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_sint };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_sint);
    {
      /*const*/ void* args[] = { &i1, &i2, &i3, &i4 };
      FFI_CALL(cif,i_i4,args,&ir);
    }
  }
  FPRINTF(out,"->%d\n",ir);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 6
  ir = i_i8(i1,i2,i3,i4,i5,i6,i7,i8);
  FPRINTF(out,"->%d\n",ir);
  fflush(out);
  ir = 0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_sint };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_sint);
    {
      /*const*/ void* args[] = { &i1, &i2, &i3, &i4, &i5, &i6, &i7, &i8 };
      FFI_CALL(cif,i_i8,args,&ir);
    }
  }
  FPRINTF(out,"->%d\n",ir);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 7
  ir = i_i16(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,i16);
  FPRINTF(out,"->%d\n",ir);
  fflush(out);
  ir = 0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_sint };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_sint);
    {
      /*const*/ void* args[] = { &i1, &i2, &i3, &i4, &i5, &i6, &i7, &i8, &i9, &i10, &i11, &i12, &i13, &i14, &i15, &i16 };
      FFI_CALL(cif,i_i16,args,&ir);
    }
  }
  FPRINTF(out,"->%d\n",ir);
  fflush(out);
#endif

  return;
}
void
  float_tests (void)
{
  float fr;

#if (!defined(DGTEST)) || DGTEST == 8
  fr = f_f(f1);
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
  fr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_float };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_float);
    {
      /*const*/ void* args[] = { &f1 };
      FFI_CALL(cif,f_f,args,&fr);
    }
  }
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 9
  fr = f_f2(f1,f2);
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
  fr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_float, &ffi_type_float };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_float);
    {
      /*const*/ void* args[] = { &f1, &f2 };
      FFI_CALL(cif,f_f2,args,&fr);
    }
  }
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 10
  fr = f_f4(f1,f2,f3,f4);
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
  fr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_float);
    {
      /*const*/ void* args[] = { &f1, &f2, &f3, &f4 };
      FFI_CALL(cif,f_f4,args,&fr);
    }
  }
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 11
  fr = f_f8(f1,f2,f3,f4,f5,f6,f7,f8);
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
  fr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_float);
    {
      /*const*/ void* args[] = { &f1, &f2, &f3, &f4, &f5, &f6, &f7, &f8 };
      FFI_CALL(cif,f_f8,args,&fr);
    }
  }
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 12
  fr = f_f16(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16);
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
  fr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_float);
    {
      /*const*/ void* args[] = { &f1, &f2, &f3, &f4, &f5, &f6, &f7, &f8, &f9, &f10, &f11, &f12, &f13, &f14, &f15, &f16 };
      FFI_CALL(cif,f_f16,args,&fr);
    }
  }
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 13
  fr = f_f24(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,f24);
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
  fr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_float);
    {
      /*const*/ void* args[] = { &f1, &f2, &f3, &f4, &f5, &f6, &f7, &f8, &f9, &f10, &f11, &f12, &f13, &f14, &f15, &f16, &f17, &f18, &f19, &f20, &f21, &f22, &f23, &f24 };
      FFI_CALL(cif,f_f24,args,&fr);
    }
  }
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
#endif
}
void
  double_tests (void)
{
  double dr;

#if (!defined(DGTEST)) || DGTEST == 14

  dr = d_d(d1);
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
  dr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_double };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_double);
    {
      /*const*/ void* args[] = { &d1 };
      FFI_CALL(cif,d_d,args,&dr);
    }
  }
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 15
  dr = d_d2(d1,d2);
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
  dr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_double, &ffi_type_double };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_double);
    {
      /*const*/ void* args[] = { &d1, &d2 };
      FFI_CALL(cif,d_d2,args,&dr);
    }
  }
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 16
  dr = d_d4(d1,d2,d3,d4);
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
  dr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_double);
    {
      /*const*/ void* args[] = { &d1, &d2, &d3, &d4 };
      FFI_CALL(cif,d_d4,args,&dr);
    }
  }
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 17
  dr = d_d8(d1,d2,d3,d4,d5,d6,d7,d8);
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
  dr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_double);
    {
      /*const*/ void* args[] = { &d1, &d2, &d3, &d4, &d5, &d6, &d7, &d8 };
      FFI_CALL(cif,d_d8,args,&dr);
    }
  }
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 18
  dr = d_d16(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16);
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
  dr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_double);
    {
      /*const*/ void* args[] = { &d1, &d2, &d3, &d4, &d5, &d6, &d7, &d8, &d9, &d10, &d11, &d12, &d13, &d14, &d15, &d16 };
      FFI_CALL(cif,d_d16,args,&dr);
    }
  }
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
#endif
  return;
}
#if HAVE_LONG_DOUBLE
void
  long_double_tests (void)
{
  long double ldr;

#if (!defined(DGTEST)) || DGTEST == 14
  ldr = ld_ld(ld1);
  FPRINTF(out,"->%g\n",(double)ldr); /* had issues with %Lg */
  fflush(out);
  ldr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_longdouble };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_longdouble);
    {
      /*const*/ void* args[] = { &ld1 };
      FFI_CALL(cif,ld_ld,args,&ldr);
    }
  }
  FPRINTF(out,"->%g\n",(double)ldr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 15
  ldr = ld_ld2(ld1,ld2);
  FPRINTF(out,"->%g\n",(double)ldr);
  fflush(out);
  ldr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_longdouble, &ffi_type_longdouble };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_longdouble);
    {
      /*const*/ void* args[] = { &ld1, &ld2 };
      FFI_CALL(cif,ld_ld2,args,&ldr);
    }
  }
  FPRINTF(out,"->%g\n",(double)ldr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 16
  ldr = ld_ld4(ld1,ld2,ld3,ld4);
  FPRINTF(out,"->%g\n",(double)ldr);
  fflush(out);
  ldr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_longdouble, &ffi_type_longdouble, &ffi_type_longdouble, &ffi_type_longdouble };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_longdouble);
    {
      /*const*/ void* args[] = { &ld1, &ld2, &ld3, &ld4 };
      FFI_CALL(cif,ld_ld4,args,&ldr);
    }
  }
  FPRINTF(out,"->%g\n",(double)ldr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 17
  ldr = ld_ld8(ld1,ld2,ld3,ld4,ld5,ld6,ld7,ld8);
  FPRINTF(out,"->%g\n",(double)ldr);
  fflush(out);
  ldr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_longdouble, &ffi_type_longdouble, &ffi_type_longdouble, &ffi_type_longdouble, &ffi_type_longdouble, &ffi_type_longdouble, &ffi_type_longdouble, &ffi_type_longdouble };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_longdouble);
    {
      /*const*/ void* args[] = { &ld1, &ld2, &ld3, &ld4, &ld5, &ld6, &ld7, &ld8 };
      FFI_CALL(cif,ld_ld8,args,&ldr);
    }
  }
  FPRINTF(out,"->%g\n",(double)ldr);
  fflush(out);
  /* */
  ldr = ld_fldx5(f1,ld1,f2,ld2,f3,ld3,f4,ld4,f5,ld5);
  FPRINTF(out,"->%g\n",(double)ldr);
  fflush(out);
  ldr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = {
	    &ffi_type_float, &ffi_type_longdouble,
	    &ffi_type_float, &ffi_type_longdouble,
	    &ffi_type_float, &ffi_type_longdouble,
	    &ffi_type_float, &ffi_type_longdouble,
	    &ffi_type_float, &ffi_type_longdouble };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_longdouble);
    {
      /*const*/ void* args[] = { &f1, &ld1, &f2, &ld2, &f3, &ld3, &f4, &ld4, &f5, &ld5 };
      FFI_CALL(cif,ld_fldx5,args,&ldr);
    }
  }
  FPRINTF(out,"->%g\n",(double)ldr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 18
  ldr = ld_ld16(ld1,ld2,ld3,ld4,ld5,ld6,ld7,ld8,ld9,ld10,ld11,ld12,ld13,ld14,ld15,ld16);
  FPRINTF(out,"->%g\n",(double)ldr);
  fflush(out);
  ldr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_longdouble, &ffi_type_longdouble, &ffi_type_longdouble, &ffi_type_longdouble, &ffi_type_longdouble, &ffi_type_longdouble, &ffi_type_longdouble, &ffi_type_longdouble, &ffi_type_longdouble, &ffi_type_longdouble, &ffi_type_longdouble, &ffi_type_longdouble, &ffi_type_longdouble, &ffi_type_longdouble, &ffi_type_longdouble, &ffi_type_longdouble };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_longdouble);
    {
      /*const*/ void* args[] = { &ld1, &ld2, &ld3, &ld4, &ld5, &ld6, &ld7, &ld8, &ld9, &ld10, &ld11, &ld12, &ld13, &ld14, &ld15, &ld16 };
      FFI_CALL(cif,ld_ld16,args,&ldr);
    }
  }
  FPRINTF(out,"->%g\n",(double)ldr);
  fflush(out);
#endif
  return;
}
#endif /*HAVE_LONG_DOUBLE*/
#if HAVE_COMPLEX
#if 1 /* float complex */
static void prt_fq(float complex fq){
  FPRINTF(out,"->%g+%g*I\n",(double)crealf(fq),(double)cimagf(fq));
  fflush(out);
}
void
  float_complex_tests (void)
{
  float complex fqr;

#if (!defined(DGTEST)) || DGTEST == 14

  fqr = fq_fq(fq1);
  prt_fq(fqr);
  fqr = 0.0+0.0*I; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_complex_float };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_complex_float);
    {
      /*const*/ void* args[] = { &fq1 };
      FFI_CALL(cif,fq_fq,args,&fqr);
    }
  }
  prt_fq(fqr);
#endif

#if (!defined(DGTEST)) || DGTEST == 15
  fqr = fq_fq2(fq1,fq2);
  prt_fq(fqr);
  fqr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_complex_float, &ffi_type_complex_float };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_complex_float);
    {
      /*const*/ void* args[] = { &fq1, &fq2 };
      FFI_CALL(cif,fq_fq2,args,&fqr);
    }
  }
  prt_fq(fqr);

  fqr = fq_ffq(f1,fq2); /* simple mixed args: what reg does fq2 start in? */
  prt_fq(fqr);
  fqr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_float, &ffi_type_complex_float };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_complex_float);
    {
      /*const*/ void* args[] = { &f1, &fq2 };
      FFI_CALL(cif,fq_ffq, args,&fqr);
    }
  }
  prt_fq(fqr);
#endif

#if (!defined(DGTEST)) || DGTEST == 16
  fqr = fq_fq4(fq1,fq2,fq3,fq4);
  prt_fq(fqr);
  fqr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_complex_float, &ffi_type_complex_float, &ffi_type_complex_float, &ffi_type_complex_float };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_complex_float);
    {
      /*const*/ void* args[] = { &fq1, &fq2, &fq3, &fq4 };
      FFI_CALL(cif,fq_fq4,args,&fqr);
    }
  }
  prt_fq(fqr);
#endif

#if 1 /* XXX ncc has a bug passing complex arguments that are NOT within %s0..%s7 */
#if (!defined(DGTEST)) || DGTEST == 17
  fqr = fq_fq8(fq1,fq2,fq3,fq4,fq5,fq6,fq7,fq8);
  prt_fq(fqr);
  fqr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = {
	    &ffi_type_complex_float, &ffi_type_complex_float, &ffi_type_complex_float,
	    &ffi_type_complex_float, &ffi_type_complex_float, &ffi_type_complex_float,
	    &ffi_type_complex_float, &ffi_type_complex_float };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_complex_float);
    {
      /*const*/ void* args[] = { &fq1, &fq2, &fq3, &fq4, &fq5, &fq6, &fq7, &fq8 };
      FFI_CALL(cif,fq_fq8,args,&fqr);
    }
  }
  prt_fq(fqr);
  /* */
  fqr = fq_ffqx5(f1,fq1,f2,fq2,f3,fq3,f4,fq4,f5,fq5);
  prt_fq(fqr);
  fqr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = {
	    &ffi_type_float, &ffi_type_complex_float,
	    &ffi_type_float, &ffi_type_complex_float,
	    &ffi_type_float, &ffi_type_complex_float,
	    &ffi_type_float, &ffi_type_complex_float,
	    &ffi_type_float, &ffi_type_complex_float };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_complex_float);
    {
      /*const*/ void* args[] = { &f1, &fq1, &f2, &fq2, &f3, &fq3, &f4, &fq4, &f5, &fq5 };
      FFI_CALL(cif,fq_ffqx5,args,&fqr);
    }
  }
  prt_fq(fqr);
#endif

#if (!defined(DGTEST)) || DGTEST == 18
  fqr = fq_fq16(fq1,fq2,fq3,fq4,fq5,fq6,fq7,fq8,fq9,fq10,fq11,fq12,fq13,fq14,fq15,fq16);
  prt_fq(fqr);
  fqr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_complex_float, &ffi_type_complex_float, &ffi_type_complex_float, &ffi_type_complex_float, &ffi_type_complex_float, &ffi_type_complex_float, &ffi_type_complex_float, &ffi_type_complex_float, &ffi_type_complex_float, &ffi_type_complex_float, &ffi_type_complex_float, &ffi_type_complex_float, &ffi_type_complex_float, &ffi_type_complex_float, &ffi_type_complex_float, &ffi_type_complex_float };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_complex_float);
    {
      /*const*/ void* args[] = { &fq1, &fq2, &fq3, &fq4, &fq5, &fq6, &fq7, &fq8, &fq9, &fq10, &fq11, &fq12, &fq13, &fq14, &fq15, &fq16 };
      FFI_CALL(cif,fq_fq16,args,&fqr);
    }
  }
  prt_fq(fqr);
#endif
#endif /* ncc-1.08 bug = doomed to failure */

  return;
}
#endif /* float complex */
#if 1 /* double complex */
static void prt_dq(double complex dq){
  FPRINTF(out,"->%g+%g*I\n",(double)creal(dq),(double)cimag(dq));
  fflush(out);
}
void
  double_complex_tests (void)
{
  double complex dqr;

#if (!defined(DGTEST)) || DGTEST == 14

  dqr = dq_dq(dq1);
  prt_dq(dqr);
  dqr = 0.0+0.0*I; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_complex_double };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_complex_double);
    {
      /*const*/ void* args[] = { &dq1 };
      FFI_CALL(cif,dq_dq,args,&dqr);
    }
  }
  prt_dq(dqr);
#endif

#if (!defined(DGTEST)) || DGTEST == 15
  dqr = dq_dq2(dq1,dq2);
  prt_dq(dqr);
  dqr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_complex_double, &ffi_type_complex_double };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_complex_double);
    {
      /*const*/ void* args[] = { &dq1, &dq2 };
      FFI_CALL(cif,dq_dq2,args,&dqr);
    }
  }
  prt_dq(dqr);

  dqr = dq_fdq(f1,dq2); /* simple mixed args: what reg does dq2 start in? */
  prt_dq(dqr);
  dqr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_float, &ffi_type_complex_double };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_complex_double);
    {
      /*const*/ void* args[] = { &f1, &dq2 };
      FFI_CALL(cif,dq_fdq, args,&dqr);
    }
  }
  prt_dq(dqr);
#endif

#if (!defined(DGTEST)) || DGTEST == 16
  dqr = dq_dq4(dq1,dq2,dq3,dq4);
  prt_dq(dqr);
  dqr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_complex_double, &ffi_type_complex_double, &ffi_type_complex_double, &ffi_type_complex_double };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_complex_double);
    {
      /*const*/ void* args[] = { &dq1, &dq2, &dq3, &dq4 };
      FFI_CALL(cif,dq_dq4,args,&dqr);
    }
  }
  prt_dq(dqr);
#endif

#if 1 /* XXX ncc has a bug passing complex arguments that are NOT within %s0..%s7 */
#if (!defined(DGTEST)) || DGTEST == 17
  dqr = dq_dq8(dq1,dq2,dq3,dq4,dq5,dq6,dq7,dq8);
  prt_dq(dqr);
  dqr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = {
	    &ffi_type_complex_double, &ffi_type_complex_double, &ffi_type_complex_double,
	    &ffi_type_complex_double, &ffi_type_complex_double, &ffi_type_complex_double,
	    &ffi_type_complex_double, &ffi_type_complex_double };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_complex_double);
    {
      /*const*/ void* args[] = { &dq1, &dq2, &dq3, &dq4, &dq5, &dq6, &dq7, &dq8 };
      FFI_CALL(cif,dq_dq8,args,&dqr);
    }
  }
  prt_dq(dqr);
  /* */
  dqr = dq_fdqx5(f1,dq1,f2,dq2,f3,dq3,f4,dq4,f5,dq5);
  prt_dq(dqr);
  dqr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = {
	    &ffi_type_float, &ffi_type_complex_double,
	    &ffi_type_float, &ffi_type_complex_double,
	    &ffi_type_float, &ffi_type_complex_double,
	    &ffi_type_float, &ffi_type_complex_double,
	    &ffi_type_float, &ffi_type_complex_double };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_complex_double);
    {
      /*const*/ void* args[] = { &f1, &dq1, &f2, &dq2, &f3, &dq3, &f4, &dq4, &f5, &dq5 };
      FFI_CALL(cif,dq_fdqx5,args,&dqr);
    }
  }
  prt_dq(dqr);
#endif

#if (!defined(DGTEST)) || DGTEST == 18
  dqr = dq_dq16(dq1,dq2,dq3,dq4,dq5,dq6,dq7,dq8,dq9,dq10,dq11,dq12,dq13,dq14,dq15,dq16);
  prt_dq(dqr);
  dqr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_complex_double, &ffi_type_complex_double, &ffi_type_complex_double, &ffi_type_complex_double, &ffi_type_complex_double, &ffi_type_complex_double, &ffi_type_complex_double, &ffi_type_complex_double, &ffi_type_complex_double, &ffi_type_complex_double, &ffi_type_complex_double, &ffi_type_complex_double, &ffi_type_complex_double, &ffi_type_complex_double, &ffi_type_complex_double, &ffi_type_complex_double };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_complex_double);
    {
      /*const*/ void* args[] = { &dq1, &dq2, &dq3, &dq4, &dq5, &dq6, &dq7, &dq8, &dq9, &dq10, &dq11, &dq12, &dq13, &dq14, &dq15, &dq16 };
      FFI_CALL(cif,dq_dq16,args,&dqr);
    }
  }
  prt_dq(dqr);
#endif
#endif /* ncc-1.08 bug = doomed to failure */

  return;
}
#endif /* double complex */
#if 1 /* long double complex */
static void prt_ldq(long double complex ldq){
  FPRINTF(out,"->%g+%g*I\n",(double)creal(ldq),(double)cimag(ldq));
  fflush(out);
}
void
  long_double_complex_tests (void)
{
  long double complex ldqr;

#if (!defined(DGTEST)) || DGTEST == 14

  ldqr = ldq_ldq(ldq1);
  prt_ldq(ldqr);
  ldqr = 0.0+0.0*I; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_complex_longdouble };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_complex_longdouble);
    {
      /*const*/ void* args[] = { &ldq1 };
      FFI_CALL(cif,ldq_ldq,args,&ldqr);
    }
  }
  prt_ldq(ldqr);
#endif

#if (!defined(DGTEST)) || DGTEST == 15
  ldqr = ldq_ldq2(ldq1,ldq2);
  prt_ldq(ldqr);
  ldqr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_complex_longdouble, &ffi_type_complex_longdouble };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_complex_longdouble);
    {
      /*const*/ void* args[] = { &ldq1, &ldq2 };
      FFI_CALL(cif,ldq_ldq2,args,&ldqr);
    }
  }
  prt_ldq(ldqr);

  ldqr = ldq_fldq(f1,ldq2); /* simple mixed args: what reg does ldq2 start in? */
  prt_ldq(ldqr);
  ldqr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_float, &ffi_type_complex_longdouble };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_complex_longdouble);
    {
      /*const*/ void* args[] = { &f1, &ldq2 };
      FFI_CALL(cif,ldq_fldq, args,&ldqr);
    }
  }
  prt_ldq(ldqr);
#endif

#if 1 /* XXX ncc has a bug passing long double complex arguments that are NOT within %s0..%s7 */
#if (!defined(DGTEST)) || DGTEST == 16
  ldqr = ldq_ldq4(ldq1,ldq2,ldq3,ldq4);
  prt_ldq(ldqr);
  ldqr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_complex_longdouble, &ffi_type_complex_longdouble, &ffi_type_complex_longdouble, &ffi_type_complex_longdouble };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_complex_longdouble);
    {
      /*const*/ void* args[] = { &ldq1, &ldq2, &ldq3, &ldq4 };
      FFI_CALL(cif,ldq_ldq4,args,&ldqr);
    }
  }
  prt_ldq(ldqr);
#endif

#if (!defined(DGTEST)) || DGTEST == 17
  ldqr = ldq_ldq8(ldq1,ldq2,ldq3,ldq4,ldq5,ldq6,ldq7,ldq8);
  prt_ldq(ldqr);
  ldqr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = {
	    &ffi_type_complex_longdouble, &ffi_type_complex_longdouble, &ffi_type_complex_longdouble,
	    &ffi_type_complex_longdouble, &ffi_type_complex_longdouble, &ffi_type_complex_longdouble,
	    &ffi_type_complex_longdouble, &ffi_type_complex_longdouble };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_complex_longdouble);
    {
      /*const*/ void* args[] = { &ldq1, &ldq2, &ldq3, &ldq4, &ldq5, &ldq6, &ldq7, &ldq8 };
      FFI_CALL(cif,ldq_ldq8,args,&ldqr);
    }
  }
  prt_ldq(ldqr);
  /* */
  ldqr = ldq_fldqx5(f1,ldq1,f2,ldq2,f3,ldq3,f4,ldq4,f5,ldq5);
  prt_ldq(ldqr);
  ldqr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = {
	    &ffi_type_float, &ffi_type_complex_longdouble,
	    &ffi_type_float, &ffi_type_complex_longdouble,
	    &ffi_type_float, &ffi_type_complex_longdouble,
	    &ffi_type_float, &ffi_type_complex_longdouble,
	    &ffi_type_float, &ffi_type_complex_longdouble };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_complex_longdouble);
    {
      /*const*/ void* args[] = { &f1, &ldq1, &f2, &ldq2, &f3, &ldq3, &f4, &ldq4, &f5, &ldq5 };
      FFI_CALL(cif,ldq_fldqx5,args,&ldqr);
    }
  }
  prt_ldq(ldqr);
#endif

#if (!defined(DGTEST)) || DGTEST == 18
  ldqr = ldq_ldq16(ldq1,ldq2,ldq3,ldq4,ldq5,ldq6,ldq7,ldq8,ldq9,ldq10,ldq11,ldq12,ldq13,ldq14,ldq15,ldq16);
  prt_ldq(ldqr);
  ldqr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_complex_longdouble, &ffi_type_complex_longdouble, &ffi_type_complex_longdouble, &ffi_type_complex_longdouble, &ffi_type_complex_longdouble, &ffi_type_complex_longdouble, &ffi_type_complex_longdouble, &ffi_type_complex_longdouble, &ffi_type_complex_longdouble, &ffi_type_complex_longdouble, &ffi_type_complex_longdouble, &ffi_type_complex_longdouble, &ffi_type_complex_longdouble, &ffi_type_complex_longdouble, &ffi_type_complex_longdouble, &ffi_type_complex_longdouble };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_complex_longdouble);
    {
      /*const*/ void* args[] = { &ldq1, &ldq2, &ldq3, &ldq4, &ldq5, &ldq6, &ldq7, &ldq8, &ldq9, &ldq10, &ldq11, &ldq12, &ldq13, &ldq14, &ldq15, &ldq16 };
      FFI_CALL(cif,ldq_ldq16,args,&ldqr);
    }
  }
  prt_ldq(ldqr);
#endif
#endif /* ncc-1.08 bug = doomed to failure */

  return;
}
#endif /* long double complex */
#endif /*HAVE_COMPLEX*/
void
  pointer_tests (void)
{
  void* vpr;
  double dr;

#if (!defined(DGTEST)) || DGTEST == 19
#if 0
  /* non-standard libffi convention for FFI_TYPE_POINTER is to pass-by-value */
  /* Works if you set
   *       libffi ve/ffitarget.h:
   *              #define VE_POINTER_BY_VALUE 1 */

  /* [Personally, putting pointers directly into args[] seems more natural,
   *  and also simplifies the implementation] */

  /* THIS DOES NOT FOLLOW THE LIBFFI CONVENTIONS ELSEWHERE IN TESTSUITE */
  dr= d_ifpsp(i1,&f5,&I4);
  FPRINTF(out,"->%f\n",dr);
  fflush(out);
  dr = 0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_sint, &ffi_type_pointer, &ffi_type_pointer };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_double);
    {
      /*const*/ void* args[] = { &i1, (void*)&f5, (void*)&I4 };
      FFI_CALL(cif,d_ifpsp,args,&dr);
    }
  }
  FPRINTF(out,"->%f\n",dr);
  fflush(out);
#else
  /* standard libffi convention for FFI_TYPE_POINTER is to pass-by-address */
  dr= d_ifpsp(i1,&f5,&I4);
  FPRINTF(out,"->%f\n",dr);
  fflush(out);
  dr = 0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_sint, &ffi_type_pointer, &ffi_type_pointer };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_double);
    {
      /* doesn't this seem like a needless hassle to pass addr-of-ptr? */
      void* pf5 = &f5;
      void* pI4 = &I4;
      /*const*/ void* args[] = { &i1, (void*)&pf5, (void*)&pI4 };
      FFI_CALL(cif,d_ifpsp,args,&dr);
    }
  }
  FPRINTF(out,"->%f\n",dr);
  fflush(out);
#endif
  /* original test has additional level of indirection (unwieldly for impl) */
  vpr = vp_vpdpcpsp(&uc1,&d2,str3,&I4);
  FPRINTF(out,"->0x%p\n",vpr);
  fflush(out);
  vpr = 0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_pointer, &ffi_type_pointer, &ffi_type_pointer, &ffi_type_pointer };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_pointer);
    {
      void* puc1 = &uc1;
      void* pd2 = &d2;
      void* pstr3 = str3;
      void* pI4 = &I4;
      /*const*/ void* args[] = { &puc1, &pd2, &pstr3, &pI4 };
      FFI_CALL(cif,vp_vpdpcpsp,args,&vpr);
    }
  }
  FPRINTF(out,"->0x%p\n",vpr);
  fflush(out);
#endif
  return;
}
void
  mixed_number_tests (void)
{
  uchar ucr;
  ushort usr;
  float fr;
  double dr;
  long long llr;

  /* Unsigned types.
   */
#if (!defined(DGTEST)) || DGTEST == 20
  ucr = uc_ucsil(uc1, us2, ui3, ul4);
  FPRINTF(out,"->%u\n",ucr);
  fflush(out);
  ucr = 0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_uchar, &ffi_type_ushort, &ffi_type_uint, &ffi_type_ulong };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_uchar);
    {
      ffi_arg r;
      /*const*/ void* args[] = { &uc1, &us2, &ui3, &ul4 };
      FFI_CALL(cif,uc_ucsil,args,&r);
      ucr = (uchar) r;
    }
  }
  FPRINTF(out,"->%u\n",ucr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 21
  /* Mixed int & float types.
   */
  dr = d_iidd(i1,i2,d3,d4);
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
  dr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_sint, &ffi_type_sint, &ffi_type_double, &ffi_type_double };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_double);
    {
      /*const*/ void* args[] = { &i1, &i2, &d3, &d4 };
      FFI_CALL(cif,d_iidd,args,&dr);
    }
  }
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 22
  dr = d_iiidi(i1,i2,i3,d4,i5);
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
  dr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_double, &ffi_type_sint };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_double);
    {
      /*const*/ void* args[] = { &i1, &i2, &i3, &d4, &i5 };
      FFI_CALL(cif,d_iiidi,args,&dr);
    }
  }
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 23
  dr = d_idid(i1,d2,i3,d4);
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
  dr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_sint, &ffi_type_double, &ffi_type_sint, &ffi_type_double };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_double);
    {
      /*const*/ void* args[] = { &i1, &d2, &i3, &d4 };
      FFI_CALL(cif,d_idid,args,&dr);
    }
  }
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 24
  dr = d_fdi(f1,d2,i3);
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
  dr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_float, &ffi_type_double, &ffi_type_sint };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_double);
    {
      /*const*/ void* args[] = { &f1, &d2, &i3 };
      FFI_CALL(cif,d_fdi,args,&dr);
    }
  }
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 25
  usr = us_cdcd(c1,d2,c3,d4);
  FPRINTF(out,"->%u\n",usr);
  fflush(out);
  usr = 0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_char, &ffi_type_double, &ffi_type_char, &ffi_type_double };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_ushort);
    {
      ffi_arg rint;
      /*const*/ void* args[] = { &c1, &d2, &c3, &d4 };
      FFI_CALL(cif,us_cdcd,args,&rint);
      usr = (ushort) rint;
    }
  }
  FPRINTF(out,"->%u\n",usr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 26
  /* Long long types.
   */
  llr = ll_iiilli(i1,i2,i3,ll1,i13);
  FPRINTF(out,"->0x%lx%08lx\n",(long)(llr>>32),(long)(llr&0xffffffff));
  fflush(out);
  llr = 0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_slonglong, &ffi_type_sint };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_slonglong);
    {
      /*const*/ void* args[] = { &i1, &i2, &i3, &ll1, &i13 };
      FFI_CALL(cif,ll_iiilli,args,&llr);
    }
  }
  FPRINTF(out,"->0x%lx%08lx\n",(long)(llr>>32),(long)(llr&0xffffffff));
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 27
  llr = ll_flli(f13,ll1,i13);
  FPRINTF(out,"->0x%lx%08lx\n",(long)(llr>>32),(long)(llr&0xffffffff));
  fflush(out);
  llr = 0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_float, &ffi_type_slonglong, &ffi_type_sint };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_slonglong);
    {
      /*const*/ void* args[] = { &f13, &ll1, &i13 };
      FFI_CALL(cif,ll_flli,args,&llr);
    }
  }
  FPRINTF(out,"->0x%lx%08lx\n",(long)(llr>>32),(long)(llr&0xffffffff));
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 28
  fr = f_fi(f1,i9);
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
  fr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_float, &ffi_type_sint };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_float);
    {
      /*const*/ void* args[] = { &f1, &i9 };
      FFI_CALL(cif,f_fi,args,&fr);
    }
  }
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 29
  fr = f_f2i(f1,f2,i9);
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
  fr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_float, &ffi_type_float, &ffi_type_sint };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_float);
    {
      /*const*/ void* args[] = { &f1, &f2, &i9 };
      FFI_CALL(cif,f_f2i,args,&fr);
    }
  }
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 30
  fr = f_f3i(f1,f2,f3,i9);
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
  fr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_sint };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_float);
    {
      /*const*/ void* args[] = { &f1, &f2, &f3, &i9 };
      FFI_CALL(cif,f_f3i,args,&fr);
    }
  }
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 31
  fr = f_f4i(f1,f2,f3,f4,i9);
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
  fr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_sint };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_float);
    {
      /*const*/ void* args[] = { &f1, &f2, &f3, &f4, &i9 };
      FFI_CALL(cif,f_f4i,args,&fr);
    }
  }
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 32
  fr = f_f7i(f1,f2,f3,f4,f5,f6,f7,i9);
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
  fr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_sint };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_float);
    {
      /*const*/ void* args[] = { &f1, &f2, &f3, &f4, &f5, &f6, &f7, &i9 };
      FFI_CALL(cif,f_f7i,args,&fr);
    }
  }
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 33
  fr = f_f8i(f1,f2,f3,f4,f5,f6,f7,f8,i9);
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
  fr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_sint };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_float);
    {
      /*const*/ void* args[] = { &f1, &f2, &f3, &f4, &f5, &f6, &f7, &f8, &i9 };
      FFI_CALL(cif,f_f8i,args,&fr);
    }
  }
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 34
  fr = f_f12i(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,i9);
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
  fr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_sint };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_float);
    {
      /*const*/ void* args[] = { &f1, &f2, &f3, &f4, &f5, &f6, &f7, &f8, &f9, &f10, &f11, &f12, &i9 };
      FFI_CALL(cif,f_f12i,args,&fr);
    }
  }
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 35
  fr = f_f13i(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,i9);
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
  fr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_sint };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_float);
    {
      /*const*/ void* args[] = { &f1, &f2, &f3, &f4, &f5, &f6, &f7, &f8, &f9, &f10, &f11, &f12, &f13, &i9 };
      FFI_CALL(cif,f_f13i,args,&fr);
    }
  }
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 36
  dr = d_di(d1,i9);
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
  dr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_double, &ffi_type_sint };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_double);
    {
      /*const*/ void* args[] = { &d1, &i9 };
      FFI_CALL(cif,d_di,args,&dr);
    }
  }
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 37
  dr = d_d2i(d1,d2,i9);
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
  dr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_double, &ffi_type_double, &ffi_type_sint };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_double);
    {
      /*const*/ void* args[] = { &d1, &d2, &i9 };
      FFI_CALL(cif,d_d2i,args,&dr);
    }
  }
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 38
  dr = d_d3i(d1,d2,d3,i9);
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
  dr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_sint };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_double);
    {
      /*const*/ void* args[] = { &d1, &d2, &d3, &i9 };
      FFI_CALL(cif,d_d3i,args,&dr);
    }
  }
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 39
  dr = d_d4i(d1,d2,d3,d4,i9);
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
  dr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_sint };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_double);
    {
      /*const*/ void* args[] = { &d1, &d2, &d3, &d4, &i9 };
      FFI_CALL(cif,d_d4i,args,&dr);
    }
  }
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 40
  dr = d_d7i(d1,d2,d3,d4,d5,d6,d7,i9);
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
  dr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_sint };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_double);
    {
      /*const*/ void* args[] = { &d1, &d2, &d3, &d4, &d5, &d6, &d7, &i9 };
      FFI_CALL(cif,d_d7i,args,&dr);
    }
  }
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 41
  dr = d_d8i(d1,d2,d3,d4,d5,d6,d7,d8,i9);
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
  dr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_sint };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_double);
    {
      /*const*/ void* args[] = { &d1, &d2, &d3, &d4, &d5, &d6, &d7, &d8, &i9 };
      FFI_CALL(cif,d_d8i,args,&dr);
    }
  }
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 42
  dr = d_d12i(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,i9);
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
  dr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_sint };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_double);
    {
      /*const*/ void* args[] = { &d1, &d2, &d3, &d4, &d5, &d6, &d7, &d8, &d9, &d10, &d11, &d12, &i9 };
      FFI_CALL(cif,d_d12i,args,&dr);
    }
  }
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
#endif

#if (!defined(DGTEST)) || DGTEST == 43
  dr = d_d13i(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,i9);
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
  dr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_sint };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_double);
    {
      /*const*/ void* args[] = { &d1, &d2, &d3, &d4, &d5, &d6, &d7, &d8, &d9, &d10, &d11, &d12, &d13, &i9 };
      FFI_CALL(cif,d_d13i,args,&dr);
    }
  }
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
#endif
  return;
}
void
  small_structure_return_tests (void)
{
#if (!defined(DGTEST)) || DGTEST == 44
  {
    Size1 r = S1_v();
    FPRINTF(out,"->{%c}\n",r.x1);
    fflush(out);
    memset(&r,0,sizeof(r)); clear_traces();
    {
      ffi_type* ffi_type_Size1_elements[] = { &ffi_type_char, NULL };
      ffi_type ffi_type_Size1;
      ffi_type_Size1.type = FFI_TYPE_STRUCT;
      ffi_type_Size1.size = sizeof(Size1);
      ffi_type_Size1.alignment = alignof_slot(Size1);
      ffi_type_Size1.elements = ffi_type_Size1_elements;
      ffi_cif cif;
      FFI_PREP_CIF_NOARGS(cif,ffi_type_Size1);
      {
        FFI_CALL(cif,S1_v,NULL,&r);
      }
    }
    FPRINTF(out,"->{%c}\n",r.x1);
    fflush(out);
  }
#endif
#if (!defined(DGTEST)) || DGTEST == 45
  {
    Size2 r = S2_v();
    FPRINTF(out,"->{%c%c}\n",r.x1,r.x2);
    fflush(out);
    memset(&r,0,sizeof(r)); clear_traces();
    {
      ffi_type* ffi_type_Size2_elements[] = { &ffi_type_char, &ffi_type_char, NULL };
      ffi_type ffi_type_Size2;
      ffi_type_Size2.type = FFI_TYPE_STRUCT;
      ffi_type_Size2.size = sizeof(Size2);
      ffi_type_Size2.alignment = alignof_slot(Size2);
      ffi_type_Size2.elements = ffi_type_Size2_elements;
      ffi_cif cif;
      FFI_PREP_CIF_NOARGS(cif,ffi_type_Size2);
      {
        FFI_CALL(cif,S2_v,NULL,&r);
      }
    }
    FPRINTF(out,"->{%c%c}\n",r.x1,r.x2);
    fflush(out);
  }
#endif
#if (!defined(DGTEST)) || DGTEST == 46
  {
    Size3 r = S3_v();
    FPRINTF(out,"->{%c%c%c}\n",r.x1,r.x2,r.x3);
    fflush(out);
    memset(&r,0,sizeof(r)); clear_traces();
    {
      ffi_type* ffi_type_Size3_elements[] = { &ffi_type_char, &ffi_type_char, &ffi_type_char, NULL };
      ffi_type ffi_type_Size3;
      ffi_type_Size3.type = FFI_TYPE_STRUCT;
      ffi_type_Size3.size = sizeof(Size3);
      ffi_type_Size3.alignment = alignof_slot(Size3);
      ffi_type_Size3.elements = ffi_type_Size3_elements;
      ffi_cif cif;
      FFI_PREP_CIF_NOARGS(cif,ffi_type_Size3);
      {
        FFI_CALL(cif,S3_v,NULL,&r);
      }
    }
    FPRINTF(out,"->{%c%c%c}\n",r.x1,r.x2,r.x3);
    fflush(out);
  }
#endif
#if (!defined(DGTEST)) || DGTEST == 47
  {
    Size4 r = S4_v();
    FPRINTF(out,"->{%c%c%c%c}\n",r.x1,r.x2,r.x3,r.x4);
    fflush(out);
    memset(&r,0,sizeof(r)); clear_traces();
    {
      ffi_type* ffi_type_Size4_elements[] = { &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, NULL };
      ffi_type ffi_type_Size4;
      ffi_type_Size4.type = FFI_TYPE_STRUCT;
      ffi_type_Size4.size = sizeof(Size4);
      ffi_type_Size4.alignment = alignof_slot(Size4);
      ffi_type_Size4.elements = ffi_type_Size4_elements;
      ffi_cif cif;
      FFI_PREP_CIF_NOARGS(cif,ffi_type_Size4);
      {
        FFI_CALL(cif,S4_v,NULL,&r);
      }
    }
    FPRINTF(out,"->{%c%c%c%c}\n",r.x1,r.x2,r.x3,r.x4);
    fflush(out);
  }
#endif
#if (!defined(DGTEST)) || DGTEST == 48
  {
    Size7 r = S7_v();
    FPRINTF(out,"->{%c%c%c%c%c%c%c}\n",r.x1,r.x2,r.x3,r.x4,r.x5,r.x6,r.x7);
    fflush(out);
    memset(&r,0,sizeof(r)); clear_traces();
    {
      ffi_type* ffi_type_Size7_elements[] = { &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, NULL };
      ffi_type ffi_type_Size7;
      ffi_type_Size7.type = FFI_TYPE_STRUCT;
      ffi_type_Size7.size = sizeof(Size7);
      ffi_type_Size7.alignment = alignof_slot(Size7);
      ffi_type_Size7.elements = ffi_type_Size7_elements;
      ffi_cif cif;
      FFI_PREP_CIF_NOARGS(cif,ffi_type_Size7);
      {
        FFI_CALL(cif,S7_v,NULL,&r);
      }
    }
    FPRINTF(out,"->{%c%c%c%c%c%c%c}\n",r.x1,r.x2,r.x3,r.x4,r.x5,r.x6,r.x7);
    fflush(out);
  }
#endif
#if (!defined(DGTEST)) || DGTEST == 49
  {
    Size8 r = S8_v();
    FPRINTF(out,"->{%c%c%c%c%c%c%c%c}\n",r.x1,r.x2,r.x3,r.x4,r.x5,r.x6,r.x7,r.x8);
    fflush(out);
    memset(&r,0,sizeof(r)); clear_traces();
    {
      ffi_type* ffi_type_Size8_elements[] = { &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, NULL };
      ffi_type ffi_type_Size8;
      ffi_type_Size8.type = FFI_TYPE_STRUCT;
      ffi_type_Size8.size = sizeof(Size8);
      ffi_type_Size8.alignment = alignof_slot(Size8);
      ffi_type_Size8.elements = ffi_type_Size8_elements;
      ffi_cif cif;
      FFI_PREP_CIF_NOARGS(cif,ffi_type_Size8);
      {
        FFI_CALL(cif,S8_v,NULL,&r);
      }
    }
    FPRINTF(out,"->{%c%c%c%c%c%c%c%c}\n",r.x1,r.x2,r.x3,r.x4,r.x5,r.x6,r.x7,r.x8);
    fflush(out);
  }
#endif
#if (!defined(DGTEST)) || DGTEST == 50
  {
    Size12 r = S12_v();
    FPRINTF(out,"->{%c%c%c%c%c%c%c%c%c%c%c%c}\n",r.x1,r.x2,r.x3,r.x4,r.x5,r.x6,r.x7,r.x8,r.x9,r.x10,r.x11,r.x12);
    fflush(out);
    memset(&r,0,sizeof(r)); clear_traces();
    {
      ffi_type* ffi_type_Size12_elements[] = { &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, NULL };
      ffi_type ffi_type_Size12;
      ffi_type_Size12.type = FFI_TYPE_STRUCT;
      ffi_type_Size12.size = sizeof(Size12);
      ffi_type_Size12.alignment = alignof_slot(Size12);
      ffi_type_Size12.elements = ffi_type_Size12_elements;
      ffi_cif cif;
      FFI_PREP_CIF_NOARGS(cif,ffi_type_Size12);
      {
        FFI_CALL(cif,S12_v,NULL,&r);
      }
    }
    FPRINTF(out,"->{%c%c%c%c%c%c%c%c%c%c%c%c}\n",r.x1,r.x2,r.x3,r.x4,r.x5,r.x6,r.x7,r.x8,r.x9,r.x10,r.x11,r.x12);
    fflush(out);
  }
#endif
#if (!defined(DGTEST)) || DGTEST == 51
  {
    Size15 r = S15_v();
    FPRINTF(out,"->{%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c}\n",r.x1,r.x2,r.x3,r.x4,r.x5,r.x6,r.x7,r.x8,r.x9,r.x10,r.x11,r.x12,r.x13,r.x14,r.x15);
    fflush(out);
    memset(&r,0,sizeof(r)); clear_traces();
    {
      ffi_type* ffi_type_Size15_elements[] = { &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, NULL };
      ffi_type ffi_type_Size15;
      ffi_type_Size15.type = FFI_TYPE_STRUCT;
      ffi_type_Size15.size = sizeof(Size15);
      ffi_type_Size15.alignment = alignof_slot(Size15);
      ffi_type_Size15.elements = ffi_type_Size15_elements;
      ffi_cif cif;
      FFI_PREP_CIF_NOARGS(cif,ffi_type_Size15);
      {
        FFI_CALL(cif,S15_v,NULL,&r);
      }
    }
    FPRINTF(out,"->{%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c}\n",r.x1,r.x2,r.x3,r.x4,r.x5,r.x6,r.x7,r.x8,r.x9,r.x10,r.x11,r.x12,r.x13,r.x14,r.x15);
    fflush(out);
  }
#endif
#if (!defined(DGTEST)) || DGTEST == 52
  {
    Size16 r = S16_v();
    FPRINTF(out,"->{%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c}\n",r.x1,r.x2,r.x3,r.x4,r.x5,r.x6,r.x7,r.x8,r.x9,r.x10,r.x11,r.x12,r.x13,r.x14,r.x15,r.x16);
    fflush(out);
    memset(&r,0,sizeof(r)); clear_traces();
    {
      ffi_type* ffi_type_Size16_elements[] = { &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, &ffi_type_char, NULL };
      ffi_type ffi_type_Size16;
      ffi_type_Size16.type = FFI_TYPE_STRUCT;
      ffi_type_Size16.size = sizeof(Size16);
      ffi_type_Size16.alignment = alignof_slot(Size16);
      ffi_type_Size16.elements = ffi_type_Size16_elements;
      ffi_cif cif;
      FFI_PREP_CIF_NOARGS(cif,ffi_type_Size16);
      {
        FFI_CALL(cif,S16_v,NULL,&r);
      }
    }
    FPRINTF(out,"->{%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c}\n",r.x1,r.x2,r.x3,r.x4,r.x5,r.x6,r.x7,r.x8,r.x9,r.x10,r.x11,r.x12,r.x13,r.x14,r.x15,r.x16);
    fflush(out);
  }
#endif
}
void
  structure_tests (void)
{
  Int Ir;
  Char Cr;
  Float Fr;
  Double Dr;
  J Jr;
#ifndef SKIP_EXTRA_STRUCTS
  T Tr;
  X Xr;
#endif

#if (!defined(DGTEST)) || DGTEST == 53
  Ir = I_III(I1,I2,I3);
  FPRINTF(out,"->{%d}\n",Ir.x);
  fflush(out);
  Ir.x = 0; clear_traces();
  {
    ffi_type* ffi_type_Int_elements[] = { &ffi_type_sint, NULL };
    ffi_type ffi_type_Int;
    ffi_type_Int.type = FFI_TYPE_STRUCT;
    ffi_type_Int.size = sizeof(Int);
    ffi_type_Int.alignment = alignof_slot(Int);
    ffi_type_Int.elements = ffi_type_Int_elements;
    ffi_type* argtypes[] = { &ffi_type_Int, &ffi_type_Int, &ffi_type_Int };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_Int);
    {
      /*const*/ void* args[] = { &I1, &I2, &I3 };
      FFI_CALL(cif,I_III,args,&Ir);
    }
  }
  FPRINTF(out,"->{%d}\n",Ir.x);
  fflush(out);
#endif
#if (!defined(DGTEST)) || DGTEST == 54
  Cr = C_CdC(C1,d2,C3);
  FPRINTF(out,"->{'%c'}\n",Cr.x);
  fflush(out);
  Cr.x = '\0'; clear_traces();
  {
    ffi_type* ffi_type_Char_elements[] = { &ffi_type_char, NULL };
    ffi_type ffi_type_Char;
    ffi_type_Char.type = FFI_TYPE_STRUCT;
    ffi_type_Char.size = sizeof(Char);
    ffi_type_Char.alignment = alignof_slot(Char);
    ffi_type_Char.elements = ffi_type_Char_elements;
    ffi_type* argtypes[] = { &ffi_type_Char, &ffi_type_double, &ffi_type_Char };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_Char);
    {
      /*const*/ void* args[] = { &C1, &d2, &C3 };
      FFI_CALL(cif,C_CdC,args,&Cr);
    }
  }
  FPRINTF(out,"->{'%c'}\n",Cr.x);
  fflush(out);
#endif
#if (!defined(DGTEST)) || DGTEST == 55
  Fr = F_Ffd(F1,f2,d3);
  FPRINTF(out,"->{%g}\n",Fr.x);
  fflush(out);
  Fr.x = 0.0; clear_traces();
  {
    ffi_type* ffi_type_Float_elements[] = { &ffi_type_float, NULL };
    ffi_type ffi_type_Float;
    ffi_type_Float.type = FFI_TYPE_STRUCT;
    ffi_type_Float.size = sizeof(Float);
    ffi_type_Float.alignment = alignof_slot(Float);
    ffi_type_Float.elements = ffi_type_Float_elements;
    ffi_type* argtypes[] = { &ffi_type_Float, &ffi_type_float, &ffi_type_double };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_Float);
    {
      /*const*/ void* args[] = { &F1, &f2, &d3 };
      FFI_CALL(cif,F_Ffd,args,&Fr);
    }
  }
  FPRINTF(out,"->{%g}\n",Fr.x);
  fflush(out);
#endif
#if (!defined(DGTEST)) || DGTEST == 56
  Dr = D_fDd(f1,D2,d3);
  FPRINTF(out,"->{%g}\n",Dr.x);
  fflush(out);
  Dr.x = 0.0; clear_traces();
  {
    ffi_type* ffi_type_Double_elements[] = { &ffi_type_double, NULL };
    ffi_type ffi_type_Double;
    ffi_type_Double.type = FFI_TYPE_STRUCT;
    ffi_type_Double.size = sizeof(Double);
    ffi_type_Double.alignment = alignof_slot(Double);
    ffi_type_Double.elements = ffi_type_Double_elements;
    ffi_type* argtypes[] = { &ffi_type_float, &ffi_type_Double, &ffi_type_double };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_Double);
    {
      /*const*/ void* args[] = { &f1, &D2, &d3 };
      FFI_CALL(cif,D_fDd,args,&Dr);
    }
  }
  FPRINTF(out,"->{%g}\n",Dr.x);
  fflush(out);
#endif
#if (!defined(DGTEST)) || DGTEST == 57
  Dr = D_Dfd(D1,f2,d3);
  FPRINTF(out,"->{%g}\n",Dr.x);
  fflush(out);
  Dr.x = 0.0; clear_traces();
  {
    ffi_type* ffi_type_Double_elements[] = { &ffi_type_double, NULL };
    ffi_type ffi_type_Double;
    ffi_type_Double.type = FFI_TYPE_STRUCT;
    ffi_type_Double.size = sizeof(Double);
    ffi_type_Double.alignment = alignof_slot(Double);
    ffi_type_Double.elements = ffi_type_Double_elements;
    ffi_type* argtypes[] = { &ffi_type_Double, &ffi_type_float, &ffi_type_double };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_Double);
    {
      /*const*/ void* args[] = { &D1, &f2, &d3 };
      FFI_CALL(cif,D_Dfd,args,&Dr);
    }
  }
  FPRINTF(out,"->{%g}\n",Dr.x);
  fflush(out);
#endif
#if (!defined(DGTEST)) || DGTEST == 58
  Jr = J_JiJ(J1,i2,J2);
  FPRINTF(out,"->{%ld,%ld}\n",Jr.l1,Jr.l2);
  fflush(out);
  Jr.l1 = Jr.l2 = 0; clear_traces();
  {
    ffi_type* ffi_type_J_elements[] = { &ffi_type_slong, &ffi_type_slong, NULL };
    ffi_type ffi_type_J;
    ffi_type_J.type = FFI_TYPE_STRUCT;
    ffi_type_J.size = sizeof(J);
    ffi_type_J.alignment = alignof_slot(J);
    ffi_type_J.elements = ffi_type_J_elements;
    ffi_type* argtypes[] = { &ffi_type_J, &ffi_type_sint, &ffi_type_J };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_J);
    {
      /*const*/ void* args[] = { &J1, &i2, &J2 };
      FFI_CALL(cif,J_JiJ,args,&Jr);
    }
  }
  FPRINTF(out,"->{%ld,%ld}\n",Jr.l1,Jr.l2);
  fflush(out);
#endif
#ifndef SKIP_EXTRA_STRUCTS
#if (!defined(DGTEST)) || DGTEST == 59
  Tr = T_TcT(T1,' ',T2);
  FPRINTF(out,"->{\"%c%c%c\"}\n",Tr.c[0],Tr.c[1],Tr.c[2]);
  fflush(out);
  Tr.c[0] = Tr.c[1] = Tr.c[2] = 0; clear_traces();
  {
    ffi_type* ffi_type_T_elements[] = { ??, NULL };
    ffi_type ffi_type_T;
    ffi_type_T.type = FFI_TYPE_STRUCT;
    ffi_type_T.size = sizeof(T);
    ffi_type_T.alignment = alignof_slot(T);
    ffi_type_T.elements = ffi_type_T_elements;
    ffi_type* argtypes[] = { &ffi_type_T, &ffi_type_char, &ffi_type_T };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_T);
    {
      char space = ' ';
      /*const*/ void* args[] = { &T1, &space, &T2 };
      FFI_CALL(cif,T_TcT,args,&Tr);
    }
  }
  FPRINTF(out,"->{\"%c%c%c\"}\n",Tr.c[0],Tr.c[1],Tr.c[2]);
  fflush(out);
#endif
#if (!defined(DGTEST)) || DGTEST == 60
  Xr = X_BcdB(B1,c2,d3,B2);
  FPRINTF(out,"->{\"%s\",'%c'}\n",Xr.c,Xr.c1);
  fflush(out);
  Xr.c[0]=Xr.c1='\0'; clear_traces();
  {
    ffi_type* ffi_type_X_elements[] = { ??, NULL };
    ffi_type ffi_type_X;
    ffi_type_X.type = FFI_TYPE_STRUCT;
    ffi_type_X.size = sizeof(X);
    ffi_type_X.alignment = alignof_slot(X);
    ffi_type_X.elements = ffi_type_X_elements;
    ffi_type* argtypes[] = { &ffi_type_X, &ffi_type_char, &ffi_type_double, &ffi_type_X };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_X);
    {
      /*const*/ void* args[] = { &B1, &c2, &d3, &B2 };
      FFI_CALL(cif,X_BcdB,args,&Xr);
    }
  }
  FPRINTF(out,"->{\"%s\",'%c'}\n",Xr.c,Xr.c1);
  fflush(out);
#endif
#endif

  return;
}

void
  gpargs_boundary_tests (void)
{
  ffi_type* ffi_type_K_elements[] = { &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, NULL };
  ffi_type ffi_type_K;
  ffi_type* ffi_type_L_elements[] = { &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, NULL };
  ffi_type ffi_type_L;
  long lr;
  long long llr;
  float fr;
  double dr;

  ffi_type_K.type = FFI_TYPE_STRUCT;
  ffi_type_K.size = sizeof(K);
  ffi_type_K.alignment = alignof_slot(K);
  ffi_type_K.elements = ffi_type_K_elements;

  ffi_type_L.type = FFI_TYPE_STRUCT;
  ffi_type_L.size = sizeof(L);
  ffi_type_L.alignment = alignof_slot(L);
  ffi_type_L.elements = ffi_type_L_elements;

#if (!defined(DGTEST)) || DGTEST == 61
  lr = l_l0K(K1,l9);
  FPRINTF(out,"->%ld\n",lr);
  fflush(out);
  lr = 0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_K, &ffi_type_slong };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_slong);
    {
      /*const*/ void* args[] = { &K1, &l9 };
      FFI_CALL(cif,l_l0K,args,&lr);
    }
  }
  FPRINTF(out,"->%ld\n",lr);
  fflush(out);
#endif
#if (!defined(DGTEST)) || DGTEST == 62
  lr = l_l1K(l1,K1,l9);
  FPRINTF(out,"->%ld\n",lr);
  fflush(out);
  lr = 0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_slong, &ffi_type_K, &ffi_type_slong };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_slong);
    {
      /*const*/ void* args[] = { &l1, &K1, &l9 };
      FFI_CALL(cif,l_l1K,args,&lr);
    }
  }
  FPRINTF(out,"->%ld\n",lr);
  fflush(out);
#endif
#if (!defined(DGTEST)) || DGTEST == 63
  lr = l_l2K(l1,l2,K1,l9);
  FPRINTF(out,"->%ld\n",lr);
  fflush(out);
  lr = 0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_slong, &ffi_type_slong, &ffi_type_K, &ffi_type_slong };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_slong);
    {
      /*const*/ void* args[] = { &l1, &l2, &K1, &l9 };
      FFI_CALL(cif,l_l2K,args,&lr);
    }
  }
  FPRINTF(out,"->%ld\n",lr);
  fflush(out);
#endif
#if (!defined(DGTEST)) || DGTEST == 64
  lr = l_l3K(l1,l2,l3,K1,l9);
  FPRINTF(out,"->%ld\n",lr);
  fflush(out);
  lr = 0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_K, &ffi_type_slong };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_slong);
    {
      /*const*/ void* args[] = { &l1, &l2, &l3, &K1, &l9 };
      FFI_CALL(cif,l_l3K,args,&lr);
    }
  }
  FPRINTF(out,"->%ld\n",lr);
  fflush(out);
#endif
#if (!defined(DGTEST)) || DGTEST == 65
  lr = l_l4K(l1,l2,l3,l4,K1,l9);
  FPRINTF(out,"->%ld\n",lr);
  fflush(out);
  lr = 0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_K, &ffi_type_slong };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_slong);
    {
      /*const*/ void* args[] = { &l1, &l2, &l3, &l4, &K1, &l9 };
      FFI_CALL(cif,l_l4K,args,&lr);
    }
  }
  FPRINTF(out,"->%ld\n",lr);
  fflush(out);
#endif
#if (!defined(DGTEST)) || DGTEST == 66
  lr = l_l5K(l1,l2,l3,l4,l5,K1,l9);
  FPRINTF(out,"->%ld\n",lr);
  fflush(out);
  lr = 0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_K, &ffi_type_slong };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_slong);
    {
      /*const*/ void* args[] = { &l1, &l2, &l3, &l4, &l5, &K1, &l9 };
      FFI_CALL(cif,l_l5K,args,&lr);
    }
  }
  FPRINTF(out,"->%ld\n",lr);
  fflush(out);
#endif
#if (!defined(DGTEST)) || DGTEST == 67
  lr = l_l6K(l1,l2,l3,l4,l5,l6,K1,l9);
  FPRINTF(out,"->%ld\n",lr);
  fflush(out);
  lr = 0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_K, &ffi_type_slong };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_slong);
    {
      /*const*/ void* args[] = { &l1, &l2, &l3, &l4, &l5, &l6, &K1, &l9 };
      FFI_CALL(cif,l_l6K,args,&lr);
    }
  }
  FPRINTF(out,"->%ld\n",lr);
  fflush(out);
#endif
#if (!defined(DGTEST)) || DGTEST == 68
  fr = f_f17l3L(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,l6,l7,l8,L1);
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
  fr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_float, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_L };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_float);
    {
      /*const*/ void* args[] = { &f1, &f2, &f3, &f4, &f5, &f6, &f7, &f8, &f9, &f10, &f11, &f12, &f13, &f14, &f15, &f16, &f17, &l6, &l7, &l8, &L1 };
      FFI_CALL(cif,f_f17l3L,args,&fr);
    }
  }
  FPRINTF(out,"->%g\n",fr);
  fflush(out);
#endif
#if (!defined(DGTEST)) || DGTEST == 69
  dr = d_d17l3L(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,l6,l7,l8,L1);
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
  dr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_double, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_L };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_double);
    {
      /*const*/ void* args[] = { &d1, &d2, &d3, &d4, &d5, &d6, &d7, &d8, &d9, &d10, &d11, &d12, &d13, &d14, &d15, &d16, &d17, &l6, &l7, &l8, &L1 };
      FFI_CALL(cif,d_d17l3L,args,&dr);
    }
  }
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
#endif
#if (!defined(DGTEST)) || DGTEST == 70
  llr = ll_l2ll(l1,l2,ll1,l9);
  FPRINTF(out,"->0x%lx%08lx\n",(long)(llr>>32),(long)(llr&0xffffffff));
  fflush(out);
  llr = 0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_slong, &ffi_type_slong, &ffi_type_slonglong, &ffi_type_slong };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_slonglong);
    {
      /*const*/ void* args[] = { &l1, &l2, &ll1, &l9 };
      FFI_CALL(cif,ll_l2ll,args,&llr);
    }
  }
  FPRINTF(out,"->0x%lx%08lx\n",(long)(llr>>32),(long)(llr&0xffffffff));
  fflush(out);
#endif
#if (!defined(DGTEST)) || DGTEST == 71
  llr = ll_l3ll(l1,l2,l3,ll1,l9);
  FPRINTF(out,"->0x%lx%08lx\n",(long)(llr>>32),(long)(llr&0xffffffff));
  fflush(out);
  llr = 0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slonglong, &ffi_type_slong };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_slonglong);
    {
      /*const*/ void* args[] = { &l1, &l2, &l3, &ll1, &l9 };
      FFI_CALL(cif,ll_l3ll,args,&llr);
    }
  }
  FPRINTF(out,"->0x%lx%08lx\n",(long)(llr>>32),(long)(llr&0xffffffff));
  fflush(out);
#endif
#if (!defined(DGTEST)) || DGTEST == 72
  llr = ll_l4ll(l1,l2,l3,l4,ll1,l9);
  FPRINTF(out,"->0x%lx%08lx\n",(long)(llr>>32),(long)(llr&0xffffffff));
  fflush(out);
  llr = 0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slonglong, &ffi_type_slong };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_slonglong);
    {
      /*const*/ void* args[] = { &l1, &l2, &l3, &l4, &ll1, &l9 };
      FFI_CALL(cif,ll_l4ll,args,&llr);
    }
  }
  FPRINTF(out,"->0x%lx%08lx\n",(long)(llr>>32),(long)(llr&0xffffffff));
  fflush(out);
#endif
#if (!defined(DGTEST)) || DGTEST == 73
  llr = ll_l5ll(l1,l2,l3,l4,l5,ll1,l9);
  FPRINTF(out,"->0x%lx%08lx\n",(long)(llr>>32),(long)(llr&0xffffffff));
  fflush(out);
  llr = 0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slonglong, &ffi_type_slong };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_slonglong);
    {
      /*const*/ void* args[] = { &l1, &l2, &l3, &l4, &l5, &ll1, &l9 };
      FFI_CALL(cif,ll_l5ll,args,&llr);
    }
  }
  FPRINTF(out,"->0x%lx%08lx\n",(long)(llr>>32),(long)(llr&0xffffffff));
  fflush(out);
#endif
#if (!defined(DGTEST)) || DGTEST == 74
  llr = ll_l6ll(l1,l2,l3,l4,l5,l6,ll1,l9);
  FPRINTF(out,"->0x%lx%08lx\n",(long)(llr>>32),(long)(llr&0xffffffff));
  fflush(out);
  llr = 0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slonglong, &ffi_type_slong };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_slonglong);
    {
      /*const*/ void* args[] = { &l1, &l2, &l3, &l4, &l5, &l6, &ll1, &l9 };
      FFI_CALL(cif,ll_l6ll,args,&llr);
    }
  }
  FPRINTF(out,"->0x%lx%08lx\n",(long)(llr>>32),(long)(llr&0xffffffff));
  fflush(out);
#endif
#if (!defined(DGTEST)) || DGTEST == 75
  llr = ll_l7ll(l1,l2,l3,l4,l5,l6,l7,ll1,l9);
  FPRINTF(out,"->0x%lx%08lx\n",(long)(llr>>32),(long)(llr&0xffffffff));
  fflush(out);
  llr = 0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slonglong, &ffi_type_slong };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_slonglong);
    {
      /*const*/ void* args[] = { &l1, &l2, &l3, &l4, &l5, &l6, &l7, &ll1, &l9 };
      FFI_CALL(cif,ll_l7ll,args,&llr);
    }
  }
  FPRINTF(out,"->0x%lx%08lx\n",(long)(llr>>32),(long)(llr&0xffffffff));
  fflush(out);
#endif
#if (!defined(DGTEST)) || DGTEST == 76
  dr = d_l2d(l1,l2,d2,l9);
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
  dr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_slong, &ffi_type_slong, &ffi_type_double, &ffi_type_slong };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_double);
    {
      /*const*/ void* args[] = { &l1, &l2, &d2, &l9 };
      FFI_CALL(cif,d_l2d,args,&dr);
    }
  }
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
#endif
#if (!defined(DGTEST)) || DGTEST == 77
  dr = d_l3d(l1,l2,l3,d2,l9);
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
  dr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_double, &ffi_type_slong };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_double);
    {
      /*const*/ void* args[] = { &l1, &l2, &l3, &d2, &l9 };
      FFI_CALL(cif,d_l3d,args,&dr);
    }
  }
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
#endif
#if (!defined(DGTEST)) || DGTEST == 78
  dr = d_l4d(l1,l2,l3,l4,d2,l9);
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
  dr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_double, &ffi_type_slong };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_double);
    {
      /*const*/ void* args[] = { &l1, &l2, &l3, &l4, &d2, &l9 };
      FFI_CALL(cif,d_l4d,args,&dr);
    }
  }
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
#endif
#if (!defined(DGTEST)) || DGTEST == 79
  dr = d_l5d(l1,l2,l3,l4,l5,d2,l9);
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
  dr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_double, &ffi_type_slong };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_double);
    {
      /*const*/ void* args[] = { &l1, &l2, &l3, &l4, &l5, &d2, &l9 };
      FFI_CALL(cif,d_l5d,args,&dr);
    }
  }
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
#endif
#if (!defined(DGTEST)) || DGTEST == 80
  dr = d_l6d(l1,l2,l3,l4,l5,l6,d2,l9);
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
  dr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_double, &ffi_type_slong };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_double);
    {
      /*const*/ void* args[] = { &l1, &l2, &l3, &l4, &l5, &l6, &d2, &l9 };
      FFI_CALL(cif,d_l6d,args,&dr);
    }
  }
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
#endif
#if (!defined(DGTEST)) || DGTEST == 81
  dr = d_l7d(l1,l2,l3,l4,l5,l6,l7,d2,l9);
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
  dr = 0.0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_slong, &ffi_type_double, &ffi_type_slong };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_double);
    {
      /*const*/ void* args[] = { &l1, &l2, &l3, &l4, &l5, &l6, &l7, &d2, &l9 };
      FFI_CALL(cif,d_l7d,args,&dr);
    }
  }
  FPRINTF(out,"->%g\n",dr);
  fflush(out);
#endif
  return;
}

void
  string_tests (void)
{
  int ir;
  ulong ulr;
#if (!defined(DGTEST)) || DGTEST == 82
  /* standard libffi convention for FFI_TYPE_POINTER is to pass-by-address */
  ir = i_cpcp(str1, str2);
  FPRINTF(out,"->%d\n",ir);
  fflush(out);
  ir = 0; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_pointer, &ffi_type_pointer };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_sint);
    {
#if 0 /* fails: */
      /*const*/ void* args[] = { (void*)&str1, (void*)&str2 };
#else
      void* pstr1 = str1;
      void* pstr2 = str2;
      /*const*/ void* args[] = { &pstr1, (void*)&pstr2 };
#endif
      FFI_CALL(cif, i_cpcp, args, &ir);
    }
  }
  FPRINTF(out,"->%d\n",ir);
  fflush(out);
  /* */
  ulr = ul_cp3(str1,str2,str3);
  FPRINTF(out,"->%lu\n",ulr);
  fflush(out);
  ulr = 0UL; clear_traces();
  {
    ffi_type* argtypes[] = { &ffi_type_pointer, &ffi_type_pointer, &ffi_type_pointer };
    ffi_cif cif;
    FFI_PREP_CIF(cif,argtypes,ffi_type_pointer);
    {
      void* pstr1 = str1;
      void* pstr2 = str2;
      void* pstr3 = str3;
      /*const*/ void* args[] = { &pstr1, &pstr2, &pstr3 };
      FFI_CALL(cif,ul_cp3,args,&ulr);
    }
  }
  FPRINTF(out,"->%lu\n",ulr);
  fflush(out);
#endif
  return;
}

#ifdef VARIADIC_SUPPORT
void
  variadic_tests (void)
{
#if (!defined(DGTEST)) || DGTEST == 83
  /* */
  {
    double dr;
    dr = d_divar(d1,3, d2,d3,d4);
    FPRINTF(out,"->%g\n",dr); fflush(out);
    dr = 0; clear_traces();
    {
      ffi_type* argtypes[] = { &ffi_type_double, &ffi_type_uint, &ffi_type_double, &ffi_type_double, &ffi_type_double };
      ffi_cif cif;
      FFI_PREP_CIF_VAR(cif,2,argtypes,ffi_type_double); /* 2 fixed args */
      {
        unsigned varcount = 3U;
        /*const*/ void* args[] = { &d1, &varcount, &d2, &d3, &d4 };
        FFI_CALL(cif, d_divar, args, &dr);
      }
    }
    FPRINTF(out,"->%g\n",dr); fflush(out);
  }
  /* */
  {
    double dr;
    void *null=NULL;
    dr = d_pdvar(&d1,&d2,&d3,&d4,null);
    FPRINTF(out,"->%g\n",dr); fflush(out);
    dr = 0; clear_traces();
    {
      ffi_type* argtypes[] = { &ffi_type_pointer, &ffi_type_pointer, &ffi_type_pointer, &ffi_type_pointer, &ffi_type_pointer };
      ffi_cif cif;
      FFI_PREP_CIF_VAR(cif,2,argtypes,ffi_type_double); /* 2 fixed args */
      {
        double *pd1=&d1;
        double *pd2=&d2;
        double *pd3=&d3;
        double *pd4=&d4;
        void *pnull=&null;
        /*const*/ void* args[] = { &pd1, &pd2, &pd3, &pd4, pnull };
        FFI_CALL(cif, d_pdvar, args, &dr);
      }
    }
    FPRINTF(out,"->%g\n",dr); fflush(out);
  }
  /* */
  {
    char *fmt="%s AND %s AND %s";
    char buf[120];
    int ir = i_cpivar(buf,fmt,str1,str2,str3);
    FPRINTF(out,"->%d\n",ir); fflush(out);
    ir = 0; clear_traces();
    {
      ffi_type* argtypes[] = { &ffi_type_pointer, &ffi_type_pointer, &ffi_type_pointer, &ffi_type_pointer, &ffi_type_pointer };
      ffi_cif cif;
      FFI_PREP_CIF_VAR(cif,2,argtypes,ffi_type_double); /* 2 fixed args */
      {
        char* pbuf=&buf[0];
        char* pfmt = fmt;
        void* pstr1 = str1;
        void* pstr2 = str2;
        void* pstr3 = str3;
        /*const*/ void* args[] = { &pbuf, &pfmt, &pstr1, &pstr2, &pstr3 };
        FFI_CALL(cif, i_cpivar, args, &ir);
      }
    }
    FPRINTF(out,"->%d\n",ir); fflush(out);
  }
  /* */
  {
    {
      int ir = i_i5uvari(1,2,3,4,5,8,1,7,2,7,3,1,7,7); /*ans?=7*/
      FPRINTF(out,"->%d\n",ir); fflush(out);
    }
    int ir = i_i5uvari(i1,i2,i3,i4,i5,8,i1,i7,i2,i7,i3,i1,i7,i7); /*ans?=7*/
    FPRINTF(out,"->%d\n",ir); fflush(out);
    ir = 0; clear_traces();
    {
      ffi_type* argtypes[] = { &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_uint,
        &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_sint, &ffi_type_sint };
      ffi_cif cif;
      FFI_PREP_CIF_VAR(cif,6,argtypes,ffi_type_sint); /* 6 fixed args */
      {
        unsigned cnt = 8U;
        int a=i1,b=i7,c=i2,d=i7,e=i3,f=i1,g=i7,h=i7;
        /*const*/ void* args[] = {&i1,&i2,&i3,&i4,&i5, &cnt, &a,&b,&c,&d, &e,&f,&g,&h};
        FFI_CALL(cif, i_i5uvari, args, &ir);
      }
    }
    FPRINTF(out,"->%d\n",ir); fflush(out);
  }
#endif
  return;
}
#endif //VARIADIC_SUPPORT

int
  main (void)
{
  ffi_type_char = (char)(-1) < 0 ? ffi_type_schar : ffi_type_uchar;
  out = stdout;

  void_tests();
  int_tests();
  float_tests();
  double_tests();
#if HAVE_LONG_DOUBLE
  long_double_tests();
#endif
#if HAVE_COMPLEX
  float_complex_tests();
  double_complex_tests();
  long_double_complex_tests();
#endif
  pointer_tests();
  mixed_number_tests();
  small_structure_return_tests();
  structure_tests();
  gpargs_boundary_tests();
  string_tests();
  variadic_tests();

  printf("test-call: normal exit\n");
  printf("test-call: normal exit\n"); /* print it twice, so uniq doesn't think FAIL */
  fflush(stdout);
  exit(0);
}
/* vim: set sw=2 ts=2 et: */
