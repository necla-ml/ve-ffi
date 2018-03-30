/* -----------------------------------------------------------------*-C-*-
   ffitarget.h - Copyright (c) 2012  Anthony Green
                 Copyright (c) 1996-2003  Red Hat, Inc.
   Target configuration macros for SuperH - SHmedia.

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

#ifndef LIBFFI_TARGET_H
#define LIBFFI_TARGET_H

#ifndef LIBFFI_H
#error "Please do not include ffitarget.h directly into your source.  Use ffi.h instead."
#endif

/* ---- Generic type definitions ----------------------------------------- */

#ifndef LIBFFI_ASM
typedef unsigned long          ffi_arg;
typedef signed long            ffi_sarg;

typedef enum ffi_abi {
  FFI_FIRST_ABI = 0,
  FFI_SYSV,
  FFI_LAST_ABI,
  FFI_DEFAULT_ABI = FFI_SYSV
} ffi_abi;

#define FFI_EXTRA_CIF_FIELDS long unsigned flags2
// TODO
//#define FFI_TARGET_SPECIFIC_VARIADIC 32
#endif

/* ---- Definitions for closures ----------------------------------------- */
#define FFI_CLOSURES 0
//#define FFI_TRAMPOLINE_SIZE 32
//#define FFI_NATIVE_RAW_API 0

/* ----- VE-specific options --------------------------------------------- */
/* in [0,5] */
#define VE_DEBUG_LEVEL 5
/* in [0,1], not very useful at VE_DEBUG_LEVEL 0 */
#define SYSV_DEBUG 0

/* Optional easier-to-use pass-pointers-by-value convention.
 *
 * libffi tests pass ADDR-of-pointer in args[],
 * rather than directly giving the pointer VALUE.
 * I don't really understand libffi rationale for this complication.
 *
 * Default is 0       to agree with standard libffi convention
 */
#define VE_POINTER_BY_VALUE 0
#endif

