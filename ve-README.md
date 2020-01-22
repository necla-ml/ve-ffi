## How to Cross Compile libffi for NEC VE "Aurora" processor

```
source ~/kruus/vt/env.bash --ve # or however you set up ncc environment
. ve-autogen.sh >& ve-autogen.log
```

Local build --> ./install/
```
. ve-conf-local.sh >& ve-conf-local.log
cd build && make
make install
```

Local test
```
rm "${VE_VE}/lib/libffi*"   # if you have some alternate (conflicting) install location
cd testsuite/libffi.bhaible # this does not need expect or tk like rest of libffi testsuite
make clean; make
# or just ..
(cd testsuite/libffi.bhaible/; make clean; make VERBOSE=1; ) >& tst.log
```

debugging output (NEC ve had issues with passing various complex types as arguments)
Edit src/ve/ffitarget.h and change debug flags:
```
/* ----- VE-specific options --------------------------------------------- */
/* in [0,5] */
#define VE_DEBUG_LEVEL 5
/* in [0,1], not very useful at VE_DEBUG_LEVEL 0 */
#define VE_SYSV_DEBUG 1
```

rebuild, reinstall, test again, run gdb and set breakpoints as per
src/ve/ffi.c and src/ve/sysv.S

To fix ABI issues when ncc changes, please rebuild the asm-examples/ and
look at actually ncc output to find out what the ABI is currently doing.

```
cd ../../asm-examples # if you are in src/git/libffi for vtorch project
. doit.sh
ls -l asm
```

### older hints ...

From Japan internal wiki:
- autoreconf
  - I had to add some search directories, to avoid missing includes:
```
autoreconf -B /opt/nec/ve/share/autoconf-1.13 -B /opt/nec/ve/share/libtool/config -v -i
```

```
 /opt/nec/ve/bin/autoreconf -ifv
```

- configure

```
 PREFIX=PATH_TO_INSTALL_OSS
 OPTS="compiler option"
 
 ./configure \
   --build=x86_64-unknown-linux-gnu \
   --host=ve-unknown-linux-gnu \
   --prefix="$PREFIX" \
   CC="ncc" \
   CFLAGS="$OPTS" \
   CXX="nc++" \
   CXXFLAGS="$OPTS" \
   FC="nfort" \
   FCFLAGS="$OPTS" \
   CCAS="nas" \
   CCASFLAGS="" \
   LDFLAGS="-Wl,-z,max-page-size=0x200000" 
```
---------------------
```
BUG: /opt/nec/ve/share/aclocal/libtool.m4
     incorrectorly determines ncc search directories,
     because ncc -print-search-dirs does not work.
NONFIX: the include directories can be gotten by compiling a dummy hello.c
     with 'ncc -v -E hello.c', but this seems to be running system
     preprocessor that reports incorrect lib paths.
Potential impact:
     bad feature/library/function availability checks?
     trying to linkwith x86 libs?

BAD: /usr/bin/ld  should be /opt/nec/ve/bin/nld
```
-----------------

in vtorch, build a local makeinfo, because texinfo seems not to be installed

Workaround:
after automake, change the line
```
MAKEINFO = ${SHELL} /usr/uhome/aurora/4gi/nlabhpg/kruus/ffi/libffi/missing makeinfo
```
to 
```
MAKEINFO = true
---------------------
installed texinfo-6.1. try again.
---------------------
```

