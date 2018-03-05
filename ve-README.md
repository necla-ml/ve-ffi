
## How to Cross Compile OSS
From Japan internal wiki:
- autoreconf
  - I had to add some search directories, to avoid missing includes:

ve-autogen.sh
~
source ~/kruus/vt/env.bash --ve
autoreconf -B /opt/nec/ve/share/autoconf-1.13 -B /opt/nec/ve/share/libtool/config -v -i
~

~
 /opt/nec/ve/bin/autoreconf -ifv
~

- configure

~
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
~
---------------------
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

-----------------

in vtorch, build a local makeinfo, because texinfo seems not to be installed
Workaround:
after automake, change the line

MAKEINFO = ${SHELL} /usr/uhome/aurora/4gi/nlabhpg/kruus/ffi/libffi/missing makeinfo

to 

MAKEINFO = true
---------------------
installed texinfo-6.1. try again.
---------------------

