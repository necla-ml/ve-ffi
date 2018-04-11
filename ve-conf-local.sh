#PREFIX=`pwd`/install
OPTS='-g2 -O1 -fPIC'
OPTS='-g2 -O1'
BUILDDIR=build
INSTALLDIR=install
PREFIX="`pwd`/${INSTALLDIR}"
if [ -f ~/kruus/vt/env.bash ]; then
       source ~/kruus/vt/env.bash --ve
fi
echo 'Not using:
   AS="ncc" \
       -   CCAS="nas" \
       -   CCASFLAGS="" \
       -   '
mkdir -p "${BUILDDIR}"
rootdir=`pwd`
{
cd "${BUILDDIR}";
../configure \
  --build=x86_64-unknown-linux-gnu \
  --host=ve-unknown-linux-gnu \
  --prefix="$PREFIX" \
   --enable-debug \
   --disable-raw-api \
   --disable-multi-os-directory \
   $@ \
   CC="ncc" \
   CFLAGS="$OPTS" \
   CXX="nc++" \
   CXXFLAGS="$OPTS" \
   FC="nfort" \
   FCFLAGS="$OPTS" \
   CCAS="ncc" \
   CCASFLAGS="" \
   AS="ncc" \
   LD="nld" \
   CXXCPP="ncc -E" \
   LDFLAGS="-Wl,-z,max-page-size=0x200000" \
   READELF="nreadelf" \
   RANLIB="nranlib" \
   STRIP="nstrip" \
   OBJDUMP="nobjdump" \
   ;
}
cd "$rootdir"
# nstrip not determined correctly?


