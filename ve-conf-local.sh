#PREFIX=`pwd`/install
OPTS='-g2 -O1 -fPIC'
BUILDDIR=build
INSTALLDIR=install
PREFIX="`pwd`/${INSTALLDIR}"
if [ -f ~/kruus/vt/env.bash ]; then
	source ~/kruus/vt/env.bash --ve
fi
echo 'Not using:
   AS="ncc" \
   CCAS="nas" \
   CCASFLAGS="" \
   '
pushd >& /dev/null
mkdir -p "${BUILDDIR}"
{
cd "${BUILDDIR}";
../configure \
   --build=x86_64-unknown-linux-gnu \
   --host=ve-unknown-linux-gnu \
   --prefix="$PREFIX" \
   $@ \
   CC="ncc" \
   CFLAGS="$OPTS" \
   CXX="nc++" \
   CXXFLAGS="$OPTS" \
   FC="nfort" \
   FCFLAGS="$OPTS" \
   CCAS="ncc" \
   CCASFLAGS="" \
   LD="nld" \
   CXXCPP="ncc -E" \
   LDFLAGS="-Wl,-z,max-page-size=0x200000" \
   READELF="nreadelf" \
   RANLIB="nranlib" \
   STRIP="nstrip";
}
popd >& /dev/null
# nstrip not determined correctly?

