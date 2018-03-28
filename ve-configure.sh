#PREFIX=`pwd`/install
OPTS='-g2 -O1'
# do not add -fPIC here. libffi builds both .a and .so
if [ -f ~/kruus/vt/env.bash ]; then
	source ~/kruus/vt/env.bash --ve
fi
echo 'Not using:
   CCAS="nas" \
   AS="ncc" \
   CCASFLAGS="" \
   '

echo "ve-configure.sh : install PREFIX=$PREFIX"
echo "                : CFLAGS=$CFLAGS"
echo "                : LDFLAGS=$LDFLAGS"
./configure \
   --build=x86_64-unknown-linux-gnu \
   --host=ve-unknown-linux-gnu \
   --prefix="$PREFIX" \
   --enable-debug \
   --disable-structs \
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
   OBJDUMP="nobjdump"

# nstrip not determined correctly?

