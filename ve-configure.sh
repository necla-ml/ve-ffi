#PREFIX=`pwd`/install
OPTS='-g2 -O1 -fPIC'
if [ -f ~/kruus/vt/env.bash ]; then
	source ~/kruus/vt/env.bash --ve
fi
echo 'Not using:
   CCAS="nas" \
   AS="ncc" \
   CCASFLAGS="" \
   '

./configure \
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
   AS="ncc" \
   LD="nld" \
   CXXCPP="ncc -E" \
   LDFLAGS="-Wl,-z,max-page-size=0x200000" \
   READELF="nreadelf" \
   RANLIB="nranlib" \
   STRIP="nstrip"

# nstrip not determined correctly?

