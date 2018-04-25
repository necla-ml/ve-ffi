rm -rf build
echo -e "ve-conf-local.sh ---> ve-conf-local.log ..."
. ./ve-conf-local.sh >& ve-conf-local.log
echo " DONE"
LIBFFIDIR=`pwd`
ok=1
echo -e "make VERBOSE=1 -C build install >& bld.log ..."
make VERBOSE=1 -C build install >& bld.log && echo " OK" || { ok=0; echo " FAILED"; }
tail -n4 bld.log
if [ "${ok}" -eq 1 ]; then
	echo "Rebuilding testsuite/libffi.bhaible/ to link with new libffi ..."
	cd testsuite/libffi.bhaible
	make clean
	make >& bld.log || { ok=0; echo "problems in testsuite/libffi.bhaible"; }
  echo "build log is in testsuite/libffi.bhaible/bld.log" 
fi
if [ "${ok}" -eq 1 ]; then
	echo -n "testsuite/libffi.bhaible/test-call --> tst.log ..."
	./test-call >& ../../tst.log || { ok=0; echo "trouble running test-call"; }
	echo " DONE"
fi
if [ "${ok}" -eq 1 ]; then
	cat test-fail >> ../../tst.log
	echo "test-fail appended to tst.log"
fi
cd "${LIBFFIDIR}"
if [ "${ok}" -ne 1 ]; then
  vim ve-conf-local.log bld.log testsuite/libffi.bhaible/bld.log tst.log -o
fi
# vim: sw=2 ts=2 et:
