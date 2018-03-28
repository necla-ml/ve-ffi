prefix ?= ../../install
ifneq ("$(wildcard $(VE_VE)/lib/libffi.a)","")
prefix=$(VE_VE)
else ifneq ("$(wildcard $(VE_ROOT)/ve/lib/libffi.a)","")
prefix=$(VE_ROOT)/ve
else ifneq ("$(wildcard ../../ve-unknown-linux-gnu/.libs/libffi.a)","")
# try to support make-before-install	
prefix=$(realpath ../../ve-unknown-linux-gnu/.libs)
endif

VE_EXEC ?=
CC ?= gcc
CFLAGS = -O2 -Wall
includedir = $(prefix)/include
libdir = $(prefix)/lib
CPPFLAGS = -I$(includedir)
LDFLAGS = -L$(libdir) -Wl,-rpath,$(libdir)

#CFLAGS += -fPIC -DPIC -g
#LDFLAGS += -shared

all: settings check-call check-callback

settings:
	echo "CC        : $(CC)"
	echo "VE_EXEC   : $(VE_EXEC)"
	echo "prefix    : $(prefix)"
	ls -l $(prefix)/lib

test-call: test-call.c testcases.c
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) -o test-call test-call.c -lffi

test-callback: test-callback.c testcases.c
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) -o test-callback test-callback.c -lffi

check-call: test-call
	$(VE_EXEC) ./test-call 2>&1 | tee test-call.out
	LC_ALL=C uniq -u < test-call.out > failed-call
	test '!' -s failed-call

check-callback: test-callback
	$(VE_EXEC) ./test-callback 2>&1 | tee test-callback.out
	LC_ALL=C uniq -u < test-callback.out > failed-callback
	test '!' -s failed-callback

clean:
	rm -f test-call test-callback test-call.out test-callback.out failed-call failed-callback
