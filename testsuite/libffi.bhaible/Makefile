CLOSURE_SUPPORT?=1
COMPLEX_SUPPORT?=1
# for VE, check-callback can't work (needs closure API)

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

# or, for standalone, could grab HAVE_foo things from environment during compile:
#CPPFLAGS += -I../../@target_triplet@ # for fficonfig, for optional types
HAVE_LONG_DOUBLE?=1
CPPFLAGS+=-DHAVE_LONG_DOUBLE=$(HAVE_LONG_DOUBLE) -DHAVE_COMPLEX=$(COMPLEX_SUPPORT)
VERBOSE_FLAGS:=-DVE_DEBUG_LEVEL=5 -DVE_SYSV_DEBUG=1

# -g triggers a compiler assertion in ncc-1.1.2
#CFLAGS+=-g

#CFLAGS += -fPIC -DPIC -g
#LDFLAGS += -shared
PIC_FLAGS:=-fPIC -DPIC -shared

CLOSURE_TARGETS:=
ifeq ($(CLOSURE_SUPPORT),1)
CLOSURE_TARGETS:=check-callback
endif
all: settings clean check-call $(CLOSURE_TARGETS)

settings:
	echo "CC              : $(CC)"
	echo "VE_EXEC         : $(VE_EXEC)"
	echo "prefix          : $(prefix)"
	echo "CLOSURE_TARGETS : $(CLOSURE_TARGETS)"
	ls -l $(prefix)/lib

test-call test-call-v test-call-pic test-call-pic-v: test-call.c testcases.c
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) -o test-call test-call.c -lffi
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) $(PIC_FLAGS) -o test-call-pic test-call.c -lffi
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) $(VERBOSE_FLAGS) -o test-call-v test-call.c -lffi
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) $(PIC_FLAGS) $(VERBOSE_FLAGS) -o test-call-pic-v test-call.c -lffi

test-callback: test-callback.c testcases.c
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) -o test-callback test-callback.c -lffi

check-call: test-call
	$(VE_EXEC) ./test-call 2>&1 | tee test-call.out
	LC_ALL=C uniq -u < test-call.out > failed-call
	test '!' -s failed-call

check-call-pic: test-call-pic
	$(VE_EXEC) $< 2>&1 | tee $<.out
	LC_ALL=C uniq -u < $<.out > failed-call-pic
	test '!' -s failed-call-pic

check-callback: test-callback
	$(VE_EXEC) ./test-callback 2>&1 | tee test-callback.out
	LC_ALL=C uniq -u < test-callback.out > failed-callback
	test '!' -s failed-callback

clean:
	rm -f test-call test-callback test-call.out test-callback.out failed-call failed-callback
