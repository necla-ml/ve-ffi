#include <alloca.h>
#include <string.h>
#include <iostream>
using namespace std;
#define GETFP(var) asm volatile( "or %0,0,%%fp" :"=r"(var) ::"memory" );
#define GETSP(var) asm volatile( "or %0,0,%%sp" :"=r"(var) ::"memory" );
//int fp, sp;
void* fp;
void* sp;
int foo(int i, int j){
	GETFP(fp); GETSP(sp);
	cout<<" enter foo: fp="<<fp<<"  sp="<<sp<<endl;
	int ret;
	switch(i){
		case(0): { ret = j*j+5; break; }
		case(12345): {ret = j+1; break;}
		default: {ret = j*j-2*j+1; break;}
	}
	GETFP(fp); GETSP(sp);
	cout<<" exit  foo: fp="<<fp<<"  sp="<<sp<<endl;
	return ret;
}
char *buf;
char *buf2;
void bar() {
	GETFP(fp); GETSP(sp); cout<<" enter bar: fp="<<fp<<"  sp="<<sp<<endl;
	buf = (char*)alloca(16); buf[0]='\0';
	GETFP(fp); GETSP(sp); cout<<" exit  bar: fp="<<fp<<"  sp="<<sp<<"  buf="<<(void*)buf<<endl;
}
void bar00() {
	GETFP(fp); GETSP(sp);
	buf = (char*)alloca(16); buf[0]='\0';
	GETFP(fp); GETSP(sp);
}
void baz(int i, int j) {
	if(i==j){
		GETFP(fp); GETSP(sp); cout<<" enter baz: fp="<<fp<<"  sp="<<sp<<endl;
		buf = (char*)alloca(16); buf[0]='\0';
		GETFP(fp); GETSP(sp); cout<<" exit  baz: fp="<<fp<<"  sp="<<sp<<"  buf="<<(void*)buf<<endl;
	}
}
int main(int,char**){
	GETFP(fp); GETSP(sp);
	cout<<" main  : fp="<<fp<<"  sp="<<sp<<endl;
	static char const* msg = "AbcdEfghIjklMno\0";
	buf = (char*)alloca(16);
	GETFP(fp); GETSP(sp);
	cout<<" alloca: fp="<<fp<<"  sp="<<sp<<"  buf="<<(void*)buf<<endl;
	memcpy(buf,msg,16);
	cout<<" alloca-->buf="<<buf<<endl;
	int i = strlen(msg);
	int j = i/2;
	int k=foo(i,j);
	cout<<" "<<k<<" = foo("<<i<<","<<j<<")"<<endl;
	cout<<" alloca-->buf="<<buf<<endl<<endl;

	buf2 = (char*)alloca(16);
	GETFP(fp); GETSP(sp);
	cout<<" 2alloca: fp="<<fp<<"  sp="<<sp<<"  buf2="<<(void*)buf2<<endl;
	memcpy(buf2,msg,16);
	cout<<" 2alloca-->buf2="<<buf<<endl;
	k=foo(i+1,i+j+k);
	cout<<" "<<k<<" = 2foo("<<i+1<<","<<i+j+k<<")"<<endl;
	GETFP(fp); GETSP(sp);
	cout<<" 2alloca: fp="<<fp<<"  sp="<<sp<<"  buf2="<<(void*)buf2<<endl;

	cout<<" alloca-->buf="<<buf<<endl;
	cout<<" alloca-->buf2="<<buf2<<endl;

	bar();
	baz(5,5);
	cout<<"Goodbye"<<endl;
	return 0;
}
#if 0
  27:a.cpp         **** 	buf = (char*)alloca(16); buf[0]='\0';
 1603              		.loc	1 27 0
 1604 0848 00000000 		or	%s63,16,(0)1
 1604      00103F45 
 1605 0850 00000000 		or	%s0,0,%s63
 1605      BF000045 
 1606 0858 00000000 		lea	%s12,__grow_stack@LO
 1606      00000C06 
 1607 0860 00000000 		and	%s12,%s12,(32)0
 1607      608C0C44 
 1608 0868 00000000 		lea.sl	%s12,__grow_stack@HI(,%s12)
 1608      8C008C06 
 1609 0870 00000000 		bsic	%lr,(,%s12)		# __grow_stack
 1609      8C000A08 
 1610              	.L_2.8:
 1611 0878 C0000000 		lea	%s63,192
 1611      00003F06 
 1612 0880 00000000 		adds.l	%s62,%sp,%s63
<__grow_stack>:
   0:   00 00 00 00     or      %s63,0,%s0
   4:   80 00 3f 45
   8:   10 00 00 00     brle.l  0,%s0,0x10
   c:   80 00 06 18
  10:   00 00 00 00     subs.l  %s0,0,%s0
  14:   80 00 00 5b
  18:   00 00 00 00     addu.l  %s0,%s0,(60)0
  1c:   7c 80 00 48
  20:   00 00 00 00     srl     %s0,%s0,4
  24:   80 04 00 75
  28:   00 00 00 00     sll     %s0,%s0,4
  2c:   80 04 00 65
  30:   10 00 00 00     brle.l  0,%s63,0x10
  34:   bf 00 06 18
  38:   00 00 00 00     subs.l  %s0,0,%s0
  3c:   80 00 00 5b
  40:   00 00 00 00     subs.l  %s11,%s11,%s0
  44:   80 8b 0b 5b
  48:   38 00 00 00     brge.l  %s11,%s8,0x38
  4c:   88 8b 05 18
  50:   18 00 00 00     ld      %s63,0x18(0,%s14)
  54:   8e 00 3f 01
  58:   3b 01 00 00     lea     %s0,0x13b(0,0)
  5c:   00 00 00 06
  60:   00 00 00 00     shm.l   %s0,0x0(%s63)
  64:   bf 03 00 31
  68:   08 00 00 00     shm.l   %s8,0x8(%s63)
  6c:   bf 03 08 31
  70:   10 00 00 00     shm.l   %s11,0x10(%s63)
  74:   bf 03 0b 31
  78:   00 00 00 00     monc    0,0,0
  7c:   00 00 00 3f
  80:   00 00 00 00     b.l     0x0(,%s10)
  84:   8a 00 0f 19
#endif
