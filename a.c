
#include <stdint.h>
#include <stdio.h>

int main(int argc,char**argv)
{
	union { uint64_t u64; float f[2]; double d; } val;
	val.u64 = 0x60ffffffd110;
	printf("val = union { u64:%x=%lu; f[2]=%f,%f; d=%f\n",
			(long unsigned)val.u64, (long unsigned)val.u64,
			val.f[0], val.f[1], val.d);
	val.u64=0UL;
	val.f[0] = 1.1;
	printf("val = union { u64:%x=%lu; f[2]=%f,%f; d=%f\n",
			(long unsigned)val.u64, (long unsigned)val.u64,
			val.f[0], val.f[1], val.d);
	val.u64=0UL;
	val.f[1] = 1.1;
	printf("val = union { u64:%x=%lu; f[2]=%f,%f; d=%f\n",
			(long unsigned)val.u64, (long unsigned)val.u64,
			val.f[0], val.f[1], val.d);
	val.u64=0UL;
	val.d = 1.1;
	printf("val = union { u64:%x=%lu; f[2]=%f,%f; d=%f\n",
			(long unsigned)val.u64, (long unsigned)val.u64,
			val.f[0], val.f[1], val.d);
}
	
