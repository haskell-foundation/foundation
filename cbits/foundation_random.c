
#include <stdlib.h>
#include <stddef.h>
#include "foundation_system.h"

#ifdef FOUNDATION_SYSTEM_LINUX
#define _GNU_SOURCE

#include <unistd.h>
#include <sys/syscall.h>
#include <linux/random.h>
int foundation_sysrandom_linux(void *buf, size_t length)
{
	unsigned int flags = 1; /* RANDOM=0x2, NONBLOCK=0x1 */
	int r = syscall(SYS_getrandom, buf, length, flags);
	return r;
}
#else
#include <errno.h>
int foundation_sysrandom_linux(void *buf, size_t length)
{
	return ENODEV;
}
#endif
