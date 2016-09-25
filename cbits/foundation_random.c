
#include <stdlib.h>
#include <stddef.h>
#include "foundation_system.h"

#if defined(FOUNDATION_SYSTEM_LINUX) && defined(SYS_getrandom)
#define _GNU_SOURCE

#include <unistd.h>
#include <sys/syscall.h>
#include <linux/random.h>
int foundation_sysrandom_linux(void *buf, size_t length)
{
	unsigned int flags = 1; /* RANDOM=0x2, NONBLOCK=0x1 */
	size_t i = 0;

	if (length == 0)
		return 0;

	while (i < length) {
		int r = syscall(SYS_getrandom, buf + i, length - i, flags);
		if (r <= 0 && r != -EAGAIN)
			return r;
		if (r != -EAGAIN)
			i += r;
	}
	return r;
}
#else
#include <errno.h>
int foundation_sysrandom_linux(void *buf, size_t length)
{
	return -ENODEV;
}
#endif
