
#include <stdlib.h>
#include <stddef.h>
#include "foundation_system.h"
#include <errno.h>

#if defined(FOUNDATION_SYSTEM_LINUX)
#include <sys/syscall.h>
#include <linux/random.h>
#include <unistd.h>
#define _GNU_SOURCE
#endif

#include <stdio.h>

#if defined(FOUNDATION_SYSTEM_LINUX) && defined(SYS_getrandom)
int foundation_sysrandom_linux(void *buf, size_t length)
{
	unsigned int flags = 1; /* RANDOM=0x2, NONBLOCK=0x1 */
	size_t i = 0;

	/* special case to detect availability */
	if (length == 0) {
		int r = syscall(SYS_getrandom, buf, 0, flags);
		return (r == -1) ? -1 : 0;
	}

	while (i < length) {
		int r = syscall(SYS_getrandom, buf + i, length - i, flags);
		if (r <= 0) {
			if (errno != -EAGAIN)
				return -errno;
		}
		if (r > 0)
			i += r;
	}
	return i;
}
#else
int foundation_sysrandom_linux(void *buf, size_t length) { return -ENODEV; }
#endif
