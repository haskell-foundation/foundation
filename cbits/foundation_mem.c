#include <string.h>

int _foundation_memcmp(const void *s1, size_t off1, const void *s2, size_t off2, size_t n)
{
	return memcmp(s1 + off1, s2 + off2, n);
}
