#include <stdint.h>

int32_t is_little_endian(void)
{
  int x = 1;
  char const* ptr = (char const*)&x;
  return *ptr & 0xFF;
}
