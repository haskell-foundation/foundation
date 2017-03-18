#include "foundation_system.h"

#if defined(FOUNDATION_SYSTEM_WINDOWS)
# error "todo"
#else
# include "netdb.h"
#endif

int get_h_errno(void)
{
  return h_errno;
}
