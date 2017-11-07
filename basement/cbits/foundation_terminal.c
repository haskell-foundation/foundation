#include "../../cbits/foundation_system.h"

#ifdef FOUNDATION_SYSTEM_WINDOWS
#include <windows.h>

#elif defined FOUNDATION_SYSTEM_UNIX
#include <sys/ioctl.h>

#endif
#include <stdio.h>

// because ioctl is a variadic function, and the haskell report specifies 
// "their use is deprecated in portable code" we need a C wrapper. same goes
// for fscanf.
#ifdef FOUNDATION_SYSTEM_UNIX
int ioctl_winsize (int fd, struct winsize *max) {
    return ioctl (fd, TIOCGWINSZ, max);
};
#endif

int fscanf_int (FILE* file, int* result) {
    return fscanf (file, " %d ", result);
};
