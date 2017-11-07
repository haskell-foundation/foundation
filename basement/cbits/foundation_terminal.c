#include "../../cbits/foundation_system.h"

#ifdef FOUNDATION_SYSTEM_WINDOWS
#include <windows.h>
#elif defined FOUNDATION_SYSTEM_UNIX
#include <sys/ioctl.h>
#endif

#include <stdlib.h>
#include <stdio.h>

// because ioctl is a variadic function, and the haskell 2010 report specifies 
// "their use is deprecated in portable code" we need a C wrapper
#ifdef FOUNDATION_SYSTEM_UNIX
int ioctl_winsize (int fd, struct winsize *max) {
    return ioctl (fd, TIOCGWINSZ, max);
};
#endif

int fscanf_int (FILE* file, int* result) {
    return fscanf (file, " %d ", result);
};
