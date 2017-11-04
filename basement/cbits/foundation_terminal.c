#include "foundation_system.h"

#ifdef FOUNDATION_SYSTEM_WINDOWS
#include <windows.h>

#else
#include <sys/ioctl.h>

#endif

int get_terminal_width (void) {
#ifdef FOUNDATION_SYSTEM_WINDOWS
    CONSOLE_SCREEN_BUFFER_INFO max;
    GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &max);
    return max.srWindow.Right - max.srWindow.Left + 1;
#elif defined FOUNDATION_SYSTEM_UNIX
    struct winsize max;
    ioctl(0, TIOCGWINSZ , &max);
    return max.ws_col;
#endif
};

int get_terminal_height (void) {
#ifdef FOUNDATION_SYSTEM_WINDOWS
    CONSOLE_SCREEN_BUFFER_INFO max;
    GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &max);
    return max.srWindow.Bottom - max.srWindow.Top + 1;
#elif defined FOUNDATION_SYSTEM_UNIX
    struct winsize max;
    ioctl(0, TIOCGWINSZ , &max);
    return max.ws_row;
#endif
}
