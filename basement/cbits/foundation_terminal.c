#include "../../cbits/foundation_system.h"

#ifdef FOUNDATION_SYSTEM_WINDOWS
#include <windows.h>

#elif defined FOUNDATION_SYSTEM_UNIX
#include <sys/ioctl.h>

#endif

#include <stdlib.h>
#include <stdio.h>

int get_terminal_width (void) {
#ifdef FOUNDATION_SYSTEM_WINDOWS
    HANDLE hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
    if (hStdout != INVALID_HANDLE_VALUE) {
        CONSOLE_SCREEN_BUFFER_INFO max;
        if (0 != GetConsoleScreenBufferInfo(hStdout, &max)) {
            return max.srWindow.Right - max.srWindow.Left + 1;
        }
    }
#elif defined FOUNDATION_SYSTEM_UNIX
    struct winsize max;
    if (-1 != ioctl(0, TIOCGWINSZ , &max)) {
        return max.ws_col;
    }
#endif
    FILE* fp = popen ("tput cols", "r");
    if (fp != NULL) {
        int result;
        if (1 == fscanf (fp, " %d ", &result)) {
            return result;
        }
    }
    pclose (fp);
    return 80;
};

int get_terminal_height (void) {
#ifdef FOUNDATION_SYSTEM_WINDOWS
    HANDLE hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
    if (hStdout != INVALID_HANDLE_VALUE) {
        CONSOLE_SCREEN_BUFFER_INFO max;
        if (0 != GetConsoleScreenBufferInfo(hStdout, &max)) {
            return max.srWindow.Bottom - max.srWindow.Top + 1;
        }
    }
#elif defined FOUNDATION_SYSTEM_UNIX
    struct winsize max;
    if (-1 != ioctl(0, TIOCGWINSZ , &max)) {
        return max.ws_row;
    }
#endif
    FILE* fp = popen ("tput lines", "r");
    if (fp != NULL) {
        int result;
        if (1 == fscanf (fp, " %d ", &result)) {
            return result;
        }
    }
    pclose (fp);
    return 25;
}
