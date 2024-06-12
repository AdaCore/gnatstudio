#include <unistd.h>
#include <stdio.h>

int main() {
    /* detect whether the standard output for this process
       is a tty */

    if (isatty(STDOUT_FILENO)) {
        printf("stdout is a tty\n");
    } else {
        printf("stdout is not a tty\n");
    }
}
