#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

void setup_terminal(struct termios* old_settings)
{
    struct termios tos;
    int result = tcgetattr(STDIN_FILENO, &tos);
    if ( result ) {
        fprintf(stderr, "tcgetattr() failed.\n");
        abort();
    }

    memcpy(old_settings, &tos, sizeof(tos));

    tos.c_lflag &= (~ECHO);
    tos.c_lflag &= (~ICANON);
    tos.c_lflag &= (~ISIG);

    result = tcsetattr(STDIN_FILENO, TCSADRAIN, &tos);
    if ( result ) {
        fprintf(stderr, "tcsetattr() failed.\n");
        abort();
    }
}

void restore_terminal(struct termios* old_settings)
{
    tcsetattr(STDIN_FILENO, TCSADRAIN, old_settings);
}

void get_terminal_size(int* w, int* h)
{
    (*w) = 0;
    (*h) = 0;

    struct winsize ws;
    int result = ioctl(STDIN_FILENO, TIOCGWINSZ, &ws);
    if ( result ) {
        fprintf(stderr, "ioctl() failed.\n");
        abort();
    }

    (*w) = ws.ws_col;
    (*h) = ws.ws_row;
}

size_t sizeof_termios(void)
{
    return sizeof(struct termios);
}

