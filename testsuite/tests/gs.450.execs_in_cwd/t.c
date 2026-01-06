/* program that creates a file to indicate it was run */

#include <stdio.h>
#include <string.h>
int main(int argc, char ** argv)
{
    /* create a file called 'argv[0].was_called" */
    char* filename = strcat(argv[0], ".was_called");
    FILE* file = fopen(filename, "w");
    fclose(file);
    return 0;
}