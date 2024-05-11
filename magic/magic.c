#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>

int main (int argc, char** argv) {
    struct stat sb;

    printf("S_IFDIR: 0x%X\n", S_IFDIR);
    printf("st_mode: %u\n", (unsigned)((uintptr_t)&sb.st_mode) - ((uintptr_t)&sb));


    // printf("st_isdir@")
    // st_isdir@

    stat("lib\\std", &sb);
    printf("st_mode= 0x%X\n", sb.st_mode);

}
