/* main.c: C wrapper for assembly programs */
   
#include <stdio.h>
#include <inttypes.h>      /* present in the start of your C file */

extern int64_t dolphin_main();

int main() {
    int64_t result = dolphin_main();
    printf("return %ld\n", result);
    return result;
}

int64_t read_integer () {
    int64_t value;
    printf("Please enter an integer: ");
    scanf("%" PRId64 "" , &value);
    return value;
}

void print_integer (int x) {
    printf ("%d\n", x);
}
