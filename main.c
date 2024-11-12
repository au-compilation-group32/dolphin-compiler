/* main.c: C wrapper for assembly programs */
   
#include <stdio.h>
#include <inttypes.h>      /* present in the start of your C file */


int64_t read_integer () {
    int64_t value;
    printf("Please enter an integer: ");
    scanf("%" PRId64 "" , &value);
    return value;
}

void print_integer (int64_t x) {
    printf ("%ld\n", x);
}

extern int64_t main();
