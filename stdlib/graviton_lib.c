#include <stdlib.h>
#include <stdio.h>

#include "graviton_lib.h"

Nil printn(I32 n) {
    printf("Number: %d", n);
}
Nil printb(Bool b) {
    if(b) {
        printf("Bool: true");
    } else {
        printf("Bool: false");
    }
}
Nil printnln(I32 n) {
    printn(n);
    printf("\n");
}
Nil printbln(Bool b) {
    printb(b);
    printf("\n");
}

I32 read_num(Nil) {
    char buffer[10];
    printf("Enter number: ");
    fflush(stdout);
    fgets(buffer, 10, stdin);
    return strtol(buffer, (char**)NULL, 10);
}