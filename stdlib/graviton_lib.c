#include <stdlib.h>
#include <stdio.h>

#include "graviton_lib.h"

Nil printn(I32 n) {
    printf("%d", n);
    fflush(stdout);
}
Nil printb(Bool b) {
    if(b) {
        printf("true");
    } else {
        printf("false");
    }
    fflush(stdout);
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
    fgets(buffer, 10, stdin);
    return strtol(buffer, (char**)NULL, 10);
}

Nil print(String s) {
    printf("%s", s);
    fflush(stdout);
}

Nil println(String s) {
    print(s);
    printf("\n");
}

Nil printf32(F32 f) {
    printf("%g", f);
    fflush(stdout);
}

Nil printf32ln(F32 f) {
    printf32(f);
    printf("\n");
}