
#include <stdint.h>
#include <stdbool.h>

// Graviton type aliases
typedef void Nil;
typedef bool Bool;

typedef int8_t I8;
typedef int16_t I16;
typedef int32_t I32;
typedef int64_t I64;
typedef uint8_t U8;

typedef uint16_t U16;
typedef uint32_t U32;
typedef uint64_t U64;

/* Stdlib */
Nil printn(I32);
Nil printb(Bool);
Nil printnln(I32);
Nil printbln(Bool);

I32 read_num(Nil);