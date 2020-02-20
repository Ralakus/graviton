
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

typedef float F32;
typedef double F64;

typedef char* String;

/* Stdlib */
I32 read_num(Nil);

Nil printn(I32);
Nil printnln(I32);

Nil printb(Bool);
Nil printbln(Bool);

Nil print(String);
Nil println(String);

Nil printf32(F32);
Nil printf32ln(F32);