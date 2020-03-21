# Grammar 
## Format
`()` -> is a grouping  
`*`  -> means can repeat 0 or more times  
`?`  -> optional  
`|`  -> or  

# Module
```
module -> (declaration | statement)* ;
```

# Declarations
```
visibility -> "pub" ;

declaration -> visibility? (module_declaration) ;

module_declaration -> "module" IDENTIFIER ("." IDENTIFIER)* ;
```

# Statements
```
statement -> let
           | expression_statement
           ;

expression_statement -> expression ";" ;

let -> "let" IDENTIFIER (":" type)? ("=" expression)? ;
```

# Expressions
```
expression -> literal
            | identifier
            | block
            | unary
            | binary
            | grouping
            | struct
            | call
            | field_access
            | if
            | loop
            | loop_control
            | fn
            ;

literal -> STRING
         | FLOAT
         | NUMBER
         | "true"
         | "false"
         ;

identifier -> IDENTIFIER ;

block -> "{" statement* expression? "}" ;

unary -> "-" expression
       | "!" expression
       ;

binary -> expression "+"  expression
        | expression "-"  expression
        | expression "*"  expression
        | expression "/"  expression
        | expression "<"  expression
        | expression "<=" expression
        | expression ">"  expression
        | expression ">=" expression
        | expression "==" expression
        | expression "!=" expression
        | expression "="  expression
        ;

grouping -> "(" expression ")" ;

struct -> "struct" "{" (visibility? IDENTIFIER ":" type ",")* (visibility? IDENTIFIER ":" type)? "}" ;

call -> expression "(" (expression ",")* (expression)? ")" ;

field_access -> expression "." IDENTIFIER ;

if -> "if" expression expression ("else" "if" expression expression)* ("else" expression)? ;

loop -> "while" expression expression
      | "loop" expression
      ;

loop_control -> "break" expression?
              | "continue"
              ;

fn -> "(" (IDENTIFIER ":" type ",")* (IDENTIFIER ":" type)? ")" "->" type ;
```

# Types
```
type -> primitive ;

primitive -> "I8"
           | "I16"
           | "I32"
           | "I64"
           | "U8"
           | "U16"
           | "U32"
           | "U64"
           | "Bool"
           | "Str"
           ;
```