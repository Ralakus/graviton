```

// () -> is a grouping
// *  -> means can repeat 0 or more times
// ?  -> optional
// |  -> or

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

binary -> "+"  expression
        | "-"  expression
        | "*"  expression
        | "/"  expression
        | "<"  expression
        | "<=" expression
        | ">"  expression
        | ">=" expression
        | "==" expression
        | "!=" expression
        | "="  expression
        ;

grouping -> "(" expression ")" ;

struct -> "struct" "{" (IDENTIFIER ":" type ",")* (IDENTIFIER ":" type)? "}" ;

call -> expression "(" (expression ",")* (expression)? ")" ;

field_access -> expression "." IDENTIFIER ;

if -> "if" expression expression ("else" "if" expression expression)* ("else" expression)? ;

loop -> "while" expression expression
      | "loop" expression
    
loop_control -> "break" expression?
              | "continue"

fn -> "(" (IDENTIFIER ":" type ",")* (IDENTIFIER ":" type)? ")" "->" type ;

```