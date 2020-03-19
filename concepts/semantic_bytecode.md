
# Semantic bytecode

Things that don't affect control flow can be regular bytecode, like expressions and statements
The analyzer will be

`<d>` => depth
`<s>` => signature index
`<c>` => constant index
`<n>` => name index

If expression can be a single label, like
```
if <d>
<condition>
ifbody <d>
<expression>
ifelse <d>
<condition>
ifelsebody <d>
<expression>
else <d>
endif <d>
```

While statement
```
while <d>
<condition>
whilebody <d>
<expression>
whileend <d>
```

Function expression
```
fn <d> <s>
param <n>
<expression>
fnend <d>
```

Return statement
```
return <d>
<expression>
endreturn <d>
```

Let statement
```
let <d> <s> <n>
<expression>
letend <d>
```

Let mut statement
```
letmut <d> <s> <n>
<expression>
letmutend <d>
```

Statement
```
<expression>
stmt
```

Fn extern expression
```
exernfn <s>
param <n>
```

Struct expression
```
struct <s>
```

Import expression
```
import <s> <n>
```

Block expression
```
block <d>
<statements>
blockexpression <d>
<expression>
blockend <d>
```
