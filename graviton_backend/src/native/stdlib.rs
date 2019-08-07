use super::ast::semantic::SemanticStdLib;

macro_rules! stdlib_fn {
    ($lib:ident, $name:ident, $sig:expr) => {
        $lib.add_fn(String::from(stringify!($name)), $sig);
    };
}

pub fn get_stdlib_signatures() -> SemanticStdLib {
    let mut stdlib = SemanticStdLib::new();
    stdlib_fn!(stdlib, printn, make_fn_sig! { fn(I32): Nil });
    stdlib_fn!(stdlib, printb, make_fn_sig! { fn(Bool): Nil });
    stdlib_fn!(stdlib, printnln, make_fn_sig! { fn(I32): Nil });
    stdlib_fn!(stdlib, printbln, make_fn_sig! { fn(Bool): Nil });
    stdlib_fn!(stdlib, read_num, make_fn_sig! { fn(): I32 });
    stdlib
}
