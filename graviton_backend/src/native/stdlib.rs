use super::ast::semantic::SemanticStdLib;

macro_rules! stdlib_fn {
    ($lib:ident, $name:ident, $sig:expr) => {
        $lib.add_fn(String::from(stringify!($name)), $sig);
    };
}

pub fn get_stdlib_signatures() -> SemanticStdLib {
    let mut stdlib = SemanticStdLib::new();
    stdlib_fn!(stdlib, print, make_fn_sig! { (String) -> Nil });
    stdlib_fn!(stdlib, println, make_fn_sig! { (String) -> Nil });
    stdlib_fn!(stdlib, read_num, make_fn_sig! { () -> I32 });
    stdlib_fn!(stdlib, printn, make_fn_sig! { (I32) -> Nil });
    stdlib_fn!(stdlib, printb, make_fn_sig! { (Bool) -> Nil });
    stdlib_fn!(stdlib, printnln, make_fn_sig! { (I32) -> Nil });
    stdlib_fn!(stdlib, printbln, make_fn_sig! { (Bool) -> Nil });
    stdlib_fn!(stdlib, read_num, make_fn_sig! { () -> I32 });
    stdlib_fn!(stdlib, printf32, make_fn_sig! { (F32) -> Nil });
    stdlib_fn!(stdlib, printf32ln, make_fn_sig! { (F32) -> Nil });
    stdlib
}
