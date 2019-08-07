#[macro_use]
extern crate graviton_ast as ast;

pub mod native;

#[deprecated(since = "0.5.0", note = "Please use native backend")]
pub mod vm;
