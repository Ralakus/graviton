
use super::ast;
use super::types;
use super::Type;
use ast::TypeSignature;
use ast::PrimitiveType;

pub fn type_to_cranelift<T: cranelift_module::Backend>(type_: &Option<TypeSignature>, module: &super::Module<T>) -> Type {
    if let Some(t) = type_ {
        match t {
            TypeSignature::Primitive(p) => {
                match p {
                    PrimitiveType::Nil => types::I32,
                    PrimitiveType::Bool => types::B1,
                    PrimitiveType::I8 => types::I8,
                    PrimitiveType::I16 => types::I16,
                    PrimitiveType::I32 => types::I32,
                    PrimitiveType::I64 => types::I64,
                    PrimitiveType::U8 => types::I8,
                    PrimitiveType::U16 => types::I16,
                    PrimitiveType::U32 => types::I32,
                    PrimitiveType::U64 => types::I64,
                }
            },
            TypeSignature::Function(_sig) => {
                module.target_config().pointer_type()
            },
            TypeSignature::Custom(name) => {
                match &**name {
                    "String" => module.target_config().pointer_type(),
                    _ => types::I32
                }
            } 
        }
    } else {
        type_to_cranelift(&Some(TypeSignature::Primitive(PrimitiveType::Nil)), module)
    }
}