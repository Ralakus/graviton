
use super::Value;
use downcast_rs::*;

#[typetag::serde(tag = "StackVmObject")]
pub trait StackVmObject: StackVmObjectClone + std::fmt::Debug + std::fmt::Display + Downcast {
    fn add(&self, r: Value) -> Result<Value, String>;
}
impl_downcast!(StackVmObject);

pub trait StackVmObjectClone {
    fn clone_box(&self) -> Box<dyn StackVmObject>;
}

impl<T> StackVmObjectClone for T
where
    T: 'static + StackVmObject + Clone,
{
    fn clone_box(&self) -> Box<dyn StackVmObject> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn StackVmObject> {
    fn clone(&self) -> Box<dyn StackVmObject> {
        self.clone_box()
    }
}

#[typetag::serde]
impl StackVmObject for String {
    fn add(&self, r: Value) -> Result<Value, String> {
        let s: String = match r {
            Value::Object(o) => {
                match o.downcast::<String>() {
                    Ok(s) => format!("{}{}", self, s),
                    Err(_) => return Err(format!("Can only add two strings together", ))
                }
            },
            _ => return Err(format!("Can only add two strings together"))
        };
        Ok(Value::Object(Box::new(s)))
    }
}