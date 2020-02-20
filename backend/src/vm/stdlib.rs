use super::ast::semantic::SemanticStdLib;
use super::{Bytecode, StackVm, Value, VmError};
use std::io::{BufRead, Write};

pub fn read_num(vm: &mut StackVm, bc: &Bytecode) -> Result<(), VmError> {
    let mut input = String::new();
    if let Err(e) = std::io::stdin().lock().read_line(&mut input) {
        return Err(vm.make_error(bc, e.to_string()));
    }

    let value = match input.trim().parse::<f64>() {
        Ok(n) => Value::Number(n),
        Err(e) => return Err(vm.make_error(bc, e.to_string())),
    };

    vm.stack.push(value);

    Ok(())
}

pub fn read_bool(vm: &mut StackVm, bc: &Bytecode) -> Result<(), VmError> {
    let mut input = String::new();
    if let Err(e) = std::io::stdin().lock().read_line(&mut input) {
        return Err(vm.make_error(bc, e.to_string()));
    }

    let value = match input.trim() {
        "true" => Value::Bool(true),
        "false" => Value::Bool(false),
        _ => return Err(vm.make_error(bc, "Not true or false".to_string())),
    };

    vm.stack.push(value);

    Ok(())
}

pub fn read_line(vm: &mut StackVm, bc: &Bytecode) -> Result<(), VmError> {
    let mut input = String::new();
    if let Err(e) = std::io::stdin().lock().read_line(&mut input) {
        return Err(vm.make_error(bc, e.to_string()));
    }

    let value = Value::Object(Box::new(input.trim().to_string()));

    vm.stack.push(value);

    Ok(())
}

pub fn println(vm: &mut StackVm, _bc: &Bytecode) -> Result<(), VmError> {
    match vm.stack.pop() {
        Some(val) => match val {
            Value::Nil => println!("nil"),
            Value::Bool(b) => println!("{}", b),
            Value::Number(n) => println!("{}", n),
            Value::Object(o) => println!("{}", o),
        },
        None => println!("No value in stack"),
    }
    Ok(())
}

pub fn print(vm: &mut StackVm, _bc: &Bytecode) -> Result<(), VmError> {
    match vm.stack.pop() {
        Some(val) => match val {
            Value::Nil => print!("nil"),
            Value::Bool(b) => print!("{}", b),
            Value::Number(n) => print!("{}", n),
            Value::Object(o) => print!("{}", o),
        },
        None => println!("No value in stack"),
    }
    std::io::stdout().flush().unwrap();
    Ok(())
}

pub fn vmto_number(vm: &mut StackVm, bc: &Bytecode) -> Result<(), VmError> {
    match vm.stack.pop() {
        Some(val) => match val {
            Value::Nil => vm.stack.push(Value::Number(0.0)),
            Value::Bool(b) => vm.stack.push(Value::Number(if b { 1.0 } else { 0.0 })),
            Value::Number(n) => vm.stack.push(Value::Number(n)),
            Value::Object(o) => match o.downcast::<String>() {
                Ok(s) => match s.trim().parse::<f64>() {
                    Ok(n) => vm.stack.push(Value::Number(n)),
                    Err(e) => return Err(vm.make_error(bc, e.to_string())),
                },
                Err(_) => {
                    return Err(
                        vm.make_error(bc, "Can only parse string object to numbers".to_string())
                    )
                }
            },
        },
        None => return Err(vm.make_error(bc, "No value in stack to convert".to_string())),
    }
    Ok(())
}

pub fn vmto_bool(vm: &mut StackVm, bc: &Bytecode) -> Result<(), VmError> {
    match vm.stack.pop() {
        Some(val) => match val {
            Value::Nil => vm.stack.push(Value::Bool(false)),
            Value::Bool(b) => vm.stack.push(Value::Bool(b)),
            Value::Number(n) => vm.stack.push(Value::Bool(n > 0.0)),
            Value::Object(_) => vm.stack.push(Value::Bool(false)),
        },
        None => return Err(vm.make_error(bc, "No value in stack to convert".to_string())),
    }
    Ok(())
}

pub fn vmto_string(vm: &mut StackVm, bc: &Bytecode) -> Result<(), VmError> {
    match vm.stack.pop() {
        Some(val) => match val {
            Value::Nil => vm.stack.push(Value::Object(Box::new("Nil".to_string()))),
            Value::Bool(b) => vm.stack.push(Value::Object(Box::new(format!("{}", b)))),
            Value::Number(n) => vm.stack.push(Value::Object(Box::new(format!("{}", n)))),
            Value::Object(o) => vm.stack.push(Value::Object(Box::new(format!("{:?}", o)))),
        },
        None => return Err(vm.make_error(bc, "No value in stack to convert".to_string())),
    }
    Ok(())
}

pub fn add_stdlib(vm: &mut StackVm) {
    vm.add_fn(&"read_line".to_string(), 0, read_line);
    vm.add_fn(&"read_num".to_string(), 0, read_num);
    vm.add_fn(&"read_bool".to_string(), 0, read_bool);
    vm.add_fn(&"println".to_string(), 1, println);
    vm.add_fn(&"print".to_string(), 1, print);
    vm.add_fn(&"nums".to_string(), 1, vmto_number);
    vm.add_fn(&"numb".to_string(), 1, vmto_number);
    vm.add_fn(&"booln".to_string(), 1, vmto_bool);
    vm.add_fn(&"strn".to_string(), 1, vmto_string);
    vm.add_fn(&"strb".to_string(), 1, vmto_string);
}

pub fn get_stdlib_signatures() -> SemanticStdLib {
    let mut stdlib = SemanticStdLib::default();
    stdlib.add_fn(String::from("read_line"), make_fn_sig! { () -> String });
    stdlib.add_fn(String::from("read_num"), make_fn_sig! { () ->I32 });
    stdlib.add_fn(String::from("read_bool"), make_fn_sig! { () -> Bool });
    stdlib.add_fn(String::from("println"), make_fn_sig! { (String) -> Nil });
    stdlib.add_fn(String::from("print"), make_fn_sig! { (String) -> Nil });
    stdlib.add_fn(String::from("nums"), make_fn_sig! { (String) -> I32 });
    stdlib.add_fn(String::from("numb"), make_fn_sig! { (Bool) -> I32 });
    stdlib.add_fn(String::from("booln"), make_fn_sig! { (I32) -> Bool });
    stdlib.add_fn(String::from("strn"), make_fn_sig! { (I32) -> String });
    stdlib.add_fn(String::from("strb"), make_fn_sig! { (Bool) -> String });
    stdlib
}
