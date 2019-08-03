
use std::io::{BufRead,Write};
use std::error::Error;
use super::{StackVm, Bytecode, VmError, Value};

pub fn read_num(vm: &mut StackVm, bc: &Bytecode) -> Result<(), VmError> {
    // print!("Input number: "); std::io::stdout().flush().unwrap();

    let mut input = String::new();
    if let Err(e) = std::io::stdin().lock().read_line(&mut input) {
        return Err(vm.make_error(bc, e.description().to_string()));
    }

    let value = match input.trim().parse::<f64>() {
            Ok(n) => Value::Number(n),
            Err(e) => return Err(vm.make_error(bc, e.description().to_string()))
        };

    vm.stack.push(value);

    Ok(())
}

pub fn read_bool(vm: &mut StackVm, bc: &Bytecode) -> Result<(), VmError> {
    // print!("Input bool [true, false]: "); std::io::stdout().flush().unwrap();

    let mut input = String::new();
    if let Err(e) = std::io::stdin().lock().read_line(&mut input) {
        return Err(vm.make_error(bc, e.description().to_string()));
    }

    let value = match input.trim() {
            "true" => Value::Bool(true),
            "false" => Value::Bool(false),
            _ => return Err(vm.make_error(bc, "Not true or false".to_string()))
        };

    vm.stack.push(value);

    Ok(())
}

pub fn println(vm: &mut StackVm, _bc: &Bytecode) -> Result<(), VmError> {
    match vm.stack.pop() {
        Some(val) => {
            match val {
                Value::Nil => println!("nil"),
                Value::Bool(b) => println!("{}", b),
                Value::Number(n) => println!("{}", n),
                Value::Object(o) => println!("{}", o),
            }
        },
        None => println!("No value in stack")
    }
    Ok(())
}

pub fn print(vm: &mut StackVm, _bc: &Bytecode) -> Result<(), VmError> {
    match vm.stack.pop() {
        Some(val) => {
            match val {
                Value::Nil => print!("nil"),
                Value::Bool(b) => print!("{}", b),
                Value::Number(n) => print!("{}", n),
                Value::Object(o) => print!("{}", o),
            }
        },
        None => println!("No value in stack")
    }
    std::io::stdout().flush().unwrap();
    Ok(())
}

pub fn vmto_number(vm: &mut StackVm, bc: &Bytecode) -> Result<(), VmError> {
    match vm.stack.pop() {
        Some(val) => {
            match val {
                Value::Nil => vm.stack.push(Value::Number(0.0)),
                Value::Bool(b) => vm.stack.push(Value::Number(if b { 1.0 } else { 0.0 })),
                Value::Number(n) => vm.stack.push(Value::Number(n)),
                Value::Object(_) => vm.stack.push(Value::Number(0.0)),
            }
        },
        None => return Err(vm.make_error(bc, format!("No value in stack to convert")))
    }
    Ok(())
}

pub fn vmto_bool(vm: &mut StackVm, bc: &Bytecode) -> Result<(), VmError> {
    match vm.stack.pop() {
        Some(val) => {
            match val {
                Value::Nil => vm.stack.push(Value::Bool(false)),
                Value::Bool(b) => vm.stack.push(Value::Bool(b)),
                Value::Number(n) => vm.stack.push(Value::Bool(n > 0.0)),
                Value::Object(_) => vm.stack.push(Value::Bool(false)),
            }
        },
        None => return Err(vm.make_error(bc, format!("No value in stack to convert")))
    }
    Ok(())
}

pub fn vmto_string(vm: &mut StackVm, bc: &Bytecode) -> Result<(), VmError> {
    match vm.stack.pop() {
        Some(val) => {
            match val {
                Value::Nil => vm.stack.push(Value::Object(Box::new(format!("Nil")))),
                Value::Bool(b) => vm.stack.push(Value::Object(Box::new(format!("{}", b)))),
                Value::Number(n) => vm.stack.push(Value::Object(Box::new(format!("{}", n)))),
                Value::Object(o) => vm.stack.push(Value::Object(Box::new(format!("{:?}", o)))),
            }
        },
        None => return Err(vm.make_error(bc, format!("No value in stack to convert")))
    }
    Ok(())
}

pub fn add_stdlib(vm: &mut StackVm) {
    vm.add_fn(&"read_num".to_string(), 0, read_num);
    vm.add_fn(&"read_bool".to_string(), 0, read_bool);
    vm.add_fn(&"println".to_string(), 1, println);
    vm.add_fn(&"print".to_string(), 1, print);
    vm.add_fn(&"num".to_string(), 1, vmto_number);
    vm.add_fn(&"bool".to_string(), 1, vmto_bool);
    vm.add_fn(&"str".to_string(), 1, vmto_string);
}