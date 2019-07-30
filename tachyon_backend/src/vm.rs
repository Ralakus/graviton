
use super::ast;

use serde::{Serialize, Deserialize};

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum Value {
    Nil,
    Number(f64),
    Bool(bool)
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
pub enum ByteOp {
    Load(u16),

    True,
    False,
    Nil,

    Add,
    Sub,
    Mul,
    Div,

    Not,

    Equal,
    Greater,
    Less,

    Negate,

    Pop,
    Return
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Bytecode {
    constants: Vec<Value>,
    ops: Vec<ByteOp>
}

impl Bytecode {
    pub fn new(ast: ast::Ast) -> Result<Bytecode, String> {
        let mut bc = Bytecode {
            constants: Vec::new(),
            ops: Vec::new()
        };
        ast_to_bytecode(&mut bc, ast)?;
        bc.ops.push(ByteOp::Return);
        Ok(bc)
    }
}

fn ast_to_bytecode(bc: &mut Bytecode, ast: ast::Ast) -> Result<(), String> {
    match ast {
        ast::Ast::Identifier(_) => {},
        ast::Ast::Number(n) => {
            bc.constants.push(Value::Number(n));
            bc.ops.push(ByteOp::Load((bc.constants.len() - 1) as u16));
        },
        ast::Ast::String(_) => {},
        ast::Ast::Bool(b) => {
            bc.ops.push(if b { ByteOp::True } else { ByteOp::False });
        }
        ast::Ast::Statement(expr) => {
            ast_to_bytecode(bc, *expr)?;
            bc.ops.push(ByteOp::Pop);
        }
        ast::Ast::Binary(op, l, r) => {
            ast_to_bytecode(bc, *l)?;
            ast_to_bytecode(bc, *r)?;
            match op {
                ast::BinaryOperation::Add => bc.ops.push(ByteOp::Add),
                ast::BinaryOperation::Subtract => bc.ops.push(ByteOp::Sub),
                ast::BinaryOperation::Multiply => bc.ops.push(ByteOp::Mul),
                ast::BinaryOperation::Divide => bc.ops.push(ByteOp::Div),

                ast::BinaryOperation::Less => bc.ops.push(ByteOp::Less),
                ast::BinaryOperation::LessEqual => {
                    bc.ops.push(ByteOp::Greater);
                    bc.ops.push(ByteOp::Not);
                },
                ast::BinaryOperation::Greater => bc.ops.push(ByteOp::Greater),
                ast::BinaryOperation::GreaterEqual => {
                    bc.ops.push(ByteOp::Less);
                    bc.ops.push(ByteOp::Not);
                },
                ast::BinaryOperation::Equals => bc.ops.push(ByteOp::Equal),
                ast::BinaryOperation::Assign => {},
            };
        },
        ast::Ast::Unary(op, expr) => {
            ast_to_bytecode(bc, *expr)?;
            match op {
                ast::UnaryOperation::Negate => bc.ops.push(ByteOp::Negate),
                ast::UnaryOperation::Not => bc.ops.push(ByteOp::Not)
            }
        },

        ast::Ast::Block(exprs) => {
            for e in exprs {
                ast_to_bytecode(bc, e)?;
            }
        }
        other => { return Err(format!("Non implemented AST node {:?}", other)); }
    };
    Ok(())
}

pub struct StackVm {
    ip_idx: usize,
    stack: Vec<Value>,
}

impl StackVm {
    pub fn new() -> StackVm {
        StackVm {
            ip_idx: 0,
            stack: Vec::new()
        }
    }

    fn stack_peek(&self, distance: usize) -> Value {
        self.stack[self.stack.len() - 1 - distance]
    }

    pub fn run(&mut self, bc: Bytecode) -> Result<Value, String> {
        'run: loop{
            println!("Stack: {:?}\nOp: {:?}", self.stack, bc.ops.get(self.ip_idx));
            match bc.ops.get(self.ip_idx) {
                Some(ByteOp::Load(n)) => {
                    self.stack.push(bc.constants[*n as usize]);
                },
                Some(ByteOp::True) => {
                    self.stack.push(Value::Bool(true));
                },
                Some(ByteOp::False) => {
                    self.stack.push(Value::Bool(false));
                },
                Some(ByteOp::Nil) => {
                    self.stack.push(Value::Nil);
                },
                Some(ByteOp::Add) => {
                    match self.stack_peek(0) {
                        Value::Nil => { return Err("Binary add value cannot be nil".to_string()); }
                        Value::Bool(_) => { return Err("Binary add value cannot be bool".to_string()); }
                        Value::Number(_) => {
                            match self.stack_peek(1) {
                                Value::Nil => { return Err("Binary add value cannot be nil".to_string()); }
                                Value::Bool(_) => { return Err("Binary add value cannot be bool".to_string()); }
                                Value::Number(_) => {
                                    if let Some(Value::Number(b)) = self.stack.pop() {
                                        if let Some(Value::Number(a)) = self.stack.pop() {
                                            self.stack.push(Value::Number(a + b));
                                        } else {
                                            return Err("Failed to pop binary add a".to_string());
                                        }
                                    } else {
                                        return Err("Failed to pop binary add b".to_string());
                                    }

                                } 
                            }
                        }
                    }
                },
                Some(ByteOp::Sub) => {
                    match self.stack_peek(0) {
                        Value::Nil => { return Err("Binary sub value cannot be nil".to_string()); }
                        Value::Bool(_) => { return Err("Binary sub value cannot be bool".to_string()); }
                        Value::Number(_) => {
                            match self.stack_peek(1) {
                                Value::Nil => { return Err("Binary sub value cannot be nil".to_string()); }
                                Value::Bool(_) => { return Err("Binary sub value cannot be bool".to_string()); }
                                Value::Number(_) => {
                                    if let Some(Value::Number(b)) = self.stack.pop() {
                                        if let Some(Value::Number(a)) = self.stack.pop() {
                                            self.stack.push(Value::Number(a - b));
                                        } else {
                                            return Err("Failed to pop binary sub a".to_string());
                                        }
                                    } else {
                                        return Err("Failed to pop binary sub b".to_string());
                                    }

                                } 
                            }
                        }
                    }
                },
                Some(ByteOp::Mul) => {
                    match self.stack_peek(0) {
                        Value::Nil => { return Err("Binary mul value cannot be nil".to_string()); }
                        Value::Bool(_) => { return Err("Binary mul value cannot be bool".to_string()); }
                        Value::Number(_) => {
                            match self.stack_peek(1) {
                                Value::Nil => { return Err("Binary mul value cannot be nil".to_string()); }
                                Value::Bool(_) => { return Err("Binary mul value cannot be bool".to_string()); }
                                Value::Number(_) => {
                                    if let Some(Value::Number(b)) = self.stack.pop() {
                                        if let Some(Value::Number(a)) = self.stack.pop() {
                                            self.stack.push(Value::Number(a * b));
                                        } else {
                                            return Err("Failed to pop binary mul a".to_string());
                                        }
                                    } else {
                                        return Err("Failed to pop binary mul b".to_string());
                                    }

                                } 
                            }
                        }
                    }
                },
                Some(ByteOp::Div) => {
                    match self.stack_peek(0) {
                        Value::Nil => { return Err("Binary div value cannot be nil".to_string()); }
                        Value::Bool(_) => { return Err("Binary div value cannot be bool".to_string()); }
                        Value::Number(_) => {
                            match self.stack_peek(1) {
                                Value::Nil => { return Err("Binary div value cannot be nil".to_string()); }
                                Value::Bool(_) => { return Err("Binary div value cannot be bool".to_string()); }
                                Value::Number(_) => {
                                    if let Some(Value::Number(b)) = self.stack.pop() {
                                        if let Some(Value::Number(a)) = self.stack.pop() {
                                            self.stack.push(Value::Number(a / b));
                                        } else {
                                            return Err("Failed to pop binary div a".to_string());
                                        }
                                    } else {
                                        return Err("Failed to pop binary div b".to_string());
                                    }

                                } 
                            }
                        }
                    }
                },
                Some(ByteOp::Not) => {
                    match self.stack_peek(0) {
                        Value::Nil => { return Err("Unary not value cannot be nil".to_string()); }
                        Value::Bool(_) => {
                            if let Some(Value::Bool(b)) = self.stack.pop() {
                                self.stack.push(Value::Bool(!b));
                            } else {
                                return Err("Failed to pop unary not value".to_string());
                            }
                        }
                        Value::Number(_) => { return Err("Unary value cannot be number".to_string()); }
                    }
                },
                Some(ByteOp::Equal) => {
                    match self.stack_peek(0) {
                        Value::Nil => { return Err("Binary equal value cannot be nil".to_string()); }
                        Value::Bool(_) => { return Err("Binary equal value cannot be bool".to_string()); }
                        Value::Number(_) => {
                            match self.stack_peek(1) {
                                Value::Nil => { return Err("Binary equal value cannot be nil".to_string()); }
                                Value::Bool(_) => { return Err("Binary equal value cannot be bool".to_string()); }
                                Value::Number(_) => {
                                    if let Some(Value::Number(b)) = self.stack.pop() {
                                        if let Some(Value::Number(a)) = self.stack.pop() {
                                            self.stack.push(Value::Bool(a == b));
                                        } else {
                                            return Err("Failed to pop binary equal a".to_string());
                                        }
                                    } else {
                                        return Err("Failed to pop binary equal b".to_string());
                                    }

                                } 
                            }
                        }
                    }
                },
                Some(ByteOp::Greater) => {
                    match self.stack_peek(0) {
                        Value::Nil => { return Err("Binary greater value cannot be nil".to_string()); }
                        Value::Bool(_) => { return Err("Binary greater value cannot be bool".to_string()); }
                        Value::Number(_) => {
                            match self.stack_peek(1) {
                                Value::Nil => { return Err("Binary greater value cannot be nil".to_string()); }
                                Value::Bool(_) => { return Err("Binary greater value cannot be bool".to_string()); }
                                Value::Number(_) => {
                                    if let Some(Value::Number(b)) = self.stack.pop() {
                                        if let Some(Value::Number(a)) = self.stack.pop() {
                                            self.stack.push(Value::Bool(a > b));
                                        } else {
                                            return Err("Failed to pop binary greater a".to_string());
                                        }
                                    } else {
                                        return Err("Failed to pop binary greater b".to_string());
                                    }

                                } 
                            }
                        }
                    }
                },
                Some(ByteOp::Less) => {
                    match self.stack_peek(0) {
                        Value::Nil => { return Err("Binary less value cannot be nil".to_string()); }
                        Value::Bool(_) => { return Err("Binary less value cannot be bool".to_string()); }
                        Value::Number(_) => {
                            match self.stack_peek(1) {
                                Value::Nil => { return Err("Binary less value cannot be nil".to_string()); }
                                Value::Bool(_) => { return Err("Binary less value cannot be bool".to_string()); }
                                Value::Number(_) => {
                                    if let Some(Value::Number(b)) = self.stack.pop() {
                                        if let Some(Value::Number(a)) = self.stack.pop() {
                                            self.stack.push(Value::Bool(a < b));
                                        } else {
                                            return Err("Failed to pop binary less a".to_string());
                                        }
                                    } else {
                                        return Err("Failed to pop binary less b".to_string());
                                    }

                                } 
                            }
                        }
                    }
                },
                Some(ByteOp::Negate) => {
                    match self.stack_peek(0) {
                        Value::Nil => { return Err("Unary negate value cannot be nil".to_string()); }
                        Value::Bool(_) => { return Err("Unary negate value cannot be bool".to_string()); }
                        Value::Number(_) => {
                            if let Some(Value::Number(n)) = self.stack.pop() {
                                self.stack.push(Value::Number(-n));
                            } else {
                                return Err("Failed to pop unary negate value".to_string());
                            }
                        }
                    }
                },
                Some(ByteOp::Pop) => {
                    self.stack.pop();
                },
                Some(ByteOp::Return) => {
                    if let Some(v) = self.stack.pop() {
                        return Ok(v);
                    } else {
                        return Ok(Value::Nil); // Err("Failed to pop return value from stack".to_string());
                    }
                },
                None => { break 'run; }
            };
            self.ip_idx += 1;
        }
        Ok(Value::Nil)
    }
}