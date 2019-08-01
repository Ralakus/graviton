
use super::ast;

use serde::{Serialize, Deserialize};
use std::collections::hash_map::*;
use std::io::{BufRead,Write};
use std::error::Error;

mod object;

#[derive(Clone, Debug)]
pub struct VmError {
    pub msg: String,

    #[cfg(feature = "node_code_pos")]
    pub pos: ast::Position
}

impl VmError {
    #[cfg(feature = "node_code_pos")]
    fn new(msg: String, ast: &ast::AstNode) -> VmError {
        VmError {
            msg,
            pos: ast.pos
        }
    }

    #[cfg(not(feature = "node_code_pos"))]
    fn new(msg: String, _ast: &ast::AstNode) -> VmError {
        VmError {
            msg,
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Value {
    Nil,
    Number(f64),
    Bool(bool),
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

    ScopeOpen,
    ScopeClose,

    NativeFnCall(u16, u8),

    DefVar(u16),
    DefMutVar(u16),
    SetVar(u16),
    GetVar(u16),

    Jump(i16),
    JumpFalse(i16),
    JumpTrue(i16),

    Pop,
    Return
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Bytecode {
    constants: Vec<Value>,
    ops: Vec<ByteOp>,

    #[cfg(feature = "node_code_pos")]
    positions: Vec<ast::Position>,

    #[cfg(feature = "store_names")]
    names: HashMap<u16, String>
}

impl Bytecode {
    pub fn new(ast: ast::AstNode) -> Result<Bytecode, VmError> {
        let mut bc = Bytecode {
            constants: Vec::new(),
            ops: Vec::new(),

            #[cfg(feature = "node_code_pos")]
            positions: Vec::new(),

            #[cfg(feature = "store_names")]
            names: HashMap::new()
        };
        ast_to_bytecode(&mut bc, &ast)?;
        Ok(bc)
    }

    #[cfg(feature = "node_code_pos")]
    fn emit(&mut self, ast: &ast::AstNode, op: ByteOp) {
        self.ops.push(op);
        self.positions.push(ast.pos);
    }

    #[cfg(not(feature = "node_code_pos"))]
    fn emit(&mut self, _sdy: &ast::AstNode, op: ByteOp) {
        self.ops.push(op);
    }
}

fn ast_to_bytecode(bc: &mut Bytecode, ast: &ast::AstNode) -> Result<(), VmError> {
    match &ast.node {
        ast::Ast::Identifier(ident) => {
            let hash = crc16::State::<crc16::ARC>::calculate(ident.as_bytes());

            #[cfg(feature = "store_names")]
            bc.names.insert(hash, ident.clone());

            bc.emit(&ast, ByteOp::GetVar(hash));
        },
        ast::Ast::Number(n) => {
            let mut idx: usize = 0;
            for val in &bc.constants {
                if let Value::Number(num) = val {
                    if *n == *num {
                        bc.emit(&ast, ByteOp::Load(idx as u16));
                        return Ok(());
                    }
                }
                idx += 1;
            }
            bc.constants.push(Value::Number(*n));
            bc.emit(&ast, ByteOp::Load((bc.constants.len() - 1) as u16));
        },
        // ast::Ast::String(_) => {},
        ast::Ast::Bool(b) => {
            bc.emit(&ast, if *b { ByteOp::True } else { ByteOp::False });
        },
        ast::Ast::Statement(expr) => {
            ast_to_bytecode(bc, &*expr)?;
            bc.emit(&ast, ByteOp::Pop);
        },
        ast::Ast::Binary(op, l, r) => {
            if let ast::BinaryOperation::Assign = op {
                if let ast::Ast::Identifier(ident) = &l.node {
                    ast_to_bytecode(bc, &*r)?;
                    let hash = crc16::State::<crc16::ARC>::calculate(ident.as_bytes());

                    #[cfg(feature = "store_names")]
                    bc.names.insert(hash, ident.clone());

                    bc.emit(&ast, ByteOp::SetVar(hash));
                } else {
                    return Err(VmError::new("Must assign to variable".to_string(), &ast));
                }
            } else {
                ast_to_bytecode(bc, &*l)?;
                ast_to_bytecode(bc, &*r)?;
                match op {
                    ast::BinaryOperation::Add => bc.emit(&ast, ByteOp::Add),
                    ast::BinaryOperation::Subtract => bc.emit(&ast, ByteOp::Sub),
                    ast::BinaryOperation::Multiply => bc.emit(&ast, ByteOp::Mul),
                    ast::BinaryOperation::Divide => bc.emit(&ast, ByteOp::Div),

                    ast::BinaryOperation::Less => bc.emit(&ast, ByteOp::Less),
                    ast::BinaryOperation::LessEqual => {
                        bc.emit(&ast, ByteOp::Greater);
                        bc.emit(&ast, ByteOp::Not);
                    },
                    ast::BinaryOperation::Greater => bc.emit(&ast, ByteOp::Greater),
                    ast::BinaryOperation::GreaterEqual => {
                        bc.emit(&ast, ByteOp::Less);
                        bc.emit(&ast, ByteOp::Not);
                    },
                    ast::BinaryOperation::Equal => bc.emit(&ast, ByteOp::Equal),
                    ast::BinaryOperation::NotEqual => {
                        bc.emit(&ast, ByteOp::Equal);
                        bc.emit(&ast, ByteOp::Not);
                    }
                    ast::BinaryOperation::Assign => {},
                };
            }
        },
        ast::Ast::Unary(op, expr) => {
            ast_to_bytecode(bc, &*expr)?;
            match op {
                ast::UnaryOperation::Negate => bc.emit(&ast, ByteOp::Negate),
                ast::UnaryOperation::Not => bc.emit(&ast, ByteOp::Not)
            }
        },
        ast::Ast::Return(expr) => {
            ast_to_bytecode(bc, &*expr)?;
            bc.emit(&ast, ByteOp::Return);
        },
        ast::Ast::Block(exprs) => {
            bc.emit(&ast, ByteOp::ScopeOpen);
            let mut idx: usize = 1;
            let len = exprs.len();
            for e in exprs {
                match &e.node {
                    ast::Ast::Statement(expr) => {
                        if let ast::Ast::Block(_) = expr.node {
                            ast_to_bytecode(bc, &*expr)?;
                        } else {
                            ast_to_bytecode(bc, &e)?
                        }
                    },
                    ast::Ast::Return(expr) => {
                        ast_to_bytecode(bc, &*expr)?;
                    }
                    _ => {
                        if idx != len{
                            return Err(VmError::new("Only the last element in a block may be an expression".to_string(), &ast));
                        }
                        ast_to_bytecode(bc, &e)?;
                        bc.emit(&ast, ByteOp::Return);
                    }
                }
                idx += 1;
            }
            bc.emit(&ast, ByteOp::ScopeClose);
        },
        ast::Ast::IfElse(ifcond, ifexpr, elseifs, elseexpr) => {
            // ppens a new scope for the if expression
            bc.emit(&ast, ByteOp::ScopeOpen);

            // generates code for condition that and creates a temporary jump instruction
            ast_to_bytecode(bc, &*ifcond)?;
            bc.emit(&ast, ByteOp::JumpFalse(1));
            let last_jump_idx = bc.ops.len() - 1;

            ast_to_bytecode(bc, &*ifexpr)?;


            // patches the temporary jump instruction to the end of the if expression's expression
            bc.ops[last_jump_idx] = ByteOp::JumpFalse((bc.ops.len() as isize - last_jump_idx as isize + 2) as i16);

            // closes if scope
            bc.emit(&ast, ByteOp::ScopeClose);

            // creates a list of the indecies of the temporary jumps that jump to the end of the entire if expresssion
            let mut last_patch_idx: Vec<usize> = Vec::new();

            // adds the first if to the list of jumps that needs patching
            bc.emit(&ast, ByteOp::Jump(1));
            last_patch_idx.push(bc.ops.len() - 1);

            for (cond, expr) in elseifs {
                // opens a new scope for the else if expression
                bc.emit(&ast, ByteOp::ScopeOpen);
                
                // generates code for condition that and creates a temporary jump instruction
                ast_to_bytecode(bc, &*cond)?;
                bc.emit(&ast, ByteOp::JumpFalse(1));
                let last_jump_idx = bc.ops.len() - 1;

                ast_to_bytecode(bc, &*expr)?;

                // patches the temporary jump instruction to the end of the if expression's expression
                bc.ops[last_jump_idx] = ByteOp::JumpFalse((bc.ops.len()  as isize - last_jump_idx as isize + 2) as i16);

                // closes scope for the else if expression
                bc.emit(&ast, ByteOp::ScopeClose);

                // adds to list of temporary jumps that need to be patched
                bc.emit(&ast, ByteOp::Jump(1));
                last_patch_idx.push(bc.ops.len() - 1);
            }

            // generates code for else expression if present
            if let Some(eexpr) = elseexpr {
                ast_to_bytecode(bc, &*eexpr)?;
            }

            // patches all temporary jumps
            for patch in last_patch_idx {
                bc.ops[patch] = ByteOp::Jump((bc.ops.len()  as isize - patch as isize) as i16);
            }
        },
        ast::Ast::While(cond, expr) => {

            // opens a new scope for the while expression
            bc.emit(&ast, ByteOp::ScopeOpen);

            // saves index in code to the begining of the condition expression
            let begin_idx = bc.ops.len();
            ast_to_bytecode(bc, &*cond)?;

            // adds a temporary jump that needs to be patched that jumps to the end of the entire expression
            bc.emit(&ast, ByteOp::JumpFalse(1));
            let cond_jump_idx = bc.ops.len() - 1;

            ast_to_bytecode(bc, &*expr)?;

            // patches jump to begining of the condition expression
            bc.emit(&ast, ByteOp::Jump((begin_idx as isize - bc.ops.len() as isize) as i16));

            // patches conditional jump to end of entire expression
            bc.ops[cond_jump_idx] = ByteOp::JumpFalse((bc.ops.len() as isize - cond_jump_idx as isize) as i16);

            // closes the while expression scope
            bc.emit(&ast, ByteOp::ScopeClose);
        },
        ast::Ast::Let(var_sig, mutable, set_expr) => {
            if let Some(se) = set_expr {
                ast_to_bytecode(bc, &*se)?;
            }
            let hash = crc16::State::<crc16::ARC>::calculate(var_sig.name.as_bytes());

            #[cfg(feature = "store_names")]
            bc.names.insert(hash, var_sig.name.clone());

            if *mutable {
                bc.emit(&ast, ByteOp::DefMutVar(hash));
            } else {
                bc.emit(&ast, ByteOp::DefVar(hash));
            }
        },
        ast::Ast::Import(_, expr) => {
            ast_to_bytecode(bc, &*expr)?;
        },
        ast::Ast::FnCall(callee, args) => {
            if let ast::Ast::Identifier(name) = &callee.node {
                for a in args {
                    ast_to_bytecode(bc, &a)?;
                }
                let hash = crc16::State::<crc16::ARC>::calculate(name.as_bytes());

                #[cfg(feature = "store_names")]
                bc.names.insert(hash, name.clone());
                
                bc.emit(&ast, ByteOp::NativeFnCall(hash, args.len() as u8));
            } else {
                return Err(VmError::new(format!("Function variables not support yet"), &ast));
            }
        },
        other => { return Err(VmError::new(format!("Non implemented AST node: {:?}", other), &ast)); }
    };
    Ok(())
}

struct Scope {
    variables: HashMap<u16, (bool, Value)>
}

pub type NativeVmFn = fn(&mut StackVm, &Bytecode) -> Result<(), VmError>;

pub struct StackVm {
    ip_idx: usize,
    pub stack: Vec<Value>,
    scopes: Vec<Scope>,

    native_fns: HashMap<u16, (u8, NativeVmFn)>,
}

fn read_num(vm: &mut StackVm, bc: &Bytecode) -> Result<(), VmError> {
    print!("Input number: "); std::io::stdout().flush().unwrap();

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

fn read_bool(vm: &mut StackVm, bc: &Bytecode) -> Result<(), VmError> {
    print!("Input bool [true, false]: "); std::io::stdout().flush().unwrap();

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

fn println(vm: &mut StackVm, _bc: &Bytecode) -> Result<(), VmError> {
    match vm.stack.pop() {
        Some(val) => {
            match val {
                Value::Nil => println!("nil"),
                Value::Bool(b) => println!("{}", b),
                Value::Number(n) => println!("{}", n)
            }
        },
        None => println!("No value in stack")
    }
    Ok(())
}

fn print(vm: &mut StackVm, _bc: &Bytecode) -> Result<(), VmError> {
    match vm.stack.pop() {
        Some(val) => {
            match val {
                Value::Nil => print!("nil"),
                Value::Bool(b) => print!("{}", b),
                Value::Number(n) => print!("{}", n)
            }
        },
        None => println!("No value in stack")
    }
    std::io::stdout().flush().unwrap();
    Ok(())
}

impl<'a> StackVm {
    pub fn new() -> StackVm {
        let mut vm = StackVm {
            ip_idx: 0,
            stack: Vec::new(),
            scopes: Vec::new(),

            native_fns: HashMap::new(),
        };

        vm.add_fn(&"read_num".to_string(), 0, read_num);
        vm.add_fn(&"read_bool".to_string(), 0, read_bool);
        vm.add_fn(&"println".to_string(), 1, println);
        vm.add_fn(&"print".to_string(), 1, print);

        return vm;
    }

    pub fn add_fn(&mut self, name: &String, arg_count: u8, function: NativeVmFn) {
        let hash = crc16::State::<crc16::ARC>::calculate(name.as_bytes());
        self.native_fns.insert(hash, (arg_count, function));
    }

    fn stack_peek(&self, distance: usize) -> Value {
        if let Some(v) = self.stack.get(self.stack.len() - 1 - distance) {
            v.clone()
        } else {
            Value::Nil
        }
    }

    fn var_in_scopes_mut(scope_stack: &'a mut Vec<Scope>, id: &u16) -> Option<&'a mut (bool, Value)> {
        for s in scope_stack.iter_mut().rev() {
            match s.variables.get_mut(id) {
                Some(var) => return Some(var),
                None => continue
            }
        }
        None
    }

    fn var_in_scopes(scope_stack: &'a Vec<Scope>, id: &u16) -> Option<&'a (bool, Value)> {
        for s in scope_stack.iter().rev() {
            match s.variables.get(id) {
                Some(var) => return Some(var),
                None => continue
            }
        }
        None
    }

    #[cfg(feature = "node_code_pos")]
    pub fn make_error(&self, bc: &Bytecode, msg: String) -> VmError {
        VmError {
            msg,
            pos: bc.positions[self.ip_idx]
        }
    }

    #[cfg(not(feature = "node_code_pos"))]
    pub fn make_error(&self, bc: &Bytecode, msg: String) -> VmError {
        VmError {
            msg
        }
    }

    pub fn run(&mut self, bc: Bytecode, debug_level: i32) -> Result<Value, VmError> {
        'run: loop {
            if debug_level >= 3 {
                match bc.ops.get(self.ip_idx) {
                    Some(op) => println!("{:?}\n{:?}\n", self.stack, op),
                    None => ()
                }
            }
            match bc.ops.get(self.ip_idx) {
                Some(ByteOp::Load(n)) => {
                    self.stack.push(bc.constants[*n as usize].clone());
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
                        Value::Nil => { return Err(self.make_error(&bc, "Binary add value cannot be nil".to_string())); }
                        Value::Bool(_) => { return Err(self.make_error(&bc, "Binary add value cannot be bool".to_string())); }
                        Value::Number(_) => {
                            match self.stack_peek(1) {
                                Value::Nil => { return Err(self.make_error(&bc, "Binary add value cannot be nil".to_string())); }
                                Value::Bool(_) => { return Err(self.make_error(&bc, "Binary add value cannot be bool".to_string())); }
                                Value::Number(_) => {
                                    if let Some(Value::Number(b)) = self.stack.pop() {
                                        if let Some(Value::Number(a)) = self.stack.pop() {
                                            self.stack.push(Value::Number(a + b));
                                        } else {
                                            return Err(self.make_error(&bc, "Failed to pop binary add a".to_string()));
                                        }
                                    } else {
                                        return Err(self.make_error(&bc, "Failed to pop binary add b".to_string()));
                                    }

                                } 
                            }
                        }
                    }
                },
                Some(ByteOp::Sub) => {
                    match self.stack_peek(0) {
                        Value::Nil => { return Err(self.make_error(&bc, "Binary sub value cannot be nil".to_string())); }
                        Value::Bool(_) => { return Err(self.make_error(&bc, "Binary sub value cannot be bool".to_string())); }
                        Value::Number(_) => {
                            match self.stack_peek(1) {
                                Value::Nil => { return Err(self.make_error(&bc, "Binary sub value cannot be nil".to_string())); }
                                Value::Bool(_) => { return Err(self.make_error(&bc, "Binary sub value cannot be bool".to_string())); }
                                Value::Number(_) => {
                                    if let Some(Value::Number(b)) = self.stack.pop() {
                                        if let Some(Value::Number(a)) = self.stack.pop() {
                                            self.stack.push(Value::Number(a - b));
                                        } else {
                                            return Err(self.make_error(&bc, "Failed to pop binary sub a".to_string()));
                                        }
                                    } else {
                                        return Err(self.make_error(&bc, "Failed to pop binary sub b".to_string()));
                                    }

                                } 
                            }
                        }
                    }
                },
                Some(ByteOp::Mul) => {
                    match self.stack_peek(0) {
                        Value::Nil => { return Err(self.make_error(&bc, "Binary mul value cannot be nil".to_string())); }
                        Value::Bool(_) => { return Err(self.make_error(&bc, "Binary mul value cannot be bool".to_string())); }
                        Value::Number(_) => {
                            match self.stack_peek(1) {
                                Value::Nil => { return Err(self.make_error(&bc, "Binary mul value cannot be nil".to_string())); }
                                Value::Bool(_) => { return Err(self.make_error(&bc, "Binary mul value cannot be bool".to_string())); }
                                Value::Number(_) => {
                                    if let Some(Value::Number(b)) = self.stack.pop() {
                                        if let Some(Value::Number(a)) = self.stack.pop() {
                                            self.stack.push(Value::Number(a * b));
                                        } else {
                                            return Err(self.make_error(&bc, "Failed to pop binary mul a".to_string()));
                                        }
                                    } else {
                                        return Err(self.make_error(&bc, "Failed to pop binary mul b".to_string()));
                                    }

                                } 
                            }
                        }
                    }
                },
                Some(ByteOp::Div) => {
                    match self.stack_peek(0) {
                        Value::Nil => { return Err(self.make_error(&bc, "Binary div value cannot be nil".to_string())); }
                        Value::Bool(_) => { return Err(self.make_error(&bc, "Binary div value cannot be bool".to_string())); }
                        Value::Number(_) => {
                            match self.stack_peek(1) {
                                Value::Nil => { return Err(self.make_error(&bc, "Binary div value cannot be nil".to_string())); }
                                Value::Bool(_) => { return Err(self.make_error(&bc, "Binary div value cannot be bool".to_string())); }
                                Value::Number(_) => {
                                    if let Some(Value::Number(b)) = self.stack.pop() {
                                        if let Some(Value::Number(a)) = self.stack.pop() {
                                            self.stack.push(Value::Number(a / b));
                                        } else {
                                            return Err(self.make_error(&bc, "Failed to pop binary div a".to_string()));
                                        }
                                    } else {
                                        return Err(self.make_error(&bc, "Failed to pop binary div b".to_string()));
                                    }
                                } 
                            }
                        }
                    }
                },
                Some(ByteOp::Not) => {
                    match self.stack_peek(0) {
                        Value::Nil => { return Err(self.make_error(&bc, "Unary not value cannot be nil".to_string())); }
                        Value::Bool(_) => {
                            if let Some(Value::Bool(b)) = self.stack.pop() {
                                self.stack.push(Value::Bool(!b));
                            } else {
                                return Err(self.make_error(&bc, "Failed to pop unary not value".to_string()));
                            }
                        }
                        Value::Number(_) => { return Err(self.make_error(&bc, "Unary value cannot be number".to_string())); }
                    }
                },
                Some(ByteOp::Equal) => {
                    match self.stack_peek(0) {
                        Value::Nil => { return Err(self.make_error(&bc, "Binary equal value cannot be nil".to_string())); }
                        Value::Bool(_) => { return Err(self.make_error(&bc, "Binary equal value cannot be bool".to_string())); }
                        Value::Number(_) => {
                            match self.stack_peek(1) {
                                Value::Nil => { return Err(self.make_error(&bc, "Binary equal value cannot be nil".to_string())); }
                                Value::Bool(_) => { return Err(self.make_error(&bc, "Binary equal value cannot be bool".to_string())); }
                                Value::Number(_) => {
                                    if let Some(Value::Number(b)) = self.stack.pop() {
                                        if let Some(Value::Number(a)) = self.stack.pop() {
                                            self.stack.push(Value::Bool(a == b));
                                        } else {
                                            return Err(self.make_error(&bc, "Failed to pop binary equal a".to_string()));
                                        }
                                    } else {
                                        return Err(self.make_error(&bc, "Failed to pop binary equal b".to_string()));
                                    }

                                } 
                            }
                        }
                    }
                },
                Some(ByteOp::Greater) => {
                    match self.stack_peek(0) {
                        Value::Nil => { return Err(self.make_error(&bc, "Binary greater value cannot be nil".to_string())); }
                        Value::Bool(_) => { return Err(self.make_error(&bc, "Binary greater value cannot be bool".to_string())); }
                        Value::Number(_) => {
                            match self.stack_peek(1) {
                                Value::Nil => { return Err(self.make_error(&bc, "Binary greater value cannot be nil".to_string())); }
                                Value::Bool(_) => { return Err(self.make_error(&bc, "Binary greater value cannot be bool".to_string())); }
                                Value::Number(_) => {
                                    if let Some(Value::Number(b)) = self.stack.pop() {
                                        if let Some(Value::Number(a)) = self.stack.pop() {
                                            self.stack.push(Value::Bool(a > b));
                                        } else {
                                            return Err(self.make_error(&bc, "Failed to pop binary greater a".to_string()));
                                        }
                                    } else {
                                        return Err(self.make_error(&bc, "Failed to pop binary greater b".to_string()));
                                    }

                                } 
                            }
                        }
                    }
                },
                Some(ByteOp::Less) => {
                    match self.stack_peek(0) {
                        Value::Nil => { return Err(self.make_error(&bc, "Binary less value cannot be nil".to_string())); }
                        Value::Bool(_) => { return Err(self.make_error(&bc, "Binary less value cannot be bool".to_string())); }
                        Value::Number(_) => {
                            match self.stack_peek(1) {
                                Value::Nil => { return Err(self.make_error(&bc, "Binary less value cannot be nil".to_string())); }
                                Value::Bool(_) => { return Err(self.make_error(&bc, "Binary less value cannot be bool".to_string())); }
                                Value::Number(_) => {
                                    if let Some(Value::Number(b)) = self.stack.pop() {
                                        if let Some(Value::Number(a)) = self.stack.pop() {
                                            self.stack.push(Value::Bool(a < b));
                                        } else {
                                            return Err(self.make_error(&bc, "Failed to pop binary less a".to_string()));
                                        }
                                    } else {
                                        return Err(self.make_error(&bc, "Failed to pop binary less b".to_string()));
                                    }

                                } 
                            }
                        }
                    }
                },
                Some(ByteOp::Negate) => {
                    match self.stack_peek(0) {
                        Value::Nil => { return Err(self.make_error(&bc, "Unary negate value cannot be nil".to_string())); }
                        Value::Bool(_) => { return Err(self.make_error(&bc, "Unary negate value cannot be bool".to_string())); }
                        Value::Number(_) => {
                            if let Some(Value::Number(n)) = self.stack.pop() {
                                self.stack.push(Value::Number(-n));
                            } else {
                                return Err(self.make_error(&bc, "Failed to pop unary negate value".to_string()));
                            }
                        }
                    }
                },
                Some(ByteOp::ScopeOpen) => {
                    self.scopes.push(Scope { variables: HashMap::new() });
                },
                Some(ByteOp::ScopeClose) => {
                    self.scopes.pop();
                },
                Some(ByteOp::NativeFnCall(id, arg_count)) => {
                    if let Some(function) = self.native_fns.get(id) {
                        if function.0 != *arg_count {
                            #[cfg(feature = "store_names")]
                            return Err(self.make_error(&bc, format!("Function: {} expects {} arguments but recieved {}", bc.names[id], function.0, arg_count)));

                            #[cfg(not(feature = "store_names"))]
                            return Err(self.make_error(&bc, format!("Function: {} expects {} arguments but recieved {}", id, function.0, arg_count)));
                        } else if let Err(e)  = (function.1)(self, &bc) {
                            #[cfg(feature = "store_names")]
                            return Err(self.make_error(&bc, format!("Function: {} returned an error: {}", bc.names[id], e.msg)));

                            #[cfg(not(feature = "store_names"))]
                            return Err(self.make_error(&bc, format!("Function: {} returned an error: {}", id, e.msg)));
                        }
                    } else {
                        #[cfg(feature = "store_names")]
                        return Err(self.make_error(&bc, format!("Function: {} not defined", bc.names[id])));

                        #[cfg(not(feature = "store_names"))]
                        return Err(self.make_error(&bc, format!("Function: {} not defined", id)));
                    }
                },
                Some(ByteOp::DefVar(id)) => {
                    match StackVm::var_in_scopes_mut(&mut self.scopes, id) {
                        Some(_) => {
                            #[cfg(feature = "store_names")]
                            return Err(self.make_error(&bc, format!("Variable: {} already defined", bc.names[id])));

                            #[cfg(not(feature = "store_names"))]
                            return Err(self.make_error(&bc, format!("Variable: {} already defined", id)));
                        },
                        None => {
                            self.scopes.last_mut().unwrap().variables.insert(*id, if let Some(val) = self.stack.pop() { self.stack.push(val.clone()); (false, val) } else { (false, Value::Nil) });
                        }
                    }
                },
                Some(ByteOp::DefMutVar(id)) => {
                    match StackVm::var_in_scopes_mut(&mut self.scopes, id) {
                        Some(_) => {

                            #[cfg(feature = "store_names")]
                            return Err(self.make_error(&bc, format!("Variable: {} already defined", bc.names[id])));

                            #[cfg(not(feature = "store_names"))]
                            return Err(self.make_error(&bc, format!("Variable: {} already defined", id)));
                        },
                        None => {
                            self.scopes.last_mut().unwrap().variables.insert(*id, if let Some(val) = self.stack.pop() { self.stack.push(val.clone()); (true, val) } else { (true, Value::Nil) });
                        }
                    }
                },
                Some(ByteOp::SetVar(id)) => {
                    match StackVm::var_in_scopes_mut(&mut self.scopes, id) {
                        Some(val) => {
                            if val.0 {
                                if self.stack.len() > 0 {
                                    val.1 = self.stack.pop().unwrap();
                                    self.stack.push(val.1.clone());
                                } else {
                                    val.1 = Value::Nil;
                                }
                            } else {

                                #[cfg(feature = "store_names")]
                                return Err(self.make_error(&bc, format!("Variable {} is not mutable", bc.names[id])));

                                #[cfg(not(feature = "store_names"))]
                                return Err(self.make_error(&bc, format!("Variable {} is not mutable", id)));
                            }
                        },
                        None => {
                            return Err(self.make_error(&bc, format!("Variable {} not defined", id)));
                        }
                    }
                },
                Some(ByteOp::GetVar(id)) => {
                    match StackVm::var_in_scopes(&self.scopes, id) {
                        Some(val) => {
                            self.stack.push(val.1.clone());
                        },
                        None => {

                            #[cfg(feature = "store_names")]
                            return Err(self.make_error(&bc, format!("Failed to find variable {} in scope", bc.names[id])));

                            #[cfg(not(feature = "store_names"))]
                            return Err(self.make_error(&bc, format!("Failed to find variable {} in scope", id)));
                        }
                    }
                },
                Some(ByteOp::Jump(distance)) => {
                    self.ip_idx = (self.ip_idx as isize + *distance as isize) as usize;
                    continue 'run;
                },
                Some(ByteOp::JumpFalse(distance)) => {
                    match self.stack_peek(0) {
                        Value::Nil => { return Err(self.make_error(&bc, "Jump on false value cannot be nil".to_string())); },
                        Value::Bool(_) => {
                            if let Some(Value::Bool(b)) = self.stack.pop() {
                                if !b {
                                    self.ip_idx = (self.ip_idx as isize + *distance as isize) as usize;
                                    continue 'run;
                                }
                            } else {
                                return Err(self.make_error(&bc, "Failed to pop jump on false value".to_string()));
                            }
                        },
                        Value::Number(_) => { return Err(self.make_error(&bc, "Jump on false value cannot be bool".to_string())); },
                    }
                },
                Some(ByteOp::JumpTrue(distance)) => {
                    match self.stack_peek(0) {
                        Value::Nil => { return Err(self.make_error(&bc, "Jump on true value cannot be nil".to_string())); },
                        Value::Bool(_) => {
                            if let Some(Value::Bool(b)) = self.stack.pop() {
                                if b {
                                    self.ip_idx = (self.ip_idx as isize + *distance as isize) as usize;
                                    continue 'run;
                                }
                            } else {
                                return Err(self.make_error(&bc, "Failed to pop jump on true value".to_string()));
                            }
                        },
                        Value::Number(_) => { return Err(self.make_error(&bc, "Jump on true value cannot be bool".to_string())); },
                    }
                },
                Some(ByteOp::Pop) => {
                    self.stack.pop();
                },
                Some(ByteOp::Return) => {
                    if let Some(v) = self.stack.pop() {
                        self.scopes.pop();
                        if self.scopes.len() > 0 {
                            self.stack.push(v);
                            while match bc.ops[self.ip_idx] {
                                ByteOp::ScopeClose => false,
                                _ => true,
                            } {
                                self.ip_idx += 1;
                            }
                        } else {
                            return Ok(v);
                        }
                    } else {
                        self.scopes.pop();
                        if self.scopes.len() > 0 {
                            self.stack.push(Value::Nil);
                        } else {
                            return Ok(Value::Nil);
                        }
                    }
                },
                None => { break 'run; }
            };
            self.ip_idx += 1;
        }
        Ok(Value::Nil)
    }
}