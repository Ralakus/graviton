use super::{ast, Notice};

/// A trait that each stage of the compiler will implement, such as the lexer/parser, semantic analyzer, and the backend's frontend
pub trait Stage {
    fn run(&mut self, stack: &mut ast::Ast) -> Vec<Notice>;
}

/// The struct that manages each stage and how they're ran and the data passed between each stage
#[derive(Default)]
pub struct Interface {
    pub stages: Vec<Box<dyn Stage>>,
    pub ast: ast::Ast,
}

impl Interface {
    /// Creates a new interface
    pub fn new() -> Self {
        Interface::default()
    }

    /// Adds a new stage to the order
    pub fn add_stage(&mut self, stage: Box<dyn Stage>) {
        self.stages.push(stage);
    }
}
