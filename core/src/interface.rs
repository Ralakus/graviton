use super::{ir, Notice};
use memmap::Mmap;
use std::collections::HashMap;

/// A trait that each stage of the compiler will implement, such as the lexer/parser, semantic analyzer, and the backend's frontend
pub trait Stage {
    fn run(&mut self) -> Vec<Notice>;
}

/// The struct that manages each stage and how they're ran and the data passed between each stage
#[derive(Default)]
pub struct Interface {
    /// Each stage of the compiler
    pub stages: Vec<Box<dyn Stage>>,
    /// A vector ir module
    pub ir: Vec<ir::Module>,
    /// A map of open maps
    pub maps: HashMap<String, Mmap>,
}

impl<'a> Interface {
    /// Creates a new interface
    pub fn new() -> Self {
        Interface::default()
    }

    /// Adds a new stage to the order
    pub fn add_stage(&mut self, stage: Box<dyn Stage>) {
        self.stages.push(stage);
    }

    /// Opens a file to the file stack
    pub fn open_file(&mut self, file_name: &'a str) -> Result<(), FileIoError> {
        let file = if let Ok(f) = std::fs::File::open(file_name) {
            f
        } else {
            return Err(FileIoError::FileNotFound);
        };
        let map = unsafe {
            if let Ok(mf) = Mmap::map(&file) {
                mf
            } else {
                return Err(FileIoError::ErrorMapping);
            }
        };
        self.maps.insert(file_name.to_string(), map);
        Ok(())
    }

    /// Closes a file from the file stack
    pub fn close_file(&mut self, _file_name: &'a str) -> Result<(), FileIoError> {
        Ok(())
    }
}

pub enum FileIoError {
    Unknown,
    FileNotFound,
    ErrorMapping,
    Utf8ConversionError,
}
