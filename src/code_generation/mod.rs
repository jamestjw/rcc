use crate::parser::{ASTnode, ASTop};
use std::error::Error;

pub mod x86_64;

pub struct Register {
    pub name: String,
    free: bool,
}

impl Register {
    fn new(name: String) -> Register {
        Self { name, free: true }
    }
    fn free(&mut self) {
        if self.free {
            panic!("Freeing already free register.");
        }
        self.free = true;
    }
}

// So that we can support multiple types of generators
// depending on the architecture
pub trait Generator {
    fn alloc_register(&mut self) -> usize;
    fn free_register(&mut self, i: usize);
    fn load_integer(&mut self, i: i32) -> usize;
    fn add(&mut self, r1: usize, r2: usize) -> usize;
    fn minus(&mut self, r1: usize, r2: usize) -> usize;
    fn multiply(&mut self, r1: usize, r2: usize) -> usize;
    fn divide(&mut self, r1: usize, r2: usize) -> usize;
    fn print(&mut self, r1: usize);
    fn preamble(&mut self);
    fn postamble(&mut self);
    fn generate_output(&mut self, output_filename: &str) -> Result<(), Box<dyn Error>>;
}

// Generates the code for the ASTnode,
// also returns the register containing the result
// of this node.
pub fn generate_code_for_node(generator: &mut impl Generator, node: Box<ASTnode>) -> Option<usize> {
    let mut left_reg: Option<usize> = None;
    if let Some(left) = node.left {
        left_reg = generate_code_for_node(generator, left);
    }

    let mut right_reg: Option<usize> = None;
    if let Some(right) = node.right {
        right_reg = generate_code_for_node(generator, right);
    }

    match node.op {
        ASTop::INTLIT => Some(generator.load_integer(node.int_value)),
        ASTop::ADD => Some(generator.add(left_reg?, right_reg?)),
        ASTop::MINUS => Some(generator.minus(left_reg?, right_reg?)),
        ASTop::MULTIPLY => Some(generator.multiply(left_reg?, right_reg?)),
        ASTop::DIVIDE => Some(generator.divide(left_reg?, right_reg?)),
        ASTop::PRINT => {
            generator.print(left_reg?);
            None
        }
    }
}
