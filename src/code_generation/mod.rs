// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.

use crate::parser::SymbolTableEntry;
use crate::parser::{ASTnode, ASTop};

use std::collections::HashMap;
use std::error::Error;
use std::rc::Rc;

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
    fn free_all_registers(&mut self);
    fn load_integer(&mut self, i: i32) -> usize;
    fn add(&mut self, r1: usize, r2: usize) -> usize;
    fn minus(&mut self, r1: usize, r2: usize) -> usize;
    fn multiply(&mut self, r1: usize, r2: usize) -> usize;
    fn divide(&mut self, r1: usize, r2: usize) -> usize;
    fn print(&mut self, r1: usize);
    fn load_glob_var(&mut self, sym: &SymbolTableEntry) -> usize;
    fn assign_glob_var(&mut self, sym: &SymbolTableEntry, r: usize) -> usize;
    fn gen_glob_syms(&mut self, symtable: HashMap<String, Rc<SymbolTableEntry>>);
    fn preamble(&mut self);
    fn postamble(&mut self);
    fn generate_output(&mut self, output_filename: &str) -> Result<(), Box<dyn Error>>;
}

// Generates the code for the ASTnode,
// also returns the register containing the result
// of this node.
pub fn generate_code_for_node(
    generator: &mut impl Generator,
    node: &Box<ASTnode>,
) -> Option<usize> {
    let mut left_reg: Option<usize> = None;
    if let Some(left) = &node.left {
        left_reg = generate_code_for_node(generator, left);

        if node.op == ASTop::GLUE {
            generator.free_all_registers();
        }
    }

    let mut right_reg: Option<usize> = None;
    if let Some(right) = &node.right {
        right_reg = generate_code_for_node(generator, right);

        if node.op == ASTop::GLUE {
            generator.free_all_registers();
        }
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
        ASTop::IDENT => {
            if node.rvalue {
                let sym = node.symtable_entry.as_ref()?;
                Some(generator.load_glob_var(sym))
            } else {
                // In cases where an IDENT is not used as a left-value,
                // the parent node will take care of what needs to be done.
                None
            }
        }
        ASTop::ASSIGN => {
            let sym = node.left.as_ref()?.symtable_entry.as_ref()?;
            Some(generator.assign_glob_var(sym, right_reg?))
        }
        ASTop::NOOP | ASTop::GLUE => None,
    }
}
