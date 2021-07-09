// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.

use crate::parser::{ASTnode, ASTop};
use crate::parser::{SymPosition, SymbolTableEntry};

use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::path::Path;
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
    fn gen_glob_syms(&mut self, symtable: &HashMap<String, Rc<RefCell<SymbolTableEntry>>>);
    fn preamble(&mut self);
    fn postamble(&mut self);
    fn func_preamble(&mut self, fn_name: &str);
    fn func_postamble(&mut self, end_label: &str);
    fn return_from_func(&mut self, label: &str, r: Option<usize>);
    fn funccall(&mut self, fn_name: &str, num_params: usize) -> usize;
    fn generate_output(&mut self, output_filename: &Path) -> Result<(), Box<dyn Error>>;
    fn set_sym_positions(&mut self, symtable: &HashMap<String, Rc<RefCell<SymbolTableEntry>>>);
    fn move_to_position(&mut self, r: usize, posn: &SymPosition);
}

// Generates the code for the ASTnode,
// also returns the register containing the result
// of this node.
pub fn generate_code_for_node(
    generator: &mut impl Generator,
    node: &Box<ASTnode>,
) -> Option<usize> {
    // Special cases
    if node.op == ASTop::FUNCTION {
        generate_code_for_function(generator, node);
        return None;
    } else if node.op == ASTop::FUNCCALL {
        return Some(generate_code_for_funccall(generator, node));
    }

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
                let sym = node.symtable_entry.as_ref().unwrap().borrow();
                Some(generator.load_glob_var(&sym))
            } else {
                // In cases where an IDENT is not used as a left-value,
                // the parent node will take care of what needs to be done.
                None
            }
        }
        ASTop::ASSIGN => {
            let sym = node
                .left
                .as_ref()
                .unwrap()
                .symtable_entry
                .as_ref()
                .unwrap()
                .borrow();
            Some(generator.assign_glob_var(&sym, right_reg?))
        }
        ASTop::RETURN => {
            generator.return_from_func(node.label.as_ref()?, left_reg);
            None
        }
        ASTop::NOOP | ASTop::GLUE => None,
        _ => {
            panic!(
                "Unknown ASTop:{} encountered during code generation",
                node.op.name()
            );
        }
    }
}

// fn_node should be an ASTnode with ASTop::FUNCTION
pub fn generate_code_for_function(generator: &mut impl Generator, fn_node: &Box<ASTnode>) {
    // This is safe as the left node and symtable_entry should always be present
    // if the operation is ASTop::FUNCTION.
    let fn_body_node = fn_node.left.as_ref().unwrap();
    let sym = fn_node.symtable_entry.as_ref().unwrap().borrow();

    generator.func_preamble(&sym.name);
    generate_code_for_node(generator, fn_body_node);
    generator.func_postamble(&generate_label_for_function(&sym));
}

pub fn generate_label_for_function(sym: &SymbolTableEntry) -> String {
    format!("L_{}_end", sym.name)
}

// fn_node should be an ASTnode with ASTop::FUNCCALL
fn generate_code_for_funccall(generator: &mut impl Generator, fn_node: &Box<ASTnode>) -> usize {
    // This is safe as the left node should always be present along with its symbol
    // if the operation is ASTop::FUNCCALL.
    let fn_sym = fn_node
        .left
        .as_ref()
        .unwrap()
        .symtable_entry
        .as_ref()
        .unwrap()
        .borrow();
    let param_node = fn_node.right.as_ref();
    let mut num_params = 0;
    match param_node {
        Some(param_node) => {
            num_params = generate_code_for_funccall_param(generator, param_node);
        }
        None => {}
    };

    return generator.funccall(&fn_sym.name, num_params as usize);
}

// Returns the number of params that we generated code for
fn generate_code_for_funccall_param(
    generator: &mut impl Generator,
    param_node: &Box<ASTnode>,
) -> u8 {
    match &param_node.right {
        Some(n) => {
            let r = generate_code_for_node(generator, n);
            match r {
                Some(r) => {
                    // Symtable entry should be available here, this shouldn't ever
                    // fail for FUNCPARAMs
                    generator.move_to_position(
                        r,
                        &param_node.symtable_entry.as_ref().unwrap().borrow().posn,
                    );
                }
                None => {
                    // This branch should never be invoked.
                    panic!("Expression to be used an argument to function is returned nothing.");
                }
            };
        }
        None => {
            // This should never happen.
            panic!("Empty right node for ASTop::FUNCPARAM node.");
        }
    }

    if let Some(next_node) = &param_node.left {
        generate_code_for_funccall_param(generator, next_node) + 1
    } else {
        1
    }
}
