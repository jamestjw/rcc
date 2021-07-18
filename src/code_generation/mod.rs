// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.

use crate::parser::{
    pointer_to, ASTnode, ASTop, DataType, SymPosition, SymType, SymbolTable, SymbolTableEntry,
};
use crate::string_table::StringTable;

use std::error::Error;
use std::path::Path;
use std::rc::Rc;

pub mod x86_64;

pub struct Register {
    quad_name: String,
    long_name: String,
    byte_name: String,
    free: bool,
}

impl Register {
    fn new(quad_name: String, long_name: String, byte_name: String) -> Register {
        Self {
            quad_name,
            long_name,
            byte_name,
            free: true,
        }
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
    fn load_strlit(&mut self, id: usize) -> usize;
    fn add(&mut self, r1: usize, r2: usize) -> usize;
    fn minus(&mut self, r1: usize, r2: usize) -> usize;
    fn multiply(&mut self, r1: usize, r2: usize) -> usize;
    fn divide(&mut self, r1: usize, r2: usize, data_size: u32) -> usize;
    fn load_var(&mut self, sym: &SymbolTableEntry) -> usize;
    fn load_from_addr(&mut self, r: usize, data_size: u32) -> usize;
    fn load_addr(&mut self, sym: &SymbolTableEntry) -> usize;
    fn assign_glob_var(&mut self, sym: &SymbolTableEntry, r: usize) -> usize;
    fn assign_to_addr(&mut self, r1: usize, r2: usize, data_size: u32) -> usize;
    fn gen_glob_syms(&mut self, symtable: &SymbolTable);
    fn preamble(&mut self);
    fn postamble(&mut self);
    fn func_preamble(&mut self, fn_name: &str);
    fn func_postamble(&mut self, end_label: &str);
    fn return_from_func(&mut self, label: &str, r: Option<usize>);
    fn funccall(&mut self, fn_name: &str, num_params: usize) -> usize;
    fn generate_output(&mut self, output_filename: &Path) -> Result<(), Box<dyn Error>>;
    // Set symbol position and sizes
    fn preprocess_symbols(&mut self, symtable: &SymbolTable);
    fn move_to_position(&mut self, r: usize, posn: &SymPosition, data_size: u32);
    fn data_type_to_size(&self, data_type: DataType) -> u32;
    fn gen_strlits(&mut self, string_table: &StringTable);
}

// Generates the code for the ASTnode,
// also returns the register containing the result
// of this node.
pub fn generate_code_for_node(
    generator: &mut impl Generator,
    node: &Box<ASTnode>,
) -> Option<usize> {
    // Special cases
    match node.op {
        ASTop::FUNCTION => {
            generate_code_for_function(generator, node);
            return None;
        }
        ASTop::FUNCCALL => {
            return Some(generate_code_for_funccall(generator, node));
        }
        ASTop::MEMBER => {
            return Some(generate_code_for_struct_member_access(generator, node));
        }
        ASTop::ASSIGN => {
            return Some(generate_code_for_assignation(generator, node));
        }
        _ => {}
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

    let data_size = generator.data_type_to_size(node.data_type);

    match node.op {
        ASTop::INTLIT => Some(generator.load_integer(node.int_value)),
        ASTop::ADD => Some(generator.add(left_reg.unwrap(), right_reg.unwrap())),
        ASTop::MINUS => Some(generator.minus(left_reg.unwrap(), right_reg.unwrap())),
        ASTop::MULTIPLY => Some(generator.multiply(left_reg.unwrap(), right_reg.unwrap())),
        ASTop::DIVIDE => Some(generator.divide(left_reg.unwrap(), right_reg.unwrap(), data_size)),
        ASTop::IDENT => {
            let sym = node.symtable_entry.as_ref().unwrap().borrow();

            match sym.sym_type {
                // Special case since array variables are actually pointers
                SymType::ARRAY(_) => Some(generator.load_addr(&sym)),
                _ => {
                    if node.rvalue {
                        let sym = node.symtable_entry.as_ref().unwrap().borrow();
                        Some(generator.load_var(&sym))
                    } else {
                        // In cases where an IDENT is used as a left-value,
                        // the parent node will take care of what needs to be done.
                        Some(generator.load_addr(&sym))
                    }
                }
            }
        }
        ASTop::DEREF => {
            if node.rvalue {
                Some(generator.load_from_addr(left_reg.unwrap(), data_size))
            } else {
                // In cases where an IDENT is used as a left-value,
                // the parent node will take care of what needs to be done.
                left_reg
            }
        }
        // ASTop::ASSIGN => {
        //     // TODO: Consider putting this in a different function.

        //     let data_size = generator.data_type_to_size(node.right.as_ref().unwrap().data_type);
        //     Some(generator.assign_to_addr(left_reg.unwrap(), right_reg.unwrap(), data_size))

        //     // match &node.left {
        //     //     Some(left) => match left.op {
        //     //         ASTop::IDENT => {
        //     //             let sym = node
        //     //                 .left
        //     //                 .as_ref()
        //     //                 .unwrap()
        //     //                 .symtable_entry
        //     //                 .as_ref()
        //     //                 .unwrap()
        //     //                 .borrow();

        //     //             Some(generator.assign_glob_var(&sym, right_reg.unwrap()))
        //     //         }

        //     //         _ => {
        //     //             let data_size =
        //     //                 generator.data_type_to_size(node.right.as_ref().unwrap().data_type);
        //     //             Some(generator.assign_to_addr(
        //     //                 left_reg.unwrap(),
        //     //                 right_reg.unwrap(),
        //     //                 data_size,
        //     //             ))
        //     //         }
        //     //     },
        //     //     None => {
        //     //         panic!("Left node should not be empty for ASTop::ASSIGN.");
        //     //     }
        //     // }
        // }
        ASTop::ADDR => match &node.left {
            Some(left) => match left.op {
                ASTop::IDENT => {
                    let sym = node
                        .left
                        .as_ref()
                        .unwrap()
                        .symtable_entry
                        .as_ref()
                        .unwrap()
                        .borrow();
                    Some(generator.load_addr(&sym))
                }
                ASTop::OFFSET => left_reg,
                _ => {
                    panic!("Unexpected operand in ASTop::ADDR");
                }
            },
            None => {
                panic!("Left node should not be empty for ASTop::ADDR.");
            }
        },
        ASTop::RETURN => {
            generator.return_from_func(node.label.as_ref()?, left_reg);
            None
        }
        ASTop::STRLIT => {
            // This is safe as we can count on STRLITs to have a string table ID
            Some(generator.load_strlit(node.string_table_id.unwrap()))
        }
        ASTop::OFFSET => {
            // TODO: This uses needlessly many operations, should be optimised
            let data_size = generator.data_type_to_size(node.data_type);
            let data_size_reg = generator.load_integer(data_size as i32);
            let offset_reg = generator.multiply(right_reg.unwrap(), data_size_reg);
            let addr_reg = generator.add(left_reg.unwrap(), offset_reg);
            if node.rvalue {
                Some(generator.load_from_addr(addr_reg, data_size))
            } else {
                Some(addr_reg)
            }
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
            let data_size = generator.data_type_to_size(n.data_type);
            let r = generate_code_for_node(generator, n);
            match r {
                Some(r) => {
                    // Symtable entry should be available here, this shouldn't ever
                    // fail for FUNCPARAMs
                    generator.move_to_position(
                        r,
                        &param_node.symtable_entry.as_ref().unwrap().borrow().posn,
                        data_size,
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

fn generate_code_for_struct_member_access(
    generator: &mut impl Generator,
    node: &Box<ASTnode>,
) -> usize {
    let struct_base_node = node.left.as_ref().unwrap();

    let mut addr_reg = match struct_base_node.op {
        ASTop::IDENT => {
            // Safe as IDENT nodes always have symtable entries
            generator.load_addr(&struct_base_node.symtable_entry.as_ref().unwrap().borrow())
        }
        ASTop::MEMBER => {
            todo!("support chaining dot operator for struct member access");
        }
        _ => {
            panic!(
                "Unexpected operand type {} for ASTop::MEMBER",
                struct_base_node.op.name()
            );
        }
    };

    // MEMBER nodes always have symtable entries, hence this is safe
    let member_node = node.symtable_entry.as_ref().unwrap().borrow();

    match member_node.posn {
        SymPosition::StructBaseOffset(i) => {
            // TODO: Optimise this extra step of loading the integer literal
            let offset_reg = generator.load_integer(i);
            addr_reg = generator.add(addr_reg, offset_reg);
        }
        _ => {
            panic!("Invalid position for member of struct");
        }
    };

    if node.rvalue {
        generator.load_from_addr(addr_reg, generator.data_type_to_size(member_node.data_type))
    } else {
        addr_reg
    }
}

fn generate_code_for_assignation(generator: &mut impl Generator, node: &Box<ASTnode>) -> usize {
    // Generate code for right register first to avoid using up registers unnecessarily
    // before we need them
    let right_reg = match &node.right {
        Some(right) => {
            let r = generate_code_for_node(generator, right);

            if node.op == ASTop::GLUE {
                generator.free_all_registers();
            }

            r
        }
        None => {
            panic!("Right operand is empty for assignation node");
        }
    };

    let left_reg = match &node.left {
        Some(left) => {
            let r = generate_code_for_node(generator, left);

            if node.op == ASTop::GLUE {
                generator.free_all_registers();
            }

            r
        }
        None => {
            panic!("Left operand is empty for assignation node");
        }
    };

    // Data size should correspond to that of the destination
    // TODO: Should we consider the size of the rvalue as well?
    let data_size = generator.data_type_to_size(node.left.as_ref().unwrap().data_type);
    generator.assign_to_addr(left_reg.unwrap(), right_reg.unwrap(), data_size)
}
