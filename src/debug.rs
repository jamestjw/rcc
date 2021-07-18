// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.
use std::rc::Rc;

use crate::parser::{ASTnode, ASTop, SymbolTable, SymbolTableEntry};

pub fn print_tree(node: &ASTnode, indentation_count: u8) {
    print!("{}{}", " ".repeat(indentation_count.into()), node.op.name());

    match node.op {
        ASTop::INTLIT => {
            println!(": {}", node.int_value);
        }
        ASTop::DEREF => {
            println!(" rvalue:{}", node.rvalue);
        }
        ASTop::ADDR => {
            println!(" rvalue:{}", node.rvalue);
        }
        ASTop::IDENT => {
            println!(
                ": {} rvalue:{}",
                node.symtable_entry.as_ref().unwrap().borrow().name,
                node.rvalue
            );
        }
        ASTop::FUNCTION => {
            println!(": {}", node.symtable_entry.as_ref().unwrap().borrow().name,);
        }
        _ => {
            println!("");
        }
    }

    if let Some(left) = &node.left {
        print_tree(left.as_ref(), indentation_count + 2);
    }

    if let Some(right) = &node.right {
        print_tree(right.as_ref(), indentation_count + 2);
    }
}

pub fn print_symbol_table(symtable: &SymbolTable, indentation_count: u8) {
    println!("Symbol table:");
    for (_, entry) in &symtable.table {
        let entry = entry.borrow();
        print_symbol_table_entry(&entry, indentation_count);
        // Print members
        let mut member_node = match &entry.members {
            Some(mem) => Some(Rc::clone(mem)),
            None => None,
        };
        loop {
            let n = match member_node {
                None => {
                    break;
                }
                Some(ref n) => Rc::clone(n),
            };

            print_symbol_table_entry(&n.borrow(), indentation_count + 2);

            member_node = match n.borrow().next {
                None => None,
                Some(ref next) => Some(Rc::clone(next)),
            };
        }
    }
}

pub fn print_symbol_table_entry(entry: &SymbolTableEntry, indentation_count: u8) {
    println!(
        "{}{} {} {} DataType: {}",
        " ".repeat(indentation_count.into()),
        entry.sym_class.name(),
        entry.name,
        entry.sym_type.name(),
        entry.data_type.name()
    );
}
