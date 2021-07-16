// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.

use crate::parser::{ASTnode, ASTop};

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
