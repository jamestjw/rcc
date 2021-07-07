// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.

use super::*;
use crate::enum_str;
use std::rc::Rc;

enum_str! {
    #[derive(Debug, PartialEq, Copy, Clone)]
    pub enum ASTop {
        ADD,
        MINUS,
        MULTIPLY,
        DIVIDE,
        INTLIT,
        IDENT,
        PRINT,
        ASSIGN,
        NOOP,
        GLUE,
        FUNCTION,
        RETURN,
        FUNCCALL,
    }

}

pub struct ASTnode {
    pub op: ASTop,
    pub left: Option<Box<ASTnode>>,
    pub right: Option<Box<ASTnode>>,
    pub int_value: i32,
    pub symtable_entry: Option<Rc<SymbolTableEntry>>,
    pub rvalue: bool,
    pub label: Option<String>,
}

impl ASTnode {
    pub fn new_leaf(op: ASTop, int_value: i32) -> Box<ASTnode> {
        Box::new(ASTnode {
            op: op,
            left: None,
            right: None,
            int_value,
            symtable_entry: None,
            rvalue: false,
            label: None,
        })
    }

    pub fn new_unary(op: ASTop, left: Box<ASTnode>) -> Box<ASTnode> {
        Box::new(ASTnode {
            op,
            left: Some(left),
            right: None,
            int_value: 0,
            symtable_entry: None,
            rvalue: false,
            label: None,
        })
    }

    pub fn new_boxed(op: ASTop, left: Box<ASTnode>, right: Box<ASTnode>) -> Box<ASTnode> {
        Box::new(ASTnode {
            op,
            left: Some(left),
            right: Some(right),
            int_value: 0,
            symtable_entry: None,
            rvalue: false,
            label: None,
        })
    }

    pub fn new_noop() -> Box<ASTnode> {
        Box::new(ASTnode {
            op: ASTop::NOOP,
            left: None,
            right: None,
            int_value: 0,
            symtable_entry: None,
            rvalue: false,
            label: None,
        })
    }
}
