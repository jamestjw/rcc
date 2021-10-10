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
        STRLIT,
        IDENT,
        ASSIGN,
        NOOP,
        GLUE,
        FUNCTION,
        RETURN,
        FUNCCALL,
        FUNCPARAM,
        DEREF,
        ADDR,
        OFFSET,
        MEMBER, // Access member of struct
        PTRMEMBER, // Access member of struct via a pointer
        ENUM,
        IF,
        WHILE,
        FOR,
        SWITCH,
        CASE,
        DEFAULT,
        CONTINUE,
        BREAK,
        UNARYMINUS,
        EQ,
        NOTEQ,
        GTEQ,
        GT,
        LTEQ,
        LT,
    }
}

#[derive(Debug)]
pub struct ASTnode {
    pub op: ASTop,
    pub left: Option<Box<ASTnode>>,
    pub right: Option<Box<ASTnode>>,
    pub int_value: i32,
    pub symtable_entry: Option<Rc<RefCell<SymbolTableEntry>>>,
    pub rvalue: bool,
    pub label: Option<String>,
    // The type of data contained in this node
    // DataType::NONE if this node yields nothing
    pub data_type: DataType,
    pub type_sym: Option<Rc<RefCell<SymbolTableEntry>>>,
    // ID to a string table entry for string literals
    pub string_table_id: Option<usize>,
}

impl ASTnode {
    pub fn new_leaf(op: ASTop, int_value: i32, data_type: DataType) -> Box<ASTnode> {
        Box::new(ASTnode {
            op: op,
            left: None,
            right: None,
            int_value,
            symtable_entry: None,
            rvalue: false,
            label: None,
            data_type,
            string_table_id: None,
            type_sym: None,
        })
    }

    pub fn new_unary(op: ASTop, left: Box<ASTnode>, data_type: DataType) -> Box<ASTnode> {
        Box::new(ASTnode {
            op,
            left: Some(left),
            right: None,
            int_value: 0,
            symtable_entry: None,
            rvalue: false,
            label: None,
            data_type,
            string_table_id: None,
            type_sym: None,
        })
    }

    pub fn new_right_unary(op: ASTop, right: Box<ASTnode>, data_type: DataType) -> Box<ASTnode> {
        Box::new(ASTnode {
            op,
            left: None,
            right: Some(right),
            int_value: 0,
            symtable_entry: None,
            rvalue: false,
            label: None,
            data_type,
            string_table_id: None,
            type_sym: None,
        })
    }

    pub fn new_boxed(
        op: ASTop,
        left: Box<ASTnode>,
        right: Box<ASTnode>,
        data_type: DataType,
    ) -> Box<ASTnode> {
        Box::new(ASTnode {
            op,
            left: Some(left),
            right: Some(right),
            int_value: 0,
            symtable_entry: None,
            rvalue: false,
            label: None,
            data_type,
            string_table_id: None,
            type_sym: None,
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
            data_type: DataType::NONE,
            string_table_id: None,
            type_sym: None,
        })
    }
}
