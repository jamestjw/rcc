// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.

use std::cell::RefCell;
use std::convert::TryFrom;
use std::rc::Rc;

use crate::enum_str;
use crate::token::TokenType;

// TODO: Is there a better way to handle pointers?
// Handling multiple levels of indirection will be complicated otherwise.
enum_str! {
    #[derive(Debug, PartialEq, Copy, Clone)]
    pub enum DataType {
        NONE, // To be used there is no specific type
        INT,
        INTPTR,
        CHAR,
        CHARPTR,
        VOID,
    }
}

impl TryFrom<TokenType> for DataType {
    type Error = String;

    fn try_from(token_type: TokenType) -> Result<Self, Self::Error> {
        match token_type {
            TokenType::CHAR => Ok(DataType::CHAR),
            TokenType::INT => Ok(DataType::INT),
            TokenType::VOID => Ok(DataType::VOID),
            _ => Err(format!(
                "Unable to convert {} token to type.",
                token_type.name()
            )),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum SymType {
    VARIABLE,
    FUNCTION,
}

#[derive(Debug, PartialEq)]
pub enum SymClass {
    GLOBAL,
    PARAM,
}

#[derive(Debug)]
pub enum SymPosition {
    // If a symbol can be expected to be found in a register, e.g. func params
    Reg(String),
    // If a symbol can be expected to be found at some offset from the base pointer,
    // e.g. func params and local vars
    PositiveBPOffset(i8),
    // If a symbol can be expected to be found at some label, e.g. global vars
    Label(String),
    // When position of a symbol is not yet determined
    TBD,
}

// TODO: Is there any way to reduce the number of possibly useless fields?
// i.e. initial_value is irrelevant if sym_type == FUNCTION
#[derive(Debug)]
pub struct SymbolTableEntry {
    pub data_type: DataType,
    pub initial_value: i32, // Initial value for ints
    pub name: String,
    pub size: u8, // Size of symbol, i.e. sizeof(name). We will also use this to represent the size to allocate on the stack for the function.
    pub sym_type: SymType,
    pub posn: SymPosition,                           // Where to access symbol
    pub next: Option<Rc<RefCell<SymbolTableEntry>>>, // Next entry in the list, currently only used with members
    // TODO: Investigate if it was right to make this a linked list instead of a HashMap
    pub members: Option<Rc<RefCell<SymbolTableEntry>>>, // Parameters and arguments in a function
    pub member_count: u8,
    pub sym_class: SymClass,
}

impl SymbolTableEntry {
    pub fn new(
        data_type: DataType,
        initial_value: i32,
        name: String,
        size: u8,
        sym_type: SymType,
        sym_class: SymClass,
    ) -> SymbolTableEntry {
        SymbolTableEntry {
            data_type,
            initial_value,
            name,
            size,
            sym_type,
            posn: SymPosition::TBD,
            next: None,
            members: None,
            member_count: 0,
            sym_class,
        }
    }

    pub fn add_member(&mut self, new_member: SymbolTableEntry) {
        match &self.members {
            Some(mem) => {
                mem.borrow_mut().add_to_list(new_member);
            }
            None => {
                self.members = Some(Rc::new(RefCell::new(new_member)));
            }
        }
    }

    pub fn add_to_list(&mut self, new_entry: SymbolTableEntry) {
        match &self.next {
            Some(entry) => {
                entry.borrow_mut().add_to_list(new_entry);
            }
            None => {
                self.next = Some(Rc::new(RefCell::new(new_entry)));
            }
        }
    }

    pub fn search_member(&self, name: &str) -> Option<Rc<RefCell<SymbolTableEntry>>> {
        if self.members.is_none() {
            return None;
        }

        let mut p = Some(Rc::clone(self.members.as_ref().unwrap()));
        loop {
            let node = match p {
                None => break,
                Some(ref n) => Rc::clone(n),
            };

            if node.borrow().name == name {
                return Some(node);
            }

            p = match node.borrow().next {
                None => None,
                Some(ref next) => Some(Rc::clone(next)),
            };
        }
        return None;
    }
}
