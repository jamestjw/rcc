// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.

use std::cell::RefCell;
use std::collections::HashMap;
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
        VOIDPTR,
        STRUCT,
        STRUCTPTR,
        ENUM,
    }
}

impl DataType {
    pub fn is_pointer(&self) -> bool {
        match self {
            DataType::INTPTR | DataType::CHARPTR | DataType::STRUCTPTR => true,
            _ => false,
        }
    }
}

impl TryFrom<TokenType> for DataType {
    type Error = String;

    fn try_from(token_type: TokenType) -> Result<Self, Self::Error> {
        match token_type {
            TokenType::CHAR => Ok(DataType::CHAR),
            TokenType::INT => Ok(DataType::INT),
            TokenType::VOID => Ok(DataType::VOID),
            TokenType::STRUCT => Ok(DataType::STRUCT),
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
    ARRAY(u16),
    STRUCT,
    ENUM,
    ENUMERATOR,
}

impl SymType {
    pub fn name(&self) -> &'static str {
        match self {
            SymType::VARIABLE => "VARIABLE",
            SymType::FUNCTION => "FUNCTION",
            SymType::ARRAY(_) => "ARRAY",
            SymType::STRUCT => "STRUCT",
            SymType::ENUM => "ENUM",
            SymType::ENUMERATOR => "ENUMERATOR",
        }
    }
}

enum_str! {
    #[derive(Debug, PartialEq)]
    pub enum SymClass {
        GLOBAL,
        PARAM,
        MEMBER,
        STRUCT,
        ENUM,
    }
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
    // Offset from base of a structure
    StructBaseOffset(i32),
    // When position of a symbol is not yet determined
    TBD,
}

pub struct SymbolTable {
    pub table: HashMap<String, Rc<RefCell<SymbolTableEntry>>>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            table: HashMap::new(),
        }
    }

    pub fn add_symbol(
        &mut self,
        lexeme: String,
        data_type: DataType,
        initial_value: i32,
        sym_type: SymType,
        size: u32,
        sym_class: SymClass,
    ) -> Rc<RefCell<SymbolTableEntry>> {
        // TODO: Is there a reasonable way to avoid cloning the lexeme?
        let sym = Rc::new(RefCell::new(SymbolTableEntry::new(
            data_type,
            initial_value,
            lexeme.clone(),
            size,
            sym_type,
            sym_class,
        )));
        self.table.insert(lexeme, Rc::clone(&sym));
        sym
    }

    pub fn find_symbol(
        &self,
        lexeme: &str,
        sym_class: Option<SymClass>,
    ) -> Option<Rc<RefCell<SymbolTableEntry>>> {
        match self.table.get(lexeme) {
            Some(sym) => match sym_class {
                Some(sym_class) => {
                    if sym.borrow().sym_class == sym_class {
                        Some(Rc::clone(sym))
                    } else {
                        None
                    }
                }
                None => Some(Rc::clone(sym)),
            },
            None => None,
        }
    }
}

// TODO: Is there any way to reduce the number of possibly useless fields?
// i.e. initial_value is irrelevant if sym_type == FUNCTION
#[derive(Debug)]
pub struct SymbolTableEntry {
    pub data_type: DataType,
    pub initial_value: i32, // Initial value for ints, also used to contain store value of enumerators
    pub name: String,
    pub size: u32, // Size of symbol, i.e. sizeof(name). We will also use this to represent the size to allocate on the stack for the function.
    pub sym_type: SymType,
    pub posn: SymPosition,                           // Where to access symbol
    pub next: Option<Rc<RefCell<SymbolTableEntry>>>, // Next entry in the list, currently only used with members
    // TODO: Investigate if it was right to make this a linked list instead of a HashMap
    pub members: Option<Rc<RefCell<SymbolTableEntry>>>, // Parameters and arguments in a function/Members of a struct
    pub type_sym: Option<Rc<RefCell<SymbolTableEntry>>>, // Reference to struct symbol for struct variables
    pub sym_class: SymClass,
}

impl SymbolTableEntry {
    pub fn new(
        data_type: DataType,
        initial_value: i32,
        name: String,
        size: u32,
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
            sym_class,
            type_sym: None,
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
