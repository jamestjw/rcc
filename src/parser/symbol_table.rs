// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.

#[derive(Debug, PartialEq)]
pub enum DataType {
    INT,
}

#[derive(Debug, PartialEq)]
pub enum SymType {
    VARIABLE,
    FUNCTION,
}

// TODO: Is there any way to reduce the number of possibly useless fields?
// i.e. initial_value is irrelevant if sym_type == FUNCTION
#[derive(Debug)]
pub struct SymbolTableEntry {
    pub data_type: DataType,
    pub initial_value: i32, // Initial value for ints
    pub name: String,
    pub size: u8, // Size of symbol, i.e. sizeof(name)
    pub sym_type: SymType,
}
