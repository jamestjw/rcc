// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.

pub mod code_generation;
pub mod debug;
pub mod parser;
pub mod scanner;
pub mod string_table;
pub mod token;

use crate::parser::{DataType, SymClass, SymType, SymbolTableEntry};
use code_generation::Generator;
use std::path::Path;
use std::process::Command;

#[macro_export]
macro_rules! enum_str {
    (#[derive($($trait:ident),*)]
    pub enum $name:ident {
        $($variant:ident),*,
    }) => {
        #[derive($($trait),*)]
        pub enum $name {
            $($variant),*
        }

        impl $name {
            pub fn name(&self) -> &'static str {
                match self {
                    $($name::$variant => stringify!($variant)),*
                }
            }
        }
    };
}

pub fn compile(input_fname: &Path, output_fname: &Path) -> Result<(), String> {
    let mut scanner = match scanner::Scanner::new(input_fname) {
        Ok(scanner) => scanner,
        Err(err) => {
            return Err(format!("Could not load input file:\n{}", err));
        }
    };

    let mut parser = match parser::Parser::new(&mut scanner) {
        Ok(parser) => parser,
        Err(err) => {
            return Err(format!("Could not initialise parser:\n{}", err));
        }
    };
    // TODO: Remove this after linking standard library
    // We are manually adding the function symbol to allow us to
    // avoid parsing errors
    let printint_sym = parser.add_global_symbol(
        "printint".to_string(),
        DataType::VOID,
        0,
        SymType::FUNCTION,
        0,
    );
    printint_sym.borrow_mut().add_member(SymbolTableEntry::new(
        DataType::INT,
        0,
        "x".to_string(),
        0,
        SymType::VARIABLE,
        SymClass::PARAM,
    ));

    let printchar_sym = parser.add_global_symbol(
        "printchar".to_string(),
        DataType::VOID,
        0,
        SymType::FUNCTION,
        0,
    );
    printchar_sym.borrow_mut().add_member(SymbolTableEntry::new(
        DataType::CHAR,
        0,
        "c".to_string(),
        0,
        SymType::VARIABLE,
        SymClass::PARAM,
    ));

    let printstr_sym = parser.add_global_symbol(
        "printstr".to_string(),
        DataType::VOID,
        0,
        SymType::FUNCTION,
        0,
    );
    printstr_sym.borrow_mut().add_member(SymbolTableEntry::new(
        DataType::CHARPTR,
        0,
        "s".to_string(),
        0,
        SymType::VARIABLE,
        SymClass::PARAM,
    ));

    let stmts = match parser.parse_global_declarations() {
        Ok(stmt) => stmt,
        Err(err) => {
            return Err(format!(
                "Error on line {} in {}:\n{}",
                scanner.line_number,
                input_fname.display(),
                err
            ));
        }
    };

    // crate::debug::print_tree(&stmts, 0);

    let mut generator = code_generation::x86_64::Generator_x86_64::new();
    generator.preprocess_symbols(&parser.global_symbol_table);
    generator.gen_glob_syms(&parser.global_symbol_table);
    generator.gen_strlits(&scanner.string_table);
    generator.preamble();
    code_generation::generate_code_for_node(&mut generator, &stmts);

    if let Err(err) = generator.generate_output(output_fname) {
        return Err(format!("Failed to generate output:\n{}", err));
    }
    Ok(())
}

// TODO: Support multiple input files
pub fn assemble_and_link(input_fname: &Path, output_fname: &Path) {
    let mut assemble = Command::new("cc");
    println!("{:?} {:?}", input_fname, output_fname);
    assemble
        .arg("-o")
        .arg(output_fname)
        .arg(input_fname)
        .arg("src/extras/print.c");
    assemble
        .status()
        .expect("Failed to execute assembler and linker");
}
