// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.

pub mod code_generation;
pub mod debug;
pub mod parser;
pub mod scanner;
pub mod token;

use std::process::Command;

use code_generation::Generator;
use debug::print_tree;

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

pub fn compile(input_fname: &str, output_fname: &str) -> Result<(), String> {
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

    let stmts = match parser.parse_global_declarations() {
        Ok(stmt) => stmt,
        Err(err) => {
            return Err(format!(
                "Failed to parse statements on line {}:\n{}",
                scanner.line_number, err
            ));
        }
    };

    print_tree(&stmts, 0);

    let mut generator = code_generation::x86_64::Generator_x86_64::new();
    generator.gen_glob_syms(parser.global_symbol_table);
    generator.preamble();
    code_generation::generate_code_for_node(&mut generator, &stmts);

    if let Err(err) = generator.generate_output(output_fname) {
        return Err(format!("Failed to generate output:\n{}", err));
    }
    Ok(())
}

// TODO: Support multiple input files
pub fn assemble_and_link(input_fname: &str, output_fname: &str) {
    let mut assemble = Command::new("cc");
    assemble.arg("-o").arg(output_fname).arg(input_fname);
    assemble
        .status()
        .expect("Failed to execute assembler and linker");
}
