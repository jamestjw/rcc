// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree. 

extern crate exitcode;

use std::env;
use std::process;

use rcc::scanner::Scanner;
use rcc::scanner::token::TokenType;

fn help(prog_name: &str) {
    println!(
        "Usage: {} [option] file [file ...]
Options:
        -h  : print this help message and exit (also --help)",
        prog_name
    );
}

fn main() {
    let args: Vec<String> = env::args().collect();

    // TODO: Make this more robust and accept flags
    if args.len() != 2 {
        eprintln!("Unexpected arguments received");
        help(&args[0]);
        process::exit(exitcode::USAGE);
    }

    // TODO: Handle --help

    let input_fname = &args[1];

    println!("Compiling input file {}", input_fname);

    let scanner = Scanner::new(input_fname).unwrap_or_else(|err| {
        eprintln!("Could not load input file:\n{}", err);
        process::exit(exitcode::DATAERR);
    });

    compile(scanner);
}

fn compile(mut scanner: Scanner) {
    loop {
        match scanner.consume() {
            Ok(tok) => {
                match tok.token_type  {
                    TokenType::EOF => {
                        println!("Reached end of input file.");
                        process::exit(exitcode::OK);
                    },
                    _ => {
                        println!("{:?}", tok);
                    }
                }
            },
            Err(e) => {
                eprintln!("Error scanning tokens:\n{}", e);
                process::exit(exitcode::DATAERR);
            }
        }
    }
}
