// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.

extern crate exitcode;

use std::env;
use std::process;

use rcc::debug;
use rcc::parser::Parser;
use rcc::scanner::Scanner;

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

    let mut scanner = Scanner::new(input_fname).unwrap_or_else(|err| {
        eprintln!("Could not load input file:\n{}", err);
        process::exit(exitcode::DATAERR);
    });

    let mut parser = Parser::new(&mut scanner).unwrap_or_else(|err| {
        eprintln!("Could not initialise parser:\n{}", err);
        process::exit(exitcode::DATAERR);
    });

    let print_statement = parser.print_statement().unwrap_or_else(|err| {
        eprintln!("Failed to parse print statement:\n{}", err);
        process::exit(exitcode::DATAERR);
    });

    debug::print_tree(print_statement.as_ref(), 0);
}
