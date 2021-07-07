// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.

extern crate exitcode;

use rcc::compile;
use std::path::PathBuf;
use std::process;

use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(
    name = "rcc",
    about = "A C-compiler written in Rust by James Tan for pedagogical purposes."
)]
struct Opt {
    #[structopt(short, long, help = "Output file path", default_value = "./out.s")]
    outfile: PathBuf,
    #[structopt(
        required = true,
        help = "List of input files to compile (it currently only supports one input file)"
    )]
    infiles: Vec<PathBuf>,
}

fn main() {
    let opt = Opt::from_args();

    let infile = &opt.infiles[0];
    let outfile = &opt.outfile;

    // TODO: Support more than one input file
    match compile(infile, outfile) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("{}", e);
            process::exit(exitcode::DATAERR);
        }
    }
}
