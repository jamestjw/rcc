use glob::glob;
use rcc::{assemble_and_link, compile};
use regex::Regex;

use std::fs;
use std::process::Command;
use std::str;

#[test]
fn generate_correct_assembly() {
    // TODO: Determine if we will handle checking for errors this way as well,
    // if not we can ditch the `-ok` in the filename
    // e.g. input filename is of the format input001-ok.c
    let re = Regex::new(r"input(\d+)-ok.c").unwrap();

    for test_file in glob("./tests/input/input*.c").expect("Failed to read glob pattern") {
        match test_file {
            Ok(path) => {
                // This is safe based on our glob pattern
                let test_file_name = path.file_name().unwrap().to_str().unwrap();
                print!("Testing {}", test_file_name);

                let mut file_id = String::new();
                for cap in re.captures_iter(test_file_name) {
                    file_id.push_str(&cap[1]);
                }

                // Path of file containing expected results of executing the input file
                let res_filename = format!("input{}.exp", file_id);
                let mut res_path = path.parent().unwrap().to_path_buf();
                res_path.push(res_filename);
                let res_path_str = res_path.to_str().unwrap();

                // Path of executable to be created from input test file
                let exec_path = res_path.with_extension("out");
                let exec_path_str = exec_path.to_str().unwrap();

                // Output assembly path is the same as original file with different extension
                let mut output_asm_path = path.clone();
                output_asm_path.set_extension("s");
                let output_asm_path_str = output_asm_path.to_str().unwrap();

                if let Err(e) = compile(path.to_str().unwrap(), output_asm_path_str) {
                    panic!("Failed to compile {:?} with error {}", path, e);
                }

                assemble_and_link(output_asm_path_str, exec_path_str);

                if let Err(e) = fs::remove_file(output_asm_path_str) {
                    panic!("Failed to remove {} with error: {}", output_asm_path_str, e);
                }

                let actual_res = execute_file(exec_path_str);
                let expected_res = fs::read_to_string(res_path_str).unwrap();

                if let Err(e) = fs::remove_file(exec_path_str) {
                    panic!("Failed to remove {} with error: {}", exec_path_str, e);
                }

                if actual_res != expected_res {
                    println!("... [failed]");
                    panic!("Output of {} did not match expectations.", test_file_name);
                } else {
                    println!("... [done]");
                }
            }
            Err(e) => println!("{:?}", e),
        }
    }
}

// Return a String containing what was written to stdout during execution of a file
// TODO: Consider making this return a result and move it to lib if we need it elsewhere,
// otherwise it is probably find to retain the calls to expect() and panic!() here.
pub fn execute_file(input_fname: &str) -> String {
    let mut exec_res = String::new();
    let output = Command::new(input_fname)
        .output()
        .expect(&format!("failed to execute {}", input_fname));

    exec_res.push_str(match str::from_utf8(&output.stdout) {
        Ok(val) => val,
        Err(_) => panic!("Got non UTF-8 data from execution of test file"),
    });

    exec_res
}
