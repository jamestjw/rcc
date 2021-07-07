use glob::glob;
use rcc::{assemble_and_link, compile};
use regex::Regex;

use std::fs;
use std::path::Path;
use std::process::Command;
use std::str;

#[test]
fn generate_correct_assembly() {
    // TODO: Determine if we will handle checking for errors this way as well,
    // if not we can ditch the `-ok` in the filename
    // e.g. input filename is of the format input001-ok.c
    let re = Regex::new(r"input(?P<id>\d+)-(?P<status>ok|err).c").unwrap();

    for test_file in glob("./tests/input/input*.c").expect("Failed to read glob pattern") {
        match test_file {
            Ok(test_file) => {
                // This is safe based on our glob pattern
                let test_file_name = test_file.file_name().unwrap().to_str().unwrap();
                print!("Testing {}\n", test_file_name);

                let mut file_id = String::new();
                let mut status = String::new();
                let cap = re.captures(test_file_name).unwrap_or_else(|| {
                    panic!(
                        "File: {} does not match expected filename format",
                        test_file_name
                    )
                });
                file_id.push_str(&cap["id"]);
                status.push_str(&cap["status"]);

                // Path of file containing expected results of executing the input file
                let res_filename = format!("input{}.exp", file_id);
                let mut res_path = test_file.parent().unwrap().to_path_buf();
                res_path.push(res_filename);

                // Path of executable to be created from input test file
                let exec_path = res_path.with_extension("out");

                // Output assembly path is the same as original file with different extension
                let mut output_asm_path = test_file.clone();
                output_asm_path.set_extension("s");

                match compile(&test_file, &output_asm_path) {
                    Ok(_) => {
                        if status == "err" {
                            panic!("Compilation should have failed for {}", test_file_name);
                        }
                        assemble_and_link(&output_asm_path, &exec_path);

                        if let Err(e) = fs::remove_file(output_asm_path.as_path()) {
                            panic!(
                                "Failed to remove {} with error: {}",
                                output_asm_path.display(),
                                e
                            );
                        }

                        let actual_res = execute_file(&exec_path);
                        let expected_res = fs::read_to_string(res_path).unwrap();

                        if let Err(e) = fs::remove_file(exec_path.as_path()) {
                            panic!("Failed to remove {} with error: {}", exec_path.display(), e);
                        }

                        if actual_res != expected_res {
                            println!("******* [FAILED] *******");
                            println!("Expected:\n{}", expected_res);
                            println!("Actual:\n{}", actual_res);
                            panic!("Output of {} did not match expectations.", test_file_name);
                        } else {
                            println!("******* [PASSED] *******");
                        }
                    }
                    Err(e) => {
                        if status == "ok" {
                            panic!("Failed to compile {:?} with error {}", test_file, e);
                        }
                        let expected_res = fs::read_to_string(res_path.as_path()).unwrap();
                        if expected_res != e {
                            println!("******* [FAILED] *******");
                            println!("Expected:\n{}", expected_res);
                            println!("Actual:\n{}", e);
                            panic!(
                                "Error message does not match expectations for {}",
                                res_path.display()
                            );
                        } else {
                            println!("******* [PASSED] *******");
                        }
                    }
                }
            }
            Err(e) => println!("{:?}", e),
        }
    }
}

// Return a String containing what was written to stdout during execution of a file
// TODO: Consider making this return a result and move it to lib if we need it elsewhere,
// otherwise it is probably find to retain the calls to expect() and panic!() here.
pub fn execute_file(input_fname: &Path) -> String {
    let mut exec_res = String::new();
    let output = Command::new(input_fname)
        .output()
        .expect(&format!("failed to execute {}", input_fname.display()));

    exec_res.push_str(match str::from_utf8(&output.stdout) {
        Ok(val) => val,
        Err(_) => panic!("Got non UTF-8 data from execution of test file"),
    });

    exec_res
}
