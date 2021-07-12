// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.

use super::*;
use crate::parser::{SymPosition, SymType};
use std::error::Error;
use std::fmt;
use std::fs;
use std::io::Write;
use std::path::Path;

// Registers to assign params to
const PARAM_REGS: [&'static str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

enum Operand {
    Int(i32),
    Reg(String),
    Func(String),
    RawString(String),
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Int(i) => write!(f, "${}", i),
            Operand::Reg(s) => write!(f, "%{}", s),
            Operand::Func(s) => write!(f, "{}@PLT", s),
            Operand::RawString(s) => write!(f, "{}", s),
        }
    }
}

#[allow(non_camel_case_types)]
pub struct Generator_x86_64 {
    output_str: String,
    registers: Vec<Register>,
}

impl Generator_x86_64 {
    pub fn new() -> Generator_x86_64 {
        Self {
            output_str: String::new(),
            // General purpose registers available
            // for x86-64
            // TODO: Support more registers
            registers: vec![
                Register::new(
                    String::from("r10"),
                    String::from("r10d"),
                    String::from("r10b"),
                ),
                Register::new(
                    String::from("r11"),
                    String::from("r11d"),
                    String::from("r11b"),
                ),
                Register::new(
                    String::from("r12"),
                    String::from("r12d"),
                    String::from("r12b"),
                ),
                Register::new(
                    String::from("r13"),
                    String::from("r13d"),
                    String::from("r13b"),
                ),
            ],
        }
    }

    fn reg_name(&self, i: usize) -> String {
        self.registers[i].quad_name.clone()
    }

    fn reg_name_for_size(&self, i: usize, data_size: u8) -> String {
        match data_size {
            1 => self.registers[i].byte_name.clone(),
            4 => self.registers[i].long_name.clone(),
            8 => self.registers[i].quad_name.clone(),
            _ => {
                panic!("Invalid size passed to reg_name.")
            }
        }
    }

    fn gen_binary_op(&mut self, op_str: &str, op1: Operand, op2: Operand) {
        self.output_str
            .push_str(&format!("\t{}\t{}, {}\n", op_str, op1, op2));
    }

    fn gen_unary_op(&mut self, op_str: &str, op: Operand) {
        self.output_str.push_str(&format!("\t{}\t{}\n", op_str, op));
    }

    fn gen_op(&mut self, op_str: &str) {
        self.output_str.push_str(&format!("\t{}\n", op_str));
    }
}

impl Generator for Generator_x86_64 {
    fn alloc_register(&mut self) -> usize {
        for i in 0..self.registers.len() {
            if self.registers[i].free {
                self.registers[i].free = false;
                return i;
            }
        }

        panic!("No free registers are available.")
    }

    fn free_register(&mut self, i: usize) {
        self.registers[i].free()
    }

    fn free_all_registers(&mut self) {
        for i in 0..self.registers.len() {
            self.registers[i].free = true;
        }
    }

    fn load_integer(&mut self, i: i32) -> usize {
        let r = self.alloc_register();
        self.gen_binary_op("movq", Operand::Int(i), Operand::Reg(self.reg_name(r)));

        r
    }

    fn add(&mut self, r1: usize, r2: usize) -> usize {
        self.gen_binary_op(
            "addq",
            Operand::Reg(self.reg_name(r2)),
            Operand::Reg(self.reg_name(r1)),
        );
        self.free_register(r2);
        r1
    }

    fn minus(&mut self, r1: usize, r2: usize) -> usize {
        self.gen_binary_op(
            "subq",
            Operand::Reg(self.reg_name(r2)),
            Operand::Reg(self.reg_name(r1)),
        );
        self.free_register(r2);
        r1
    }

    fn multiply(&mut self, r1: usize, r2: usize) -> usize {
        self.gen_binary_op(
            "imulq",
            Operand::Reg(self.reg_name(r2)),
            Operand::Reg(self.reg_name(r1)),
        );
        self.free_register(r2);
        r1
    }

    fn divide(&mut self, r1: usize, r2: usize, data_size: u8) -> usize {
        // Move dividend to rax
        self.gen_binary_op(
            move_signex_op(data_size),
            Operand::Reg(self.reg_name_for_size(r1, data_size)),
            Operand::Reg("rax".into()),
        );

        // Sign extend dividend to 8 bytes
        self.gen_op("cqo");

        // Divide dividend in rax with divisor in r2
        self.gen_unary_op("idivq", Operand::Reg(self.reg_name(r2)));

        // Move result from rax to r1
        self.gen_binary_op(
            "movq",
            Operand::Reg("rax".into()),
            Operand::Reg(self.reg_name(r1)),
        );

        self.free_register(r2);
        r1
    }

    fn load_var(&mut self, sym: &SymbolTableEntry) -> usize {
        let r = self.alloc_register();
        let op_name = move_signex_op(8);
        self.gen_binary_op(
            op_name,
            Operand::RawString(sym_posn_to_string(&sym.posn)),
            Operand::Reg(self.reg_name(r)),
        );
        r
    }

    fn assign_glob_var(&mut self, sym: &SymbolTableEntry, r: usize) -> usize {
        let op_name = match sym.size {
            1 => "movb",
            4 => "movl",
            8 => "movq",
            _ => panic!("Invalid size in assign_glob_var."),
        };
        self.gen_binary_op(
            op_name,
            Operand::Reg(self.reg_name_for_size(r, sym.size)),
            Operand::RawString(sym_posn_to_string(&sym.posn)),
        );
        r
    }

    fn gen_glob_syms(&mut self, symtable: &HashMap<String, Rc<RefCell<SymbolTableEntry>>>) {
        for (sym_name, entry) in symtable {
            if entry.borrow().sym_type != SymType::VARIABLE {
                continue;
            }
            self.output_str.push_str(&format!(
                r#"    .globl  {0}
    .bss
    .align 4
    .type   {0}, @object
    .size   {0}, {1}
{0}:
    .zero   {1}
"#,
                sym_name,
                entry.borrow().size
            ));
        }
    }

    fn preamble(&mut self) {
        // For now we assume that everything after the preamble falls in the
        // text section.
        self.output_str.push_str("\t.text\n");
    }

    fn postamble(&mut self) {}

    fn func_preamble(&mut self, fn_name: &str) {
        self.output_str.push_str(&format!(
            r#"
    .globl  {0}
    .type   {0},   @function
{0}:
    pushq   %rbp
    movq    %rsp, %rbp
"#,
            fn_name
        ));
    }

    fn func_postamble(&mut self, end_label: &str) {
        self.output_str.push_str(&format!(
            r#"{}:
    popq    %rbp
    ret
"#,
            end_label
        ));
    }

    fn generate_output(&mut self, output_filename: &Path) -> Result<(), Box<dyn Error>> {
        self.postamble();

        let mut f = fs::OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(output_filename)?;
        f.write_all(self.output_str.as_bytes())?;

        Ok(())
    }

    fn return_from_func(&mut self, label: &str, r: Option<usize>) {
        if let Some(r) = r {
            self.gen_binary_op(
                "movq",
                Operand::Reg(self.reg_name(r)),
                Operand::Reg("rax".into()),
            );
            self.free_register(r);
        }
        self.output_str.push_str(&format!("\tjmp {}\n", label));
    }

    // TODO: Do we need to copy return val to a register if no one intends
    // use it? Especially in the case of void functions.
    fn funccall(&mut self, fn_name: &str, num_params: usize) -> usize {
        let r = self.alloc_register();

        self.gen_unary_op("call", Operand::Func(fn_name.to_string()));

        if num_params > PARAM_REGS.len() {
            for _ in 0..(num_params - PARAM_REGS.len()) {
                // Temporarily use this register to pop out args
                self.gen_unary_op("popq", Operand::Reg(self.reg_name(r)));
            }
        }
        self.gen_binary_op(
            "movq",
            Operand::Reg("rax".into()),
            Operand::Reg(self.reg_name(r)),
        );
        return r;
    }

    fn set_sym_positions(&mut self, symtable: &HashMap<String, Rc<RefCell<SymbolTableEntry>>>) {
        for (sym_name, entry) in symtable {
            let mut entry = entry.borrow_mut();
            match entry.sym_type {
                SymType::VARIABLE => {
                    // Global variables can be referred directly to by their names
                    entry.posn = SymPosition::Label(sym_name.clone());
                }
                SymType::FUNCTION => {
                    // Functions can be referred directly to by their names
                    entry.posn = SymPosition::Label(sym_name.clone());

                    // Assign function params to registers or stack offsets
                    let mut member_node = match &entry.members {
                        Some(n) => Some(Rc::clone(n)),
                        None => None,
                    };
                    let mut i = 0;
                    // Offset of next param on the stack (16 because of pushed rbp and the retaddr)
                    let mut param_offset = 16;

                    loop {
                        match member_node {
                            Some(node) => {
                                if i < PARAM_REGS.len() {
                                    node.borrow_mut().posn =
                                        SymPosition::Reg(PARAM_REGS[i].to_string());
                                } else {
                                    node.borrow_mut().posn =
                                        SymPosition::PositiveBPOffset(param_offset);
                                    param_offset += 8;
                                }
                                i += 1;
                                if node.borrow().next.is_some() {
                                    member_node =
                                        Some(Rc::clone(node.borrow().next.as_ref().unwrap()));
                                } else {
                                    member_node = None;
                                }
                            }
                            None => {
                                break;
                            }
                        }
                    }
                }
            }
        }
    }

    // TODO: The behaviour of this method is only coherent if we invoke this
    // before function calls. Perhaps the name of the method should be changed.
    fn move_to_position(&mut self, r: usize, posn: &SymPosition, data_size: u8) {
        if let SymPosition::PositiveBPOffset(_) = posn {
            // TODO: Push with right datasize
            self.gen_unary_op("pushq", Operand::Reg(self.reg_name(r)));
        } else {
            self.gen_binary_op(
                move_signex_op(data_size),
                Operand::Reg(self.reg_name_for_size(r, data_size)),
                Operand::RawString(sym_posn_to_string(posn)),
            );
        }

        self.free_register(r);
    }

    fn data_type_to_size(&self, data_type: DataType) -> u8 {
        match data_type {
            DataType::CHAR => 1,
            DataType::INT => 4,
            _ => 0,
        }
    }
}

fn sym_posn_to_string(posn: &SymPosition) -> String {
    match posn {
        SymPosition::Reg(s) => format!("%{}", s),
        SymPosition::PositiveBPOffset(i) => format!("{}(%rbp)", i),
        SymPosition::Label(s) => format!("{}(%rip)", s),
        SymPosition::TBD => {
            panic!("Position of symbol undetermined.");
        }
    }
}

fn move_signex_op(data_size: u8) -> &'static str {
    match data_size {
        1 => "movsbq",
        4 => "movslq",
        8 => "movq",
        _ => {
            panic!("Unknown size: {} in move_signex_op", data_size);
        }
    }
}

// TODO: Seems like this is not necessary for now
// Returns the op name and the size that was matched, some sizes
// default to operators of a different size.
// fn add_op(data_size: u8) -> (&'static str, u8) {
//     match data_size {
//         1..=4 => ("addl", 4),
//         8 => ("addq", 8),
//         _ => {
//             panic!("Unknown size: {} in add_op", data_size);
//         }
//     }
// }

// fn sub_op(data_size: u8) -> (&'static str, u8) {
//     match data_size {
//         1..=4 => ("subl", 4),
//         8 => ("subq", 8),
//         _ => {
//             panic!("Unknown size: {} in sub_op", data_size);
//         }
//     }
// }

// fn mul_op(data_size: u8) -> (&'static str, u8) {
//     match data_size {
//         1..=4 => ("imull", 4),
//         8 => ("imulq", 8),
//         _ => {
//             panic!("Unknown size: {} in mul_op", data_size);
//         }
//     }
// }
