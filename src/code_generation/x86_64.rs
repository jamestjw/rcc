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
                Register::new(String::from("r10")),
                Register::new(String::from("r11")),
                Register::new(String::from("r12")),
                Register::new(String::from("r13")),
            ],
        }
    }

    fn reg_name(&self, i: usize) -> String {
        self.registers[i].name.to_string()
    }

    fn reg_name_for_size(&self, i: usize, data_size: u8) -> String {
        let mut name = self.registers[i].name.to_string();
        match data_size {
            1 => {
                name.push('b');
            }
            4 => {
                name.push('d');
            }
            8 => {}
            _ => {
                panic!("Invalid size passed to reg_name.")
            }
        }
        name
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
    fn divide(&mut self, r1: usize, r2: usize) -> usize {
        // Move dividend to rax
        self.gen_binary_op(
            "movq",
            Operand::Reg(self.reg_name(r1)),
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

    fn print(&mut self, r1: usize) {
        self.gen_binary_op(
            "movq",
            Operand::Reg(self.reg_name(r1)),
            Operand::Reg("rdi".into()),
        );
        self.gen_unary_op("call", Operand::Func("printint".into()));
        self.free_register(r1);
    }

    fn load_glob_var(&mut self, sym: &SymbolTableEntry) -> usize {
        let r = self.alloc_register();
        // TODO: Investigate how relevant it is to use the right
        // instruction here. For now this doesn't work since we
        // don't know the size of the first operand
        // let op_name = match sym.size {
        //     1 => "movzbq",
        //     4 => "movslq",
        //     8 => "movq",
        //     _ => panic!("Invalid size in assign_glob_var."),
        // };
        let op_name = "movq";
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
        let printint_code = r#"
    .text
.LCprintint:
    .string "%d\n"
printint:
    pushq   %rbp
    movq    %rsp, %rbp
    subq    $16, %rsp
    movl    %edi, -4(%rbp)
    movl    -4(%rbp), %eax
    movl    %eax, %esi
    leaq    .LCprintint(%rip), %rdi
    movl    $0, %eax
    call    printf@PLT
    nop
    leave
    ret
"#;
        self.output_str.push_str(printint_code);
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
    fn funccall(&mut self, fn_name: &str, r: Option<usize>) -> usize {
        if let Some(r) = r {
            self.gen_binary_op(
                "movq",
                Operand::Reg(self.reg_name(r)),
                Operand::Reg("rdi".into()),
            );
        }

        self.gen_unary_op("call", Operand::Func(fn_name.to_string()));

        // If a register was passed in, reuse it. Otherwise we allocate a new one.
        let out_reg = match r {
            Some(r) => r,
            None => self.alloc_register(),
        };

        self.gen_binary_op(
            "movq",
            Operand::Reg("rax".into()),
            Operand::Reg(self.reg_name(out_reg)),
        );
        return out_reg;
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
                                    node.borrow_mut().posn = SymPosition::BPOffset(param_offset);
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
}

fn sym_posn_to_string(posn: &SymPosition) -> String {
    match posn {
        SymPosition::Reg(s) => format!("%{}", s),
        SymPosition::BPOffset(i) => format!("{}(%rbp)", i),
        SymPosition::Label(s) => format!("{}(%rip)", s),
        SymPosition::TBD => {
            panic!("Position of symbol undetermined.");
        }
    }
}
