// Copyright (c) 2021, James Tan Juan Whei
// All rights reserved.

// This source code is licensed under the BSD-style license found in the
// LICENSE file in the root directory of this source tree.

use super::*;
use crate::parser::SymType;
use std::error::Error;
use std::fmt;
use std::fs;
use std::io::Write;
use std::path::Path;

// Registers to assign params to
const PARAM_REGS: [&'static str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
const PTR_SIZE: u32 = 8;

enum Operand {
    Int(i32),
    Strlit(String),
    Reg(String),
    Func(String),
    RawString(String),
    Addr(String),
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Int(i) => write!(f, "${}", i),
            Operand::Strlit(i) => write!(f, "{}(%rip)", i),
            Operand::Reg(s) => write!(f, "%{}", s),
            Operand::Func(s) => write!(f, "{}@PLT", s),
            Operand::RawString(s) => write!(f, "{}", s),
            Operand::Addr(s) => write!(f, "(%{})", s),
        }
    }
}

#[allow(non_camel_case_types)]
pub struct Generator_x86_64 {
    output_str: String,
    registers: Vec<Register>,
    label_idx: u32,
    break_stack: Vec<String>,
    continue_stack: Vec<String>,
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
            label_idx: 0,
            break_stack: Vec::new(),
            continue_stack: Vec::new(),
        }
    }

    fn reg_name(&self, i: usize) -> String {
        self.registers[i].quad_name.clone()
    }

    fn reg_name_for_size(&self, i: usize, data_size: u32) -> String {
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

    fn load_strlit(&mut self, id: usize) -> usize {
        let r = self.alloc_register();
        self.gen_binary_op(
            "leaq",
            Operand::Strlit(str_label(id)),
            Operand::Reg(self.reg_name(r)),
        );

        r
    }

    fn gen_strlits(&mut self, string_table: &StringTable) {
        self.output_str.push_str(&format!("\t.section .rodata\n"));
        for id in 0..string_table.len() {
            self.output_str.push_str(&format!(
                "{}:\n\t.string \"{}\"\n",
                str_label(id),
                string_table.get_by_id(id)
            ));
        }
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

    fn divide(&mut self, r1: usize, r2: usize, data_size: u32) -> usize {
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

    fn assign_to_addr(&mut self, r1: usize, r2: usize, data_size: u32) -> usize {
        // let op_name = match data_size {
        //     1 => "movb",
        //     4 => "movl",
        //     8 => "movq",
        //     _ => panic!("Invalid size in assign_to_addr."),
        // };
        let op_name = move_op(data_size);
        self.gen_binary_op(
            op_name,
            Operand::Reg(self.reg_name_for_size(r2, data_size)),
            Operand::Addr(self.reg_name(r1)),
        );
        self.free_register(r2);
        r1
    }

    fn gen_glob_syms(&mut self, symtable: &SymbolTable) {
        for (sym_name, entry) in &symtable.table {
            match entry.borrow().sym_type {
                SymType::VARIABLE => {}
                SymType::ARRAY(_) => {}
                _ => {
                    continue;
                }
            };
            self.output_str
                .push_str(&format!("\t.globl  {0}\n\t.bss\n", sym_name,));

            // TODO: Handle alignment correctly based on the structure.
            let size = entry.borrow().size;

            let align = 2_u32.pow(crate::log_2(size));

            if align >= 32 {
                self.output_str.push_str(&format!("\t.align 32\n"));
            } else if align >= 4 {
                self.output_str.push_str(&format!("\t.align {}\n", align));
            }

            self.output_str.push_str(&format!(
                r#"    .type   {0}, @object
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

        // TODO: Unpop registers we spilled earlier
        for r in PARAM_REGS.iter().rev() {
            self.gen_unary_op("popq", Operand::Reg(r.to_string()));
        }

        self.gen_binary_op(
            "movq",
            Operand::Reg("rax".into()),
            Operand::Reg(self.reg_name(r)),
        );
        return r;
    }

    fn pre_funccall(&mut self) {
        // TODO: Improve this, we should also store registers that are in use
        for r in PARAM_REGS.iter() {
            self.gen_unary_op("pushq", Operand::Reg(r.to_string()));
        }
    }

    fn preprocess_symbols(&mut self, symtable: &SymbolTable) {
        for (sym_name, entry) in &symtable.table {
            let mut entry = entry.borrow_mut();
            match entry.sym_type {
                SymType::VARIABLE | SymType::ENUMERATOR => {
                    // Global variables can be referred directly to by their names
                    entry.posn = SymPosition::Label(sym_name.clone());

                    entry.size = match &entry.type_sym {
                        Some(sym) => {
                            // We only copy the size from the symbol if this variable
                            // is not a pointer to that data type.
                            if entry.data_type.is_pointer() {
                                PTR_SIZE
                            } else {
                                sym.borrow().size
                            }
                        }
                        None => self.data_type_to_size(entry.data_type),
                    };
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
                                let mem_size = self.data_type_to_size(node.borrow().data_type);
                                node.borrow_mut().size = mem_size as u32;

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
                SymType::ARRAY(size) => {
                    entry.posn = SymPosition::Label(sym_name.clone());
                    // This unwrap should be safe since data type for arrays should
                    // always be validated in the parser
                    entry.size =
                        self.data_type_to_size(pointer_to(entry.data_type).unwrap()) * size as u32;
                }
                SymType::STRUCT => {
                    let mut struct_size = 0;

                    let mut member_node = match &entry.members {
                        Some(n) => Some(Rc::clone(n)),
                        None => None,
                    };

                    loop {
                        match member_node {
                            Some(node) => {
                                let mem_size = self.data_type_to_size(node.borrow().data_type);
                                node.borrow_mut().size = mem_size as u32;

                                // TODO: Handle aligning of struct members
                                node.borrow_mut().posn =
                                    SymPosition::StructBaseOffset(struct_size as i32);
                                struct_size += mem_size;
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
                    entry.size = struct_size;
                }
                SymType::ENUM => {}
            }
        }
    }

    // TODO: The behaviour of this method is only coherent if we invoke this
    // before function calls. Perhaps the name of the method should be changed.
    fn move_to_position(&mut self, r: usize, posn: &SymPosition, data_size: u32) {
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

    fn data_type_to_size(&self, data_type: DataType) -> u32 {
        match data_type {
            DataType::INT => 4,
            DataType::CHAR => 1,
            DataType::INTPTR => PTR_SIZE,
            DataType::CHARPTR => PTR_SIZE,
            DataType::STRUCTPTR => PTR_SIZE,
            DataType::VOIDPTR => PTR_SIZE,
            DataType::VOID => 0,
            DataType::NONE => 0,
            DataType::STRUCT => 0,
            DataType::ENUM => 4,
        }
    }

    // TODO: Customise based on size
    fn load_from_addr(&mut self, r: usize, data_size: u32) -> usize {
        let op = move_signex_op(data_size);
        self.gen_binary_op(
            op,
            Operand::Addr(self.reg_name(r)),
            Operand::Reg(self.reg_name(r)),
        );
        r
    }

    fn load_addr(&mut self, sym: &SymbolTableEntry) -> usize {
        let r = self.alloc_register();
        self.gen_binary_op(
            "leaq",
            Operand::RawString(sym_posn_to_string(&sym.posn)),
            Operand::Reg(self.reg_name(r)),
        );
        r
    }

    fn new_label(&mut self) -> String {
        let label = format!(".L{}", self.label_idx);
        self.label_idx += 1;
        label
    }

    fn gen_label(&mut self, label: &str) {
        self.output_str.push_str(&format!("{}:\n", label));
    }

    fn jump_if_zero(&mut self, r: usize, label: &str) {
        self.gen_binary_op(
            "testq",
            Operand::Reg(self.reg_name(r)),
            Operand::Reg(self.reg_name(r)),
        );

        self.free_register(r);

        self.gen_unary_op("je", Operand::RawString(label.to_string()));
    }

    fn jump_to_label(&mut self, label: &str) {
        self.gen_unary_op("jmp", Operand::RawString(label.to_string()));
    }

    fn comparison(&mut self, r1: usize, r2: usize, comp_type: ComparisonType) -> usize {
        let tmp_reg = self.alloc_register();
        self.gen_binary_op(
            "movq",
            Operand::Reg(self.reg_name(r1)),
            Operand::Reg(self.reg_name(tmp_reg)),
        );
        self.gen_binary_op(
            "cmpq",
            Operand::Reg(self.reg_name(r2)),
            Operand::Reg(self.reg_name(tmp_reg)),
        );

        let comp_op = match comp_type {
            ComparisonType::GT => "setg",
            ComparisonType::GTEQ => "setge",
            ComparisonType::LT => "setl",
            ComparisonType::LTEQ => "setle",
            ComparisonType::EQ => "sete",
            ComparisonType::NOTEQ => "setne",
        };

        self.gen_unary_op(comp_op, Operand::Reg("al".to_string()));
        self.gen_binary_op(
            "movzbq",
            Operand::Reg("al".to_string()),
            Operand::Reg(self.reg_name(r1)),
        );
        self.free_register(tmp_reg);
        self.free_register(r2);
        r1
    }

    fn gen_break(&mut self) {
        let break_label = match self.break_stack.last() {
            Some(label) => label.clone(),
            // The parser should ensure that this never happens
            None => panic!("Tried to generate break when break stack is empty"),
        };
        self.jump_to_label(&break_label);
    }

    fn gen_continue(&mut self) {
        let continue_label = match self.continue_stack.last() {
            Some(label) => label.clone(),
            // The parser should ensure that this never happens
            None => panic!("Tried to generate continue_stack when continue_stack stack is empty"),
        };
        self.jump_to_label(&continue_label);
    }

    fn push_break_label(&mut self, label: String) {
        self.break_stack.push(label);
    }

    fn pop_break_label(&mut self) {
        if self.break_stack.pop().is_none() {
            // The parser should ensure that this never happens
            panic!("Tried to pop from empty break stack.");
        }
    }

    fn push_continue_label(&mut self, label: String) {
        self.continue_stack.push(label);
    }

    fn pop_continue_label(&mut self) {
        if self.continue_stack.pop().is_none() {
            // The parser should ensure that this never happens
            panic!("Tried to pop from empty continue stack.");
        }
    }

    // TODO: This method doesn't mark the input register as freed as of now
    fn jump_if_equal_to(&mut self, r: usize, v: i32, label: &str) {
        self.gen_binary_op("cmpq", Operand::Int(v), Operand::Reg(self.reg_name(r)));

        self.gen_unary_op("je", Operand::RawString(label.into()));
    }
}

fn sym_posn_to_string(posn: &SymPosition) -> String {
    match posn {
        SymPosition::Reg(s) => format!("%{}", s),
        SymPosition::PositiveBPOffset(i) => format!("{}(%rbp)", i),
        SymPosition::Label(s) => format!("{}(%rip)", s),
        SymPosition::StructBaseOffset(_) => {
            panic!("Position of struct base offset cannot be determined without the base.");
        }
        SymPosition::TBD => {
            panic!("Position of symbol undetermined.");
        }
    }
}

fn move_signex_op(data_size: u32) -> &'static str {
    match data_size {
        1 => "movsbq",
        4 => "movslq",
        8 => "movq",
        _ => {
            panic!("Unknown size: {} in move_signex_op", data_size);
        }
    }
}

fn move_op(data_size: u32) -> &'static str {
    match data_size {
        1 => "movb",
        4 => "movl",
        8 => "movq",
        _ => {
            panic!("Unknown size: {} in move_signex_op", data_size);
        }
    }
}

fn str_label(id: usize) -> String {
    format!(".LC{}", id)
}

// TODO: Seems like this is not necessary for now
// Returns the op name and the size that was matched, some sizes
// default to operators of a different size.
// fn add_op(data_size: u32) -> (&'static str, u8) {
//     match data_size {
//         1..=4 => ("addl", 4),
//         8 => ("addq", 8),
//         _ => {
//             panic!("Unknown size: {} in add_op", data_size);
//         }
//     }
// }

// fn sub_op(data_size: u32) -> (&'static str, u8) {
//     match data_size {
//         1..=4 => ("subl", 4),
//         8 => ("subq", 8),
//         _ => {
//             panic!("Unknown size: {} in sub_op", data_size);
//         }
//     }
// }

// fn mul_op(data_size: u32) -> (&'static str, u8) {
//     match data_size {
//         1..=4 => ("imull", 4),
//         8 => ("imulq", 8),
//         _ => {
//             panic!("Unknown size: {} in mul_op", data_size);
//         }
//     }
// }
