use super::*;
use crate::parser::SymType;
use std::error::Error;
use std::fmt;
use std::fs;
use std::io::Write;

enum Operand {
    Int(i32),
    Reg(String),
    Func(String),
    IPOffset(String),
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Int(i) => write!(f, "${}", i),
            Operand::Reg(str) => write!(f, "%{}", str),
            Operand::Func(str) => write!(f, "{}", str),
            Operand::IPOffset(str) => write!(f, "{}(%rip)", str),
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
        self.gen_unary_op("call", Operand::Func("printint@PLT".into()));
        self.free_register(r1);
    }

    fn load_glob_var(&mut self, sym: &SymbolTableEntry) -> usize {
        let r = self.alloc_register();
        let op_name = match sym.size {
            1 => "movzbq",
            4 => "movslq",
            8 => "movq",
            _ => panic!("Invalid size in assign_glob_var."),
        };
        self.gen_binary_op(
            op_name,
            Operand::IPOffset(sym.name.to_string()),
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
            Operand::IPOffset(sym.name.to_string()),
        );
        r
    }

    fn gen_glob_syms(&mut self, symtable: HashMap<String, Rc<SymbolTableEntry>>) {
        for (sym_name, entry) in symtable {
            if entry.sym_type != SymType::VARIABLE {
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
                sym_name, entry.size
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

    fn generate_output(&mut self, output_filename: &str) -> Result<(), Box<dyn Error>> {
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
}
