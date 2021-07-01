use super::*;
use std::error::Error;
use std::fmt;
use std::fs;
use std::io::Write;

enum Operand {
    Int(i32),
    Reg(String),
    Func(String),
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Int(i) => write!(f, "${}", i),
            Operand::Reg(str) => write!(f, "%{}", str),
            Operand::Func(str) => write!(f, "{}", str),
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
        let mut gen = Self {
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
        };
        gen.preamble();
        gen
    }

    fn reg_name(&self, i: usize) -> String {
        self.registers[i].name.to_string()
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

    fn preamble(&mut self) {
        let code = r#"
    .text
.LC0:
    .string "%d\n"
printint:
    pushq   %rbp
    movq    %rsp, %rbp
    subq    $16, %rsp
    movl    %edi, -4(%rbp)
    movl    -4(%rbp), %eax
    movl    %eax, %esi
    leaq    .LC0(%rip), %rdi
    movl    $0, %eax
    call    printf@PLT
    nop
    leave
    ret
    .globl  main
    .type   main,   @function
main:
    pushq   %rbp
    movq    %rsp, %rbp
"#;
        self.output_str.push_str(code);
    }

    fn postamble(&mut self) {
        let code = r#"      
    movl    $0, %eax
    popq    %rbp
    ret
"#;
        self.output_str.push_str(code);
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
}
