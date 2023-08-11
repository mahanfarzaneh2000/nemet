use std::collections::HashMap;
use std::error::Error;
use std::fs::{self, File};
use std::io::{BufWriter, Write};
use std::process::exit;

use crate::parser::block::Block;
use crate::parser::expr::{CompareOp, Expr, Op, FunctionCall};
use crate::parser::function::{Function, FunctionArg};
use crate::parser::program::{ProgramFile, ProgramItem};
use crate::parser::stmt::{Assgin, AssginOp, ElseBlock, IFStmt, Stmt, VariableDeclare, WhileStmt};

macro_rules! asm {
    ($($arg:tt)+) => (
        format!("    {}\n",format_args!($($arg)+))
    );
}

pub fn rbs(register: &str,size: usize) -> String {
    match register {
        "a" | "b" | "c" | "d" => {
            match size {
                1 => format!("{register}l"),
                2 => format!("{register}x"),
                4 => format!("e{register}x"),
                8 => format!("r{register}x"),
                _ => {
                    unreachable!("Incurrect Size")
                }
            }
        },
        "sp" | "bp"  => {
            match size {
                1 => format!("{register}l"),
                2 => format!("{register}"),
                4 => format!("e{register}"),
                8 => format!("r{register}"),
                _ => {
                    unreachable!("Incurrect Size")
                }
            }
        },
        "si" | "di"  => {
            match size {
                1 => format!("{register}l"),
                2 => format!("{register}"),
                4 => format!("e{register}"),
                8 => format!("r{register}"),
                _ => {
                    unreachable!("Incurrect Size")
                }
            }
        },
        "r8" | "r9" | "r10" | "r11" => {
            match size {
                1 => format!("{register}b"),
                2 => format!("{register}w"),
                4 => format!("{register}d"),
                8 => format!("{register}"),
                _ => {
                    unreachable!("Incurrect Size")
                }
            }
        },
        _ => {
            panic!("Wrong register identifier!");
        }
    }
}

pub fn function_args_register(arg_numer: usize,size: usize) -> String {
    match arg_numer {
        0 => rbs("di",size),
        1 => rbs("si",size),
        2 => rbs("d", size),
        3 => rbs("c", size),
        4 => rbs("r8",size),
        5 => rbs("r9",size),
        _ => unreachable!(),
    }
}

#[derive(Debug, Clone)]
pub struct VariableMap {
    _ident: String,
    offset: usize,
    size: usize,
    is_mut: bool,
}

pub struct IRGenerator {
    instruct_buf: Vec<String>,
    data_buf: Vec<String>,
    scoped_blocks: Vec<usize>,
    block_id: usize,
    variables_map: HashMap<String, VariableMap>,
    functions_map: HashMap<String, Function>,
    mem_offset: usize,
    g_mem_offset: usize,
}

impl IRGenerator {
    // TODO: handle Error for Parsing
    pub fn new() -> Self {
        Self {
            instruct_buf: Vec::new(),
            data_buf: Vec::new(),
            scoped_blocks: Vec::new(),
            block_id: 0,
            variables_map: HashMap::new(),
            functions_map: HashMap::new(),
            mem_offset: 0,
            g_mem_offset: 0,
        }
    }

    fn frame_size(&self) -> usize {
        2 << self.mem_offset.ilog2() as usize
    }

    pub fn find_variable(&self, ident: String) -> Option<VariableMap> {
        for block_id in &self.scoped_blocks {
            let map_ident = format!("{ident}%{}", block_id);
            let map = self.variables_map.get(&map_ident);
            if let Some(map) = map {
                return Some(map.clone());
            }
        }
        None
    }

    pub fn insert_variable(&mut self, var: &VariableDeclare) {
        let ident: String;
        let var_map: VariableMap;
        if var.is_static {
            ident = format!("{}%{}", var.ident, 0);
            var_map = VariableMap {
                _ident: var.ident.clone(),
                offset: self.g_mem_offset,
                // TODO: Change size
                size: 8,
                is_mut: false,
            };
        } else {
            ident = format!("{}%{}", var.ident, self.block_id);
            var_map = VariableMap {
                _ident: var.ident.clone(),
                offset: self.mem_offset,
                // TODO: Change size
                size: 8,
                is_mut: var.mutable,
            };
        }
        self.mem_offset += 8;
        if var.init_value.is_some() {
            let init_value = var.init_value.clone().unwrap();
            // this pushes result in stack
            self.compile_expr(&init_value);
            let mem_acss = format!("qword [rbp-{}]", var_map.offset + var_map.size);
            self.instruct_buf.push(asm!("pop rax"));
            self.instruct_buf.push(asm!("mov {mem_acss},rax"));
        }
        self.variables_map.insert(ident, var_map);
    }

    pub fn function_args(&mut self,args: &Vec<FunctionArg>) {
        let mut args_count = 0;
        for arg in args {
            let ident = format!("{}%{}", arg.ident, self.block_id);
            let map = VariableMap{
                _ident: arg.ident.clone(),
                offset: self.mem_offset,
                is_mut: false,
                size: 8,
            };
            if args_count < 6 {
                let mem_acss = format!("qword [rbp-{}]", map.offset + map.size);
                let reg = function_args_register(args_count,8);
                self.instruct_buf.push(asm!("mov {},{}",mem_acss,reg));
            } else {
                let mem_overload = format!("qword [rbp+{}]", 16 + (args_count-6)*8);
                let mem_acss = format!("qword [rbp-{}]", map.offset + map.size);
                self.instruct_buf.push(asm!("mov {},{}",mem_acss,mem_overload));
            }
            self.variables_map.insert(ident, map);
            self.mem_offset += 8;
            args_count += 1;
        }

    }

    pub fn function(&mut self, f: Function) {
        self.scoped_blocks = Vec::new();
        self.block_id = 0;
        self.scoped_blocks.push(0);
        if f.ident == "main" {
            self.instruct_buf.push("_start:\n".to_string());
        } else {
            self.instruct_buf.push(format!("{}:\n",f.ident));
        }

        // set rbp to stack pointer for this block
        let index_1 = self.instruct_buf.len();
        self.instruct_buf.push(String::new());
        let index_2 = self.instruct_buf.len();
        self.instruct_buf.push(String::new());
        let index_3 = self.instruct_buf.len();
        self.instruct_buf.push(String::new());

        self.function_args(&f.args);
        self.compile_block(&f.block);
        self.scoped_blocks.pop();
        // revert rbp
        if !self.variables_map.is_empty() {
            self.instruct_buf[index_1] = asm!("push rbp");
            self.instruct_buf[index_2] = asm!("mov rbp, rsp");
            self.instruct_buf[index_3] = asm!("sub rsp, {}", self.frame_size());
            self.instruct_buf.push(asm!("pop rbp"));
        }
        // Call Exit Syscall
        if f.ident == "main" {
            self.instruct_buf.push(asm!("mov rax, 60"));
            self.instruct_buf.push(asm!("mov rdi, 0"));
            self.instruct_buf.push(asm!("syscall"));
        }
    }

    // TODO: Handle Compilation Error
    pub fn compile(&mut self, program: ProgramFile) -> Result<(), Box<dyn Error>> {
        for item in program.items {
            match item {
                ProgramItem::StaticVar(_s) => {
                    todo!();
                    // self.insert_variable(&s);
                }
                ProgramItem::Func(f) => {
                    self.functions_map.insert(f.ident.clone(),f.clone());
                    self.function(f);
                }
            }
        }
        assert!(
            self.scoped_blocks.is_empty(),
            "Somting went wrong: Scope has not been cleared"
        );

        //println!("{:?}",self.scoped_blocks);
        self.write_to_file()?;
        Ok(())
    }

    fn compile_block(&mut self, block: &Block) {
        self.block_id += 1;
        self.scoped_blocks.push(self.block_id);
        for stmt in &block.stmts {
            self.compile_stmt(stmt);
        }
        self.scoped_blocks.pop().unwrap();
    }

    fn compile_if_stmt(&mut self, ifs: &IFStmt, exit_tag: usize) {
        self.compile_expr(&ifs.condition);
        let next_tag = match ifs.else_block.as_ref() {
            ElseBlock::None => exit_tag,
            _ => self.instruct_buf.len(),
        };
        self.instruct_buf.push(asm!("pop rax"));
        self.instruct_buf.push(asm!("test rax, rax"));
        self.instruct_buf.push(asm!("jz .L{}", next_tag));

        self.compile_block(&ifs.then_block);
        match ifs.else_block.as_ref() {
            ElseBlock::None => {
                self.instruct_buf.push(asm!(".L{}:", next_tag));
            }
            ElseBlock::Else(b) => {
                self.instruct_buf.push(asm!("jmp .L{}", exit_tag));
                self.instruct_buf.push(asm!(".L{}:", next_tag));
                self.compile_block(b);
                self.instruct_buf.push(asm!(".L{}:", exit_tag));
            }
            ElseBlock::Elif(iff) => {
                self.instruct_buf.push(asm!("jmp .L{}", exit_tag));
                self.instruct_buf.push(asm!(".L{}:", next_tag));
                self.compile_if_stmt(iff, exit_tag);
            }
        }
    }

    fn compile_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::VariableDecl(v) => {
                self.insert_variable(v);
            }
            Stmt::Print(e) => {
                self.compile_expr(e);
                match e {
                    Expr::String(_) => {
                        self.instruct_buf.push(asm!("mov rax, 1"));
                        self.instruct_buf.push(asm!("mov rdi, 1"));
                        self.instruct_buf
                            .push(asm!("mov rsi, data{}", self.data_buf.len() - 2));
                        self.instruct_buf
                            .push(asm!("mov rdx, len{}", self.data_buf.len() - 2));
                        self.instruct_buf.push(asm!("syscall"));
                    }
                    _ => {
                        self.instruct_buf.push(asm!("pop rdi"));
                        self.instruct_buf.push(asm!("call print"));
                    }
                }
            }
            Stmt::If(ifs) => {
                let exit_tag = self.instruct_buf.len();
                self.compile_if_stmt(ifs, exit_tag);
            }
            Stmt::Assgin(a) => {
                self.compile_assgin(a);
            }
            Stmt::While(w) => {
                self.compile_while(w);
            }
            Stmt::Return(e) => {
                self.compile_expr(e);
                self.instruct_buf.push(asm!("pop rax"));
                self.instruct_buf.push(asm!("leave"));
                self.instruct_buf.push(asm!("ret"));
                println!("Warning: might segfault add leave or fix dataframe");
            }
            _ => {
                todo!();
            }
        }
    }

    fn compile_while(&mut self, w_stmt: &WhileStmt) {
        let cond_tag = self.instruct_buf.len();
        self.instruct_buf.push(asm!("jmp .L{}", cond_tag));
        let block_tag = cond_tag + 1;
        self.instruct_buf.push(asm!(".L{}:", block_tag));
        self.compile_block(&w_stmt.block);
        self.instruct_buf.push(asm!(".L{}:", cond_tag));
        // Jump after a compare
        self.compile_expr(&w_stmt.condition);
        self.instruct_buf.push(asm!("pop rax"));
        self.instruct_buf.push(asm!("test rax, rax"));
        self.instruct_buf.push(asm!("jnz .L{}", block_tag));
    }

    fn compile_assgin(&mut self, assign: &Assgin) {
        match &assign.left {
            Expr::Variable(v) => {
                let v_map = self.find_variable(v.clone()).unwrap_or_else(|| {
                    eprintln!("Error: Could not find variable {} in this scope", v.clone());
                    exit(1);
                });
                if !v_map.is_mut {
                    eprintln!("Error: Variable is not mutable. Did you forgot to define it with '=' insted of ':=' ?");
                    exit(1);
                }
                match assign.op {
                    AssginOp::Eq => {
                        self.compile_expr(&assign.right);
                        let mem_acss = format!("qword [rbp-{}]", v_map.offset + v_map.size);
                        self.instruct_buf.push(asm!("pop rax"));
                        self.instruct_buf.push(asm!("mov {mem_acss},rax"));
                    }
                }
            }
            Expr::ArrayIndex(_) => {
                todo!();
            }
            _ => {
                eprintln!("Error: Expected a Variable type expression found Value");
                exit(1);
            }
        }
    }

    fn compile_expr(&mut self, expr: &Expr) {
        // left = compile expr
        // right = compile expr
        // +
        match expr {
            Expr::Variable(v) => {
                let v_map = self.find_variable(v.clone()).unwrap_or_else(|| {
                    eprintln!("Error: Trying to access an Undifined variable ({v})");
                    exit(1);
                });
                let mem_acss = format!("qword [rbp-{}]", v_map.offset + v_map.size);
                self.instruct_buf.push(asm!("mov rax,{mem_acss}"));
                self.instruct_buf.push(asm!("push rax"));
            }
            Expr::Int(x) => {
                // push x
                self.instruct_buf.push(asm!("push {}", x));
            }
            Expr::Compare(c) => {
                // TODO: Convert exprs to 0 or 1 and push into stack
                self.compile_expr(c.left.as_ref());
                self.compile_expr(c.right.as_ref());
                self.instruct_buf.push(asm!("mov rcx, 0"));
                self.instruct_buf.push(asm!("mov rdx, 1"));
                self.instruct_buf.push(asm!("pop rbx"));
                self.instruct_buf.push(asm!("pop rax"));
                self.instruct_buf.push(asm!("cmp rax, rbx"));
                match c.op {
                    CompareOp::Eq => {
                        self.instruct_buf.push(asm!("cmove rcx, rdx"));
                    }
                    CompareOp::NotEq => {
                        self.instruct_buf.push(asm!("cmovne rcx, rdx"));
                    }
                    CompareOp::Bigger => {
                        self.instruct_buf.push(asm!("cmovg rcx, rdx"));
                    }
                    CompareOp::Smaller => {
                        self.instruct_buf.push(asm!("cmovl rcx, rdx"));
                    }
                    CompareOp::BiggerEq => {
                        self.instruct_buf.push(asm!("cmovge rcx, rdx"));
                    }
                    CompareOp::SmallerEq => {
                        self.instruct_buf.push(asm!("cmovle rcx, rdx"));
                    }
                }
                self.instruct_buf.push(asm!("push rcx"));
            }
            Expr::Binary(b) => {
                self.compile_expr(b.left.as_ref());
                self.compile_expr(b.right.as_ref());
                self.instruct_buf.push(asm!("pop rbx"));
                self.instruct_buf.push(asm!("pop rax"));
                match b.op {
                    Op::Plus => {
                        self.instruct_buf.push(asm!("add rax, rbx"));
                        self.instruct_buf.push(asm!("push rax"));
                    }
                    Op::Sub => {
                        self.instruct_buf.push(asm!("sub rax, rbx"));
                        self.instruct_buf.push(asm!("push rax"));
                    }
                    Op::Multi => {
                        self.instruct_buf.push(asm!("imul rax, rbx"));
                        self.instruct_buf.push(asm!("push rax"));
                    }
                    Op::Devide => {
                        self.instruct_buf.push(asm!("cdq"));
                        self.instruct_buf.push(asm!("idiv rbx"));
                        self.instruct_buf.push(asm!("push rax"));
                    }
                    Op::Mod => {
                        self.instruct_buf.push(asm!("cdq"));
                        self.instruct_buf.push(asm!("idiv rbx"));
                        self.instruct_buf.push(asm!("push rdx"));
                    }
                    _ => unreachable!(),
                }
            }
            Expr::String(str) => {
                let id = self.data_buf.len();
                let data_array = Self::asmfy_string(str);
                self.data_buf.push(asm!("data{id} db {}", data_array));
                self.data_buf.push(asm!("len{id} equ $ - data{id}"));
                // data6524 db "<str>"
                // len6524     data6524
                // push len6524jkjk
                // push data6524
                // self.instruct_buf.push(asm!("push 13"));
            }
            Expr::Unary(_) => {
                todo!();
            }
            Expr::FunctionCall(fc) => {
                self.compile_function_call(fc);
            }
            _ => {
                todo!();
            }
        }
    }

    fn compile_function_call(&mut self,fc: &FunctionCall) {
        let mut index = 0;
        for arg in &fc.args {
            self.compile_expr(arg);
            self.instruct_buf.push(asm!("pop {}",function_args_register(index,8)));
            index += 1;
        }
        // TODO: Setup a unresolved function table
        let fun = self.functions_map.get(&fc.ident).unwrap();
        self.instruct_buf.push(asm!("mov rax, 0"));
        self.instruct_buf.push(asm!("call {}",fc.ident));
        if fun.ret_type.is_some() {
            self.instruct_buf.push(asm!("push rax"));
        }
    }

    fn asmfy_string(str: &str) -> String {
        let mut res = String::new();
        let source: Vec<char> = str.chars().collect();
        let mut i = 0;
        while i < source.len() {
            match source[i] {
                '\n' => {
                    if !res.is_empty() {
                        res.push(',');
                    }
                    res.push_str(" 10");
                    i += 1;
                }
                '\t' => {
                    if !res.is_empty() {
                        res.push(',');
                    }
                    res.push_str(" 9");
                    i += 1;
                }
                '\r' => {
                    if !res.is_empty() {
                        res.push(',');
                    }
                    res.push_str(" 13");
                    i += 1;
                }
                '\"' => {
                    if !res.is_empty() {
                        res.push(',');
                    }
                    res.push_str(" 34");
                    i += 1;
                }
                _ => {
                    if !res.is_empty() {
                        res.push(',');
                    }
                    res.push('\"');
                    while i < source.len() {
                        if source[i] == '\n'
                            || source[i] == '\"'
                            || source[i] == '\t'
                            || source[i] == '\r'
                        {
                            break;
                        }
                        res.push(source[i]);
                        i += 1;
                    }
                    res.push('\"');
                }
            }
        }
        res
    }

    // TODO: Error Handleing Error Type FILE
    fn write_to_file(&self) -> Result<(), Box<dyn Error>> {
        fs::create_dir_all("./build").unwrap();
        let stream = File::create("./build/output.asm").unwrap();
        let mut file = BufWriter::new(stream);
        println!("[info] Generating asm files...");
        file.write_all(b";; This File is Automatically Created Useing Nemet Parser\n")?;
        file.write_all(b";; Under MIT License Copyright MahanFarzaneh 2023-2024\n\n")?;

        file.write_all(b"\n")?;
        if !self.data_buf.is_empty() {
            file.write_all(b"section .data\n")?;
            for data in &self.data_buf {
                file.write_all(data.as_bytes())?;
            }
        }
        file.write_all(b"\n")?;

        file.write_all(b"section .text\n")?;
        file.write_all(b"global _start\n")?;

        file.write_all(b"print:\n")?;
        file.write_all(b"    push    rbp\n")?;
        file.write_all(b"    mov     rbp, rsp\n")?;
        file.write_all(b"    sub     rsp, 64\n")?;
        file.write_all(b"    mov     qword [rbp-56], rdi\n")?;
        file.write_all(b"    mov     qword [rbp-8], 1\n")?;
        file.write_all(b"    mov     eax, 32\n")?;
        file.write_all(b"    sub     rax, qword [rbp-8]\n")?;
        file.write_all(b"    mov     BYTE [rbp-48+rax], 10\n")?;
        file.write_all(b".L3:\n")?;
        file.write_all(b"    mov     rcx, qword [rbp-56]\n")?;
        file.write_all(b"    mov     rdx, -3689348814741910323\n")?;
        file.write_all(b"    mov     rax, rcx\n")?;
        file.write_all(b"    mul     rdx\n")?;
        file.write_all(b"    shr     rdx, 3\n")?;
        file.write_all(b"    mov     rax, rdx\n")?;
        file.write_all(b"    sal     rax, 2\n")?;
        file.write_all(b"    add     rax, rdx\n")?;
        file.write_all(b"    add     rax, rax\n")?;
        file.write_all(b"    sub     rcx, rax\n")?;
        file.write_all(b"    mov     rdx, rcx\n")?;
        file.write_all(b"    mov     eax, edx\n")?;
        file.write_all(b"    lea     edx, [rax+48]\n")?;
        file.write_all(b"    mov     eax, 31\n")?;
        file.write_all(b"    sub     rax, qword [rbp-8]\n")?;
        file.write_all(b"    mov     byte [rbp-48+rax], dl\n")?;
        file.write_all(b"    add     qword [rbp-8], 1\n")?;
        file.write_all(b"    mov     rax, qword [rbp-56]\n")?;
        file.write_all(b"    mov     rdx, -3689348814741910323\n")?;
        file.write_all(b"    mul     rdx\n")?;
        file.write_all(b"    mov     rax, rdx\n")?;
        file.write_all(b"    shr     rax, 3\n")?;
        file.write_all(b"    mov     qword [rbp-56], rax\n")?;
        file.write_all(b"    cmp     qword [rbp-56], 0\n")?;
        file.write_all(b"    jne     .L3\n")?;
        file.write_all(b"    mov     eax, 32\n")?;
        file.write_all(b"    sub     rax, qword [rbp-8]\n")?;
        file.write_all(b"    lea     rdx, [rbp-48]\n")?;
        file.write_all(b"    add     rax, rdx\n")?;
        file.write_all(b"    mov     rsi, rax\n")?;
        file.write_all(b"    mov     rbx, qword [rbp-8]\n")?;
        file.write_all(b"    mov     rdx, rbx\n")?;
        file.write_all(b"    mov     rdi, 1\n")?;
        file.write_all(b"    mov     rax, 1\n")?;
        file.write_all(b"    syscall\n")?;
        file.write_all(b"    leave\n")?;
        file.write_all(b"    ret\n")?;

        for instruct in &self.instruct_buf {
            file.write_all(instruct.as_bytes())?;
        }

        file.flush().unwrap();
        Ok(())
    }
}
