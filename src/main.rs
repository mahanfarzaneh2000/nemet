/**********************************************************************************************
*
*   Nmet main entry point
*
*   This file provides and entry point to the compiler all the cli arguments and shell
*   funtionalites are handeled frem this file.
*   All modules used in the code base must be defined here.
*
*   LICENSE: MIT
*
*   Copyright (c) 2023-2024 Mahan Farzaneh (@mahanfr)
*
*   This software is provided "as-is", without any express or implied warranty. In no event
*   will the authors be held liable for any damages arising from the use of this software.
*
*   Permission is granted to anyone to use this software for any purpose, including commercial
*   applications, and to alter it and redistribute it freely, subject to the following restrictions:
*
*     1. The origin of this software must not be misrepresented; you must not claim that you
*     wrote the original software. If you use this software in a product, an acknowledgment
*     in the product documentation would be appreciated but is not required.
*
*     2. Altered source versions must be plainly marked as such, and must not be misrepresented
*     as being the original software.
*
*     3. This notice may not be removed or altered from any source distribution.
*
**********************************************************************************************/
use std::env::Args;
use std::error::Error;
use std::fs::remove_file;
use std::path::PathBuf;
use std::process::Command;
use std::str::FromStr;
use std::sync::Mutex;
use std::{env::args, process::exit};

mod assembler;
mod compiler;
mod error_handeling;
mod formats;
mod lexer;
mod linker;
mod macros;
mod optim;
mod parser;
mod terms;
mod ir;
#[cfg(test)]
mod tests;
mod utils;
use assembler::text::x86_64_nasm_generator;
use compiler::{compile, CompilerContext};
use utils::get_output_path_from_input;

use crate::compiler::impl_bifs;
use crate::utils::padding_right;

// --- Static Compiler Defenition
pub static VERSION: &str = "v0.8.1";

/// Terget name for assembling using Nasm
fn assembler_target(co: &CompilerOptions) -> &'static str {
    if co.target_platform == 1 {
        return "win64";
    }
    if cfg!(windows) {
        "win64"
    } else {
        "elf64"
    }
}

// nmet [options] (input_file)
// -nasm -no-link -no-assemble -keep-asm -keep-obj
// -o outputpath
// -l<mod_name>
// -L<mod_path>
// -T <Target>
#[derive(Debug, Default, Clone)]
pub struct CompilerOptions {
    pub output_path: Option<PathBuf>,
    pub use_nasm: bool,
    pub no_linking: bool,
    pub no_assembling: bool,
    pub keep_asm: bool,
    pub keep_obj: bool,
    pub static_lib: bool,
    pub dynamic_lib: bool,
    pub linker_flags: Vec<String>,
    pub use_libc: bool,
    pub create_bin: bool,
    pub target_platform: u8,
}

fn copywrite() {
    println!("-------------------------------------------------------------");
    println!("| Nmet {} |", padding_right(VERSION, 54));
    println!("| Nmet Programmin Language Copyright: Mahanfaraneh 2023-2024 |");
    println!("| Project distributed Under MIT License                      |");
    println!("-------------------------------------------------------------");
}

pub fn help_command(program_name: &str) {
    println!("{program_name} [options] (input_file)");
    println!("Options:");
    println!(
        "  {} Specify output path (default: \"./build/input_file\")",
        padding_right("-o <output_path>", 20)
    );
    println!(
        "  {} Simulate (Interpet) the program for debugging and running on unsupported OS",
        padding_right("-s | --simulate", 20)
    );
    println!(
        "  {} dump instructions in a binary file",
        padding_right("-b | --bin", 20)
    );
    println!(
        "  {} use Nasm Assembler to assemble generated code",
        padding_right("--nasm", 20)
    );
    println!(
        "  {} Do not link the generated object file",
        padding_right("--no-link", 20)
    );
    println!(
        "  {} Only Generates an asm file",
        padding_right("--no-assemble", 20)
    );
    println!(
        "  {} Do not remove the generated asm file",
        padding_right("--keep-asm", 20)
    );
    println!(
        "  {} Do not remove the generated object file",
        padding_right("--keep-obj", 20)
    );
    println!(
        "  {} Use C library Dynamicaly",
        padding_right("--use-libc", 20)
    );
    println!(
        "  {} Search for library LIBNAME",
        padding_right("-l<LIBNAME>", 20)
    );
    println!(
        "  {} add directory to library search path",
        padding_right("-L<DIR>", 20)
    );
    println!(
        "  {} Generate a dynamic library",
        padding_right("--dynamic-lib", 20)
    );
    println!("  {} Generate a static library", padding_right("--lib", 20));
    println!("  {} Show help", padding_right("-h, --help", 20));
    println!("  {} Show Version", padding_right("-v, --version", 20));
}

/// Runs External commands for generating the object files
pub fn assemble_with_nasm(path: PathBuf, co: &CompilerOptions) {
    log_info!(
        "Assembling for {} - generaiting {}",
        assembler_target(co),
        path.with_extension("o").to_string_lossy()
    );
    let nasm_output = Command::new("nasm")
        .arg(format!("-f{}", assembler_target(co)))
        .arg("-o")
        .arg(path.with_extension("o"))
        .arg(path.with_extension("asm"))
        .output()
        .expect("Can not run nasm command! do you have nasm installed?");
    if !nasm_output.status.success() {
        log_error!("Failed to Assemble: Status code non zero");
        eprintln!("{}", String::from_utf8(nasm_output.stderr).unwrap());
    }
    log_success!("Object file generated using Nasm!");
}

/// Runs External commands for generating the executable
pub fn link_to_exc(path: PathBuf, co: &CompilerOptions) {
    log_info!(
        "Linking object file - generating {}",
        path.with_extension("").to_string_lossy()
    );
    let linker_output = Command::new("ld")
        .arg("-o")
        .arg(path.with_extension(""))
        .arg(path.with_extension("o"))
        .args(["-dynamic-linker", "/usr/lib64/ld-linux-x86-64.so.2"])
        .args(&co.linker_flags)
        .output()
        .expect("Can not link using ld command!");
    if !linker_output.status.success() {
        log_error!("Failed to Link Exectable: Status code non zero");
        eprintln!("{}", String::from_utf8(linker_output.stderr).unwrap());
    } else {
        log_success!("Executable file have been Generated!");
    }
}

// Link to Static Library
pub fn link_to_static_lib(path: PathBuf, _: &CompilerOptions) {
    log_info!(
        "Archiving object file - generating {}",
        path.with_extension("a").to_string_lossy()
    );
    let linker_output = Command::new("ar")
        .arg("-cvq")
        .arg(path.with_extension("a"))
        .arg(path.with_extension("o"))
        .output()
        .expect("Can not archive using ar command!");
    if !linker_output.status.success() {
        log_error!("Failed to Generate Static Library: Status code non zero");
        eprintln!("{}", String::from_utf8(linker_output.stderr).unwrap());
    } else {
        log_success!("Static Library file have been Generated!");
    }
}

// Link to Dynamin Library
pub fn link_to_dynamic_lib(path: PathBuf, co: &CompilerOptions) {
    log_info!(
        "Linking object file - generating {}",
        path.with_extension("so").to_string_lossy()
    );
    let linker_output = Command::new("ld")
        .arg("-shared")
        .arg("-o")
        .arg(path.with_extension("so"))
        .arg(path.with_extension("o"))
        .args(["-dynamic-linker", "/usr/lib64/ld-linux-x86-64.so.2"])
        .args(&co.linker_flags)
        .output()
        .expect("Can not link using ld command!");
    if !linker_output.status.success() {
        log_error!("Failed to Link Dynamic Library: Status code non zero");
        eprintln!("{}", String::from_utf8(linker_output.stderr).unwrap());
    } else {
        log_success!("Dynamic Library file have been Generated!");
    }
}

pub fn setup_compiler(input: String, co: &CompilerOptions) {
    let out_path = match co.output_path.clone() {
        None => get_output_path_from_input(input.clone().into()),
        Some(pt) => pt,
    };
    let mut compiler_context = CompilerContext::new(input.clone(), co);

    compile(&mut compiler_context, input.clone());
    impl_bifs(&mut compiler_context);
    let prefix = out_path.parent().unwrap();
    std::fs::create_dir_all(prefix).unwrap();
    if co.use_nasm {
        log_info!("Generating asm text file...");
        x86_64_nasm_generator(out_path.as_path(), &compiler_context).unwrap();
        log_success!("Nasm Text file Generated!");
        if co.no_assembling {
            return;
        }
        assemble_with_nasm(out_path.clone(), co);
    } else {
        if co.create_bin {
            log_info!("Generating binary file...");
            crate::formats::elf::generate_bin(out_path.as_path(), &mut compiler_context);
            log_success!("Instructions Binary file Generated!");
        }
        log_info!("Generating elf object file...");
        crate::formats::elf::generate_elf(out_path.as_path(), &mut compiler_context);
        log_success!("Elf object file Generated!");
    }
    if !co.no_linking {
        if co.dynamic_lib {
            link_to_dynamic_lib(out_path.clone(), co);
        } else if co.static_lib {
            link_to_static_lib(out_path.clone(), co);
        } else {
            link_to_exc(out_path.clone(), co);
        }
    }
    if !co.keep_asm && remove_file(out_path.with_extension("asm")).is_ok() {
        log_info!("Removing asm files")
    }
    if !co.keep_obj && remove_file(out_path.with_extension("o")).is_ok() {
        log_info!("Removing object files")
    }
}

fn collect_compiler_options(args: &mut Args) -> (String, CompilerOptions) {
    let mut co = CompilerOptions::default();
    if cfg!(windows) {
        co.target_platform = 1;
    }
    let compiler_path = args.next().unwrap();
    let mut input_path = String::new();
    loop {
        let Some(arg) = args.next() else {
            break;
        };
        if arg.starts_with("-l") || arg.starts_with("-L") {
            co.linker_flags.push(arg.clone());
            continue;
        }
        match arg.as_str() {
            "-h" | "--help" => {
                copywrite();
                help_command(&compiler_path);
                exit(0);
            }
            "-v" | "--version" => {
                copywrite();
                exit(0);
            }
            "--no-link" | "-c" => {
                co.no_linking = true;
                co.keep_obj = true;
            }
            "--no-assemble" => {
                co.no_assembling = true;
                co.keep_asm = true;
            }
            "--nasm" => co.use_nasm = true,
            "--keep-asm" => co.keep_asm = true,
            "--keep-obj" => co.keep_obj = true,
            "--use-libc" => co.use_libc = true,
            "--lib" => {
                co.static_lib = true;
                co.dynamic_lib = false;
                co.keep_obj = true;
            }
            "--dynamic-lib" => {
                co.static_lib = false;
                co.dynamic_lib = true;
            }
            "-b" | "--bin" => co.create_bin = true,
            "-T" => {
                let Some(target) = args.next() else {
                    log_error!("No target specified!");
                    help_command(&compiler_path);
                    exit(-1);
                };
                co.target_platform = target_string_to_number(&target);
            }
            "-o" => {
                let Some(path) = args.next() else {
                    log_error!("No output path after -o option!");
                    help_command(&compiler_path);
                    exit(-1);
                };
                co.output_path = Some(PathBuf::from_str(&path).unwrap());
            }
            _ => {
                if arg.starts_with('-') {
                    log_error!("Invalid compiler option ({})!", arg);
                    help_command(&compiler_path);
                    exit(-1);
                } else {
                    input_path = arg;
                }
            }
        }
    }
    if input_path.is_empty() {
        log_error!("No file has been provided!");
        help_command(&compiler_path);
        exit(-1);
    }
    (input_path, co)
}

static TARGET_PLATFORM: Mutex<u8> = Mutex::new(0);

pub fn target_string_to_number(target: &str) -> u8 {
    match target {
        "LINUX" | "linux" => 0,
        "WINDOWS" | "WIN" | "windows" | "win" => 1,
        _ => u8::MAX,
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    // parse_elf_objfile("./tests/libadd.a".to_string());

    let mut args = args();
    let (ipath, co) = collect_compiler_options(&mut args);
    *TARGET_PLATFORM.lock().unwrap() = co.target_platform;
    setup_compiler(ipath, &co);
    Ok(())
}
