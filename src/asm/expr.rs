use crate::{
    codegen::{
        Mem,
        Mnemonic::*,
        Reg::{self, *},
    },
    error_handeling::error,
    parser::{
        expr::{CompareOp, Expr, ExprType, FunctionCall, Op},
        types::VariableType,
    },
};

use super::{
    function_args_register,
    variables::{find_variable, get_vriable_map},
    CompilerContext,
};

pub fn compile_expr(cc: &mut CompilerContext, expr: &Expr) -> VariableType {
    // left = compile expr
    // right = compile expr
    // +
    match &expr.etype {
        ExprType::Variable(v) => {
            let Some(v_map) = get_vriable_map(cc, v) else {
                error("Trying to access an Undifined variable", expr.loc.clone());
            };
            // let mem_acss = format!(
            //     "{} [rbp-{}]",
            //     mem_word(&v_map.vtype),
            //     v_map.offset + v_map.vtype.size()
            // );
            let mem_acss = Mem::dyn_sized(&v_map.vtype, RBP - v_map.stack_offset());
            cc.codegen
                .instr2(Mov, Reg::AX_sized(&v_map.vtype), mem_acss);
            cc.codegen.instr1(Push, RAX);
            v_map.vtype
        }
        ExprType::Access(ident, ac) => {
            let Some(v_map) = get_vriable_map(cc, ident) else {
                error("Trying to access an Undifined variable", expr.loc.clone());
            };
            let VariableType::Custom(struct_ident) = v_map.vtype.clone() else {
                error("Trying to access an Undifined Structure", expr.loc.clone());
            };
            let Some(struc) = cc.structs_map.get(&struct_ident) else {
                error("Trying to access an Undifined Structure", expr.loc.clone());
            };
            let mut offset: usize = 0;
            let mut actype = VariableType::Any;
            match &ac.etype {
                ExprType::Variable(v) => {
                    for item in struc.items.iter() {
                        offset += item.1.size();
                        if &item.0 == v {
                            actype = item.1.clone();
                            break;
                        }
                    }
                }
                _ => todo!(),
            }
            if actype.is_any() {
                error(
                    "Trying to access unknown item from the list",
                    expr.loc.clone(),
                );
            }
            // cc.instruct_buf
            //     .push(asm!("mov rdx, [rbp-{}]", v_map.offset + v_map.vtype.size()));
            cc.codegen
                .instr2(Mov, RDX, Mem::U(RBP - v_map.stack_offset()));
            cc.codegen.instr2(Add, RDX, offset);
            cc.codegen.instr2(
                Mov,
                Reg::AX_sized(&actype),
                Mem::dyn_sized(&actype, RDX.into()),
            );
            // cc.instruct_buf.push(asm!(
            //     "mov {}, {} [rdx]",
            //     rbs("a", &actype),
            //     mem_word(&actype)
            // ));
            cc.codegen.instr1(Push, RAX);
            actype
        }
        ExprType::Bool(b) => {
            cc.codegen.instr1(Push, b);
            VariableType::Bool
        }
        ExprType::Char(x) => {
            cc.codegen.instr1(Push, x);
            VariableType::Char
        }
        ExprType::Int(x) => {
            // push x
            cc.codegen.instr1(Push, x);
            VariableType::Int
        }
        ExprType::Float(f) => {
            cc.codegen.instr1(Push, format!("__float64__({f})"));
            VariableType::Float
        }
        ExprType::Compare(c) => {
            let left_type = compile_expr(cc, c.left.as_ref());
            let right_type = compile_expr(cc, c.right.as_ref());
            cc.codegen.instr2(Mov, RCX, 0);
            cc.codegen.instr2(Mov, RDX, 1);
            let mut reg_type = left_type.clone();
            if right_type != left_type {
                if right_type.is_numeric() && left_type.is_numeric() {
                    if left_type.size() < right_type.size() {
                        reg_type = left_type;
                    } else {
                        reg_type = right_type;
                    }
                } else {
                    error(
                        format!(
                            "Invalid Comparison between types: ({}) and ({})",
                            left_type, right_type
                        ),
                        expr.loc.clone(),
                    );
                }
            }
            // Make sure rbx is first so the order is correct
            cc.codegen.instr1(Pop, RBX);
            cc.codegen.instr1(Pop, RAX);
            cc.codegen
                .instr2(Cmp, Reg::AX_sized(&reg_type), Reg::BX_sized(&reg_type));
            match c.op {
                CompareOp::Eq => {
                    cc.codegen.instr2(Cmove, RCX, RDX);
                }
                CompareOp::NotEq => {
                    cc.codegen.instr2(Cmovne, RCX, RDX);
                }
                CompareOp::Bigger => {
                    cc.codegen.instr2(Cmovg, RCX, RDX);
                }
                CompareOp::Smaller => {
                    cc.codegen.instr2(Cmovl, RCX, RDX);
                }
                CompareOp::BiggerEq => {
                    cc.codegen.instr2(Cmovge, RCX, RDX);
                }
                CompareOp::SmallerEq => {
                    cc.codegen.instr2(Cmovle, RCX, RDX);
                }
            }
            cc.codegen.instr1(Push, RCX);
            VariableType::Bool
        }
        ExprType::Binary(b) => {
            let left_type = compile_expr(cc, b.left.as_ref());
            let right_type = compile_expr(cc, b.right.as_ref());
            cc.codegen.instr1(Pop, RBX);
            cc.codegen.instr1(Pop, RAX);
            match b.op {
                Op::Plus => {
                    cc.codegen.instr2(Add, RAX, RBX);
                    cc.codegen.instr1(Push, RAX);
                }
                Op::Sub => {
                    cc.codegen.instr2(Sub, RAX, RBX);
                    cc.codegen.instr1(Push, RAX);
                }
                Op::Multi => {
                    cc.codegen.instr2(Imul, RAX, RBX);
                    cc.codegen.instr1(Push, RAX);
                }
                Op::Devide => {
                    cc.codegen.instr0(Cqo);
                    cc.codegen.instr1(Idiv, RBX);
                    cc.codegen.instr1(Push, RAX);
                }
                Op::Mod => {
                    cc.codegen.instr0(Cqo);
                    cc.codegen.instr1(Idiv, RBX);
                    cc.codegen.instr1(Push, RDX);
                }
                Op::Or => {
                    cc.codegen.instr2(Or, RAX, RBX);
                    cc.codegen.instr1(Push, RAX);
                }
                Op::And => {
                    cc.codegen.instr2(And, RAX, RBX);
                    cc.codegen.instr1(Push, RAX);
                }
                Op::Lsh => {
                    cc.codegen.instr2(Mov, RCX, RBX);
                    cc.codegen.instr2(Sal, RAX, CL);
                    cc.codegen.instr1(Push, RAX);
                }
                Op::Rsh => {
                    cc.codegen.instr2(Mov, RCX, RBX);
                    cc.codegen.instr2(Sar, RAX, CL);
                    cc.codegen.instr1(Push, RAX);
                }
                Op::LogicalOr => {
                    cc.codegen.instr2(Or, RAX, RBX);
                    cc.codegen.instr1(Push, RAX);
                    return VariableType::Bool;
                }
                Op::LogicalAnd => {
                    cc.codegen.instr2(And, RAX, RBX);
                    cc.codegen.instr1(Push, RAX);
                    return VariableType::Bool;
                }
                Op::Not => {
                    panic!("Unvalid binary operation");
                }
            }
            if right_type.is_numeric() && left_type.is_numeric() {
                match left_type.cast(&right_type) {
                    Ok(t) => t,
                    Err(msg) => error(msg, expr.loc.clone()),
                }
            } else {
                error(
                    format!(
                        "Invalid Operation ({}) on non-numeric types: ({}) and ({})",
                        b.op, left_type, right_type
                    ),
                    expr.loc.clone(),
                );
            }
        }
        ExprType::String(str) => {
            let data_array = asmfy_string(str);
            let id = cc.codegen.add_data_seg(data_array, 8);
            cc.codegen.instr1(Push, format!("data{id}"));
            cc.codegen.instr1(Push, format!("len{id}"));
            VariableType::String
        }
        ExprType::Unary(u) => {
            let right_type = compile_expr(cc, &u.right);
            cc.codegen.instr1(Pop, RAX);
            match u.op {
                Op::Sub => {
                    cc.codegen.instr1(Neg, RAX);
                    cc.codegen.instr1(Push, RAX);
                    if right_type == VariableType::UInt {
                        return VariableType::Int;
                    } else {
                        return right_type;
                    }
                }
                Op::Plus => {
                    cc.codegen.instr1(Push, RAX);
                }
                Op::Not => {
                    cc.codegen.instr1(Not, RAX);
                    cc.codegen.instr1(Push, RAX);
                }
                _ => {
                    unreachable!();
                }
            }
            if right_type.is_numeric() {
                right_type
            } else {
                error(
                    format!("Invalid Operation ({}) for type ({})", u.op, right_type),
                    expr.loc.clone(),
                );
            }
        }
        ExprType::FunctionCall(fc) => match compile_function_call(cc, fc) {
            Ok(ftype) => ftype,
            Err(msg) => error(msg, expr.loc.clone()),
        },
        ExprType::Ptr(e) => {
            compile_ptr(cc, e);
            VariableType::Pointer
        }
        ExprType::DeRef(r) => {
            let t = compile_expr(cc, r);
            match t {
                VariableType::Array(_, _) => {
                    todo!("Changed!");
                }
                VariableType::Pointer => {
                    cc.codegen.instr1(Pop, RAX);
                    cc.codegen.instr2(Mov, RCX, Mem::Qword(RAX.into()));
                    cc.codegen.instr1(Push, RCX);
                }
                _ => {
                    error(format!("Expected a Pointer found ({t})"), expr.loc.clone());
                }
            }
            VariableType::Any
        }
        ExprType::ArrayIndex(ai) => {
            let v_map = find_variable(cc, ai.ident.clone()).unwrap_or_else(|| {
                error(
                    format!("Trying to access an Undifined variable ({})", ai.ident),
                    expr.loc.clone(),
                );
            });
            compile_expr(cc, &ai.indexer);
            cc.codegen.instr1(Pop, RBX);
            // cc.instruct_buf
            //     .push(asm!("mov rdx, [rbp-{}]", v_map.offset + v_map.vtype.size()));
            // cc.instruct_buf
            //     .push(asm!("imul rbx, {}", v_map.vtype.item_size()));
            // cc.instruct_buf.push(asm!("add rdx, rbx"));

            // let mem_acss = format!("{} [rdx]", mem_word(&v_map.vtype));
            // let mem_acss = format!(
            //     "{} [rbp-{}+rbx*{}]",
            //     mem_word(&v_map.vtype),
            //     v_map.offset + v_map.vtype.size(),
            //     v_map.vtype.item_size()
            // );
            let mem_acss = Mem::dyn_sized(
                &v_map.vtype,
                RBP - v_map.stack_offset() + RBX * v_map.vtype.item_size(),
            );
            let reg = Reg::AX_sized(&v_map.vtype);
            cc.codegen.instr2(Mov, reg, mem_acss);
            cc.codegen.instr1(Push, RAX);
            match v_map.vtype {
                VariableType::Array(t, _) => t.as_ref().clone(),
                _ => unreachable!(),
            }
        }
    }
}

fn compile_ptr(cc: &mut CompilerContext, expr: &Expr) {
    match &expr.etype {
        ExprType::Variable(v) => {
            let Some(v_map) = get_vriable_map(cc, v) else {
                error("Trying to access an Undifined variable", expr.loc.clone());
            };
            match v_map.vtype {
                VariableType::Array(_, _) => {
                    //todo!("ReImplemnt");
                    //let mov_addr = format!("qword [rbp - {}]", v_map.offset + v_map.vtype.size());
                    //cc.codegen.mov(RAX, mov_addr);
                    //cc.codegen.push(RAX);

                    //cc.codegen.mov(RAX, RBP);
                    cc.codegen
                        .instr2(Lea, RAX, Mem::U(RBP - v_map.stack_offset()));
                    cc.codegen.instr1(Push, RAX);
                }
                _ => {
                    cc.codegen.instr2(Mov, RAX, RBP);
                    cc.codegen
                        .instr2(Sub, RAX, v_map.offset + v_map.vtype.size());
                    cc.codegen.instr1(Push, RAX);
                }
            }
        }
        _ => {
            todo!("Impl Pointers");
        }
    }
}

fn compile_function_call(
    cc: &mut CompilerContext,
    fc: &FunctionCall,
) -> Result<VariableType, String> {
    for (index, arg) in fc.args.iter().enumerate() {
        // let argv_type = expr_type(arg);
        compile_expr(cc, arg);
        match arg.etype {
            ExprType::String(_) => {
                cc.codegen.instr1(Pop, RAX);
                cc.codegen.instr1(Pop, function_args_register(index));
            }
            _ => {
                cc.codegen.instr1(Pop, function_args_register(index));
            }
        }
    }
    // TODO: Setup a unresolved function table
    let Some(fun) = cc.functions_map.get(&fc.ident) else {
        return Err(format!(
            "Error: Function {} is not avaliable in this scope. {}",
            &fc.ident, "Make sure you are calling the correct function"
        ));
    };
    cc.codegen.instr2(Mov, RAX, 0);
    cc.codegen.instr1(Call, fc.ident.clone());
    if fun.ret_type != VariableType::Void {
        cc.codegen.instr1(Push, RAX);
    }
    Ok(fun.ret_type.clone())
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
