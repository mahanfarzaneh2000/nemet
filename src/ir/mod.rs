use crate::parser::types::VariableType;

#[derive(Debug, Clone)]
pub enum IRExpr {
    Var(VariableType),
    Imm(u64),
    TmpVar(usize),
}
impl IRExpr {
    pub fn is_imm(&self) -> bool {
        matches!(self, Self::Imm(_))
    }
}

#[derive(Debug, Clone)]
pub struct IRTag {
    tag: String,
}

#[derive(Debug)]
pub struct IRFunc {
    instrs: Vec<IRStmt>,
}

#[derive(Debug, Clone)]
pub enum IROperation {
    Add(IRExpr, IRExpr),
    Sub(IRExpr, IRExpr),
    Mult(IRExpr, IRExpr),
    Divide(IRExpr, IRExpr),
    Mod(IRExpr, IRExpr),
    Store(IRExpr),
    Mov(IRExpr, IRExpr),
    Cmove(IRExpr, IRExpr),
    Cmovne(IRExpr, IRExpr),
    Cmovg(IRExpr, IRExpr),
    Cmovl(IRExpr, IRExpr),
    Cmovge(IRExpr, IRExpr),
    Cmovle(IRExpr, IRExpr),
    Or(IRExpr, IRExpr),
    And(IRExpr, IRExpr),
    Sal(IRExpr, IRExpr),
    Sar(IRExpr, IRExpr),
    Shr(IRExpr, IRExpr),
    Cmp(IRExpr, IRExpr),
    Neg(IRExpr),
    Not(IRExpr),
    Call(IRExpr),
    Jmp(IRTag),
    Jz(IRTag),
    Jnz(IRTag),
    Jne(IRTag),
    Syscall(Vec<IRExpr>),
    Inc(IRExpr),
}

#[derive(Debug)]
pub struct IRGenerator {
    code: Vec<IRFunc>,
    data: Vec<IRData>,
}

#[derive(Debug)]
pub struct IRStmt {
    operation: Vec<IROperation>,
    temp_vars: Vec<IRExpr>,
}
impl IRStmt {
    pub fn new() -> Self {
        Self {
            operation: vec![],
            temp_vars: vec![]
        }
    }

    pub fn inst(&mut self, opr: IROperation) -> Option<IRExpr> {
        match &opr {
            IROperation::Add(ire1, ire2) => {
                match (ire1, ire2) {
                    (IRExpr::Imm(a) , IRExpr::Imm(b)) => Some(IRExpr::Imm(a + b)),
                    _ => {
                        self.operation.push(opr);
                        Some(IRExpr::TmpVar(self.operation.len() - 1))
                    }
                }                
            },
            IROperation::Sub(ire1, ire2) => {
                match (ire1, ire2) {
                    (IRExpr::Imm(a) , IRExpr::Imm(b)) => Some(IRExpr::Imm(a - b)),
                    _ => {
                        self.operation.push(opr);
                        Some(IRExpr::TmpVar(self.operation.len() - 1))
                    }
                }
            },
            IROperation::Mult(ire1, ire2) => {
                match (ire1, ire2) {
                    (IRExpr::Imm(a) , IRExpr::Imm(b)) => Some(IRExpr::Imm(a * b)),
                    _ => {
                        self.operation.push(opr);
                        Some(IRExpr::TmpVar(self.operation.len() - 1))
                    }
                }
            },
            IROperation::Divide(ire1, ire2) => {
                match (ire1, ire2) {
                    (IRExpr::Imm(a) , IRExpr::Imm(b)) => Some(IRExpr::Imm(a / b)),
                    _ => {
                        self.operation.push(opr);
                        Some(IRExpr::TmpVar(self.operation.len() - 1))
                    }
                }
            },
            IROperation::Mod(ire1, ire2) => {
                match (ire1, ire2) {
                    (IRExpr::Imm(a) , IRExpr::Imm(b)) => Some(IRExpr::Imm(a % b)),
                    _ => {
                        self.operation.push(opr);
                        Some(IRExpr::TmpVar(self.operation.len() - 1))
                    }
                }
            },
            IROperation::Store(_) => {
                self.operation.push(opr);
                Some(IRExpr::TmpVar(self.operation.len() - 1))
            },
            IROperation::Mov(ire1, _) 
                | IROperation::Cmove(ire1, _)
                | IROperation::Cmovne(ire1, _)
                | IROperation::Cmovg(ire1, _)
                | IROperation::Cmovl(ire1, _)
                | IROperation::Cmovge(ire1, _)
                | IROperation::Cmovle(ire1, _) => {
                self.operation.push(opr.clone());
                Some(ire1.clone())
            },
            IROperation::Or(ire1, ire2) => {
                match (ire1, ire2) {
                    (IRExpr::Imm(a) , IRExpr::Imm(b)) => Some(IRExpr::Imm(a | b)),
                    _ => {
                        self.operation.push(opr);
                        Some(IRExpr::TmpVar(self.operation.len() - 1))
                    }
                }
            },
            IROperation::And(ire1, ire2) => {
                match (ire1, ire2) {
                    (IRExpr::Imm(a) , IRExpr::Imm(b)) => Some(IRExpr::Imm(a & b)),
                    _ => {
                        self.operation.push(opr);
                        Some(IRExpr::TmpVar(self.operation.len() - 1))
                    }
                }
            },
            IROperation::Sal(ire1, ire2) => {
                match (ire1, ire2) {
                    (IRExpr::Imm(a) , IRExpr::Imm(b)) => Some(IRExpr::Imm(a << b)),
                    _ => {
                        self.operation.push(opr);
                        Some(IRExpr::TmpVar(self.operation.len() - 1))
                    }
                }
            },
            IROperation::Sar(ire1, ire2) | IROperation::Shr(ire1, ire2) => {
                match (ire1, ire2) {
                    (IRExpr::Imm(a) , IRExpr::Imm(b)) => Some(IRExpr::Imm(a >> b)),
                    _ => {
                        self.operation.push(opr);
                        Some(IRExpr::TmpVar(self.operation.len() - 1))
                    }
                }
            },
            IROperation::Cmp(ire1, ire2) => {
                 match (ire1, ire2) {
                    (IRExpr::Imm(a) , IRExpr::Imm(b)) => {
                        if a == b {
                            Some(IRExpr::Imm(1))
                        } else {
                            Some(IRExpr::Imm(0))
                        }
                    },
                    _ => {
                        self.operation.push(opr);
                        Some(IRExpr::TmpVar(self.operation.len() - 1))
                    }
                 }
            },
            IROperation::Neg(ire1) => {
                match ire1 {
                    IRExpr::Imm(a) => Some(IRExpr::Imm((-(*a as i64)) as u64)),
                    _ => {
                        self.operation.push(opr);
                        Some(IRExpr::TmpVar(self.operation.len() - 1))
                    }
                }
            },
            IROperation::Not(ire1) => {
                match ire1 {
                    IRExpr::Imm(a) => Some(IRExpr::Imm(!a)),
                    _ => {
                        self.operation.push(opr);
                        Some(IRExpr::TmpVar(self.operation.len() - 1))
                    }
                }
            },
            IROperation::Call(_) => {
                self.operation.push(opr);
                Some(IRExpr::TmpVar(self.operation.len() - 1))
            },
            IROperation::Jmp(_) 
                | IROperation::Jz(_)
                | IROperation::Jnz(_)
                | IROperation::Jne(_)
                | IROperation::Syscall(_) => None,
            IROperation::Inc(ire1) => {
                match ire1 {
                    IRExpr::Imm(a) => Some(IRExpr::Imm(a + 1)),
                    _ => {
                        self.operation.push(opr.clone());
                        Some(ire1.clone())
                    }
                }
            },
        }
    }
}

#[derive(Debug)]
pub struct IRData {
    tag: String,
    size: usize,
    data: Option<Vec<u8>>,
}
