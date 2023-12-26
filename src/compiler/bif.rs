use crate::{
    codegen::{instructions::Opr, mnmemonic::Mnemonic::*, register::Reg::*, Codegen},
    mem, memb, memq,
};

#[derive(Hash, PartialEq, Eq)]
pub enum Bif {
    Print,
}

impl Bif {
    pub fn implement(&self, codegen: &mut Codegen) {
        match self {
            Self::Print => Self::print_impl(codegen),
        }
    }

    fn print_impl(codegen: &mut Codegen) {
        codegen.set_lable("print");
        codegen.instr1(Push, RBP);
        codegen.instr2(Mov, RBP, RSP);
        codegen.instr2(Sub, RSP, 64);
        codegen.instr2(Mov, memq!(RBP, -56), RDI);
        codegen.instr2(Mov, memq!(RBP, -8), 1);
        codegen.instr2(Mov, RAX, 32);
        codegen.instr2(Sub, RAX, memq!(RBP, -8));
        codegen.instr2(Mov, memb!(RBP, -48, RAX), 10);
        codegen.set_lable(".L3");
        codegen.instr2(Mov, RCX, memq!(RBP, -56));
        codegen.instr2(Mov, RDX, -3689348814741910323i64);
        codegen.instr2(Mov, RAX, RCX);
        codegen.instr1(Mul, RDX);
        codegen.instr2(Shr, RDX, 3);
        codegen.instr2(Mov, RAX, RDX);
        codegen.instr2(Sal, RAX, 2);
        codegen.instr2(Add, RAX, RDX);
        codegen.instr2(Add, RAX, RAX);
        codegen.instr2(Sub, RCX, RAX);
        codegen.instr2(Mov, RDX, RCX);
        codegen.instr2(Mov, EAX, EDX);
        codegen.instr2(Lea, EDX, mem!(RAX, 48));
        codegen.instr2(Mov, EAX, 31);
        codegen.instr2(Sub, RAX, memq!(RBP, -8));
        codegen.instr2(Mov, memb!(RBP, -48, RAX), DL);
        codegen.instr2(Add, memq!(RBP, -8), 1);
        codegen.instr2(Mov, RAX, memq!(RBP, -56));
        codegen.instr2(Mov, RDX, -3689348814741910323i64);
        codegen.instr1(Mul, RDX);
        codegen.instr2(Mov, RAX, RDX);
        codegen.instr2(Shr, RAX, 3);
        codegen.instr2(Mov, memq!(RBP, -56), RAX);
        codegen.instr2(Cmp, memq!(RBP, -56), 0);
        codegen.instr1(Jne, ".L3");
        codegen.instr2(Mov, EAX, 32);
        codegen.instr2(Sub, RAX, memq!(RBP, -8));
        codegen.instr2(Lea, RDX, mem!(RBP, -48));
        codegen.instr2(Add, RAX, RDX);
        codegen.instr2(Mov, RSI, RAX);
        codegen.instr2(Mov, RBX, memq!(RBP, -8));
        codegen.instr2(Mov, RDX, RBX);
        codegen.instr2(Mov, RDI, 1);
        codegen.instr2(Mov, RAX, 1);
        codegen.instr0(Syscall);
        codegen.instr0(Leave);
        codegen.instr0(Ret);
    }
}
