use p8n_types::{
    Value,
    Result,
    Variable,
    Constant,
    Statement,
    Name,
};
use p8n_rreil_macro::rreil;

pub fn entry(_as_: Variable, _s: Constant, _imm12: Constant) -> Result<Vec<Statement>> {
    rreil! {
    }
}

pub fn l32r(pc: u64, at: Variable, label: Constant) -> Result<Vec<Statement>> {
    let pc = Constant::new(pc, 32);
    let mask = Constant::new(0b10_0000_0000_0000_0000, 18);

    rreil! {
        shr _catenated_label:32, [2]:32, (label)
        and _catenated_label:32, _catenated_label:32, (mask)

        add vAddr:32, (pc), [3]:32
        add vAddr:32, vAddr:32, _catenated_label:32
    }
}
