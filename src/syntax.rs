use std::{error, fmt};

macro_rules! mask {
    ($v:expr, $bits:expr, $pos:expr) => {
        (((1 << $bits) - 1) & ($v >> $pos));
    };
}

#[test]
fn test_mask() {
    let opcode = 0b0011_1000_0110_0000_0100_0000;
    let op1 = mask!(opcode, 4, 16);
    assert_eq!(op1, 0b1000);
}

macro_rules! constraint {
    ($val:expr, $($exp:expr),+) => {
        match $val {
            $($exp => {}),+
            _ => {
                return Err(Error::ConstraintNotSatisfied {
                    field: stringify!($val),
                    value: $val,
                    expected: &[$($exp),+],
                });
            }
        }
    };
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Error {
    Reserved,
    ConstraintNotSatisfied {
        field: &'static str,
        value: u32,
        expected: &'static [u32],
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Reserved => write!(f, "reserved instruction"),
            Error::ConstraintNotSatisfied { field, value, expected } => {
                write!(f, "constraint {}", field)?;
                match expected.len() {
                    1 => write!(f, " = {}", expected[0])?,
                    2 => write!(f, " = {},{}", expected[0], expected[1])?,
                    _ => {},
                }
                write!(f, " not satisfied, got {}", value)
            }
        }
    }
}

impl error::Error for Error {
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Id {
    ILL,
    RET,
    RETW,
    JX,
    CALLX0,
    CALLX4,
    CALLX8,
    CALLX12,
    MOVSP,
    ISYNC,
    RSYNC,
    ESYNC,
    DSYNC,
    EXCW,
    MEMW,
    EXTW,
    RFE,
    RFUE,
    RFDE,
    RFWO,
    RFWU,
    RFI,
    RFME,
    BREAK,
    SYSCALL,
    RSIL,
    WAITI,
    ANY4,
    ALL4,
    ANY8,
    ALL8,
    AND,
    OR,
    XOR,
    SSR,
    SSL,
    SSA8L,
    SSA8B,
    SSAI,
    RER,
    WER,
    ROTW,
    NSA,
    NSAU,
    RITLB0,
    IITLB,
    PITLB,
    WITLB,
    RITLB1,
    RDTLB0,
    IDTLB,
    PDTLB,
    WDTLB,
    RDTLB1,
    NEG,
    ABS,
    ADD,
    ADDX2,
    ADDX4,
    ADDX8,
    SUB,
    SUBX2,
    SUBX4,
    SUBX8,
    SLLI,
    SRAI,
    SRLI,
    XSR,
    SRC,
    SRL,
    SLL,
    SRA,
    MUL16U,
    MUL16S,
    LICT,
    SICT,
    LICW,
    SICW,
    LDCT,
    SDCT,
    RFDO,
    RFDD,
    ANDB,
    ANDBC,
    ORB,
    ORBC,
    XORB,
    MULL,
    MULUH,
    MULSH,
    QUOU,
    QUOS,
    REMU,
    REMS,
    RSR,
    WSR,
    SEXT,
    CLAMPS,
    MIN,
    MAX,
    MINU,
    MAXU,
    MOVEQZ,
    MOVNEZ,
    MOVLTZ,
    MOVGEZ,
    MOVF,
    MOVT,
    RUR,
    WUR,
    EXTUI,

    L32R,

    J,
    BZ,
    BEQI,
    BNEI,
    BLTI,
    BGEI,
    ENTRY,
    BF,
    BT,
    LOOP,
    LOOPNEZ,
    LOOPGTZ,
    BLTUI,
    BGEUI,

    L32R_N,
    S32I_N,
    ADD_N,
    ADDI_N,

    MOV_N,
    RET_N,
    RETW_N,
    BREAK_N,
    NOP_N,

    RESERVED,
    RESERVED_N,
}

impl Id {
    pub fn size(self) -> usize {
        if self.is_narrow() {
            2
        } else {
            3
        }
    }

    #[rustfmt::skip]
    pub fn mnemonic(self) -> &'static str {
        use Id::*;

        match self {
            ILL     => "ill",
            RET     => "ret",
            RETW    => "retw",
            JX      => "jx",
            CALLX0  => "callx0",
            CALLX4  => "callx4",
            CALLX8  => "callx8",
            CALLX12 => "callx12",
            MOVSP   => "movsp",
            ISYNC   => "isync",
            RSYNC   => "rsync",
            ESYNC   => "esync",
            DSYNC   => "dsync",
            EXCW    => "excw",
            MEMW    => "memw",
            EXTW    => "extw",
            RFE     => "rfe",
            RFUE    => "rfue",
            RFDE    => "rfde",
            RFWO    => "rfwo",
            RFWU    => "rfwu",
            RFI     => "rfi",
            RFME    => "rfme",
            BREAK   => "break",
            SYSCALL => "syscall",
            RSIL    => "syscall",
            WAITI   => "waiti",
            ANY4    => "any4",
            ALL4    => "all4",
            ANY8    => "any8",
            ALL8    => "all8",
            AND     => "and",
            OR      => "or",
            XOR     => "xor",
            SSR     => "ssr",
            SSL     => "ssl",
            SSA8L   => "ssa8l",
            SSA8B   => "ssa8b",
            SSAI    => "ssai",
            RER     => "rer",
            WER     => "wer",
            ROTW    => "rotw",
            NSA     => "nsa",
            NSAU    => "nsau",
            RITLB0  => "ritlb0",
            IITLB   => "iitlb",
            PITLB   => "pitlb",
            WITLB   => "witlb",
            RITLB1  => "ritlb1",
            RDTLB0  => "rdtlb0",
            IDTLB   => "idtlb",
            PDTLB   => "pdtlb",
            WDTLB   => "wdtlb",
            RDTLB1  => "rdtlb1",
            NEG     => "neg",
            ABS     => "abs",
            ADD     => "add",
            ADDX2   => "addx2",
            ADDX4   => "addx4",
            ADDX8   => "addx8",
            SUB     => "sub",
            SUBX2   => "subx2",
            SUBX4   => "subx4",
            SUBX8   => "subx8",
            SLLI    => "slli",
            SRAI    => "srai",
            SRLI    => "srli",
            XSR     => "xsr",
            SRC     => "src",
            SRL     => "srl",
            SLL     => "sll",
            SRA     => "sra",
            MUL16U  => "mul16u",
            MUL16S  => "mul16s",
            LICT    => "lict",
            SICT    => "sict",
            LICW    => "licw",
            SICW    => "sicw",
            LDCT    => "ldct",
            SDCT    => "sdct",
            RFDO    => "rfdo",
            RFDD    => "rfdd",
            ANDB    => "andb",
            ANDBC   => "andbc",
            ORB     => "orb",
            ORBC    => "orbc",
            XORB    => "xorb",
            MULL    => "mull",
            MULUH   => "muluh",
            MULSH   => "mulsh",
            QUOU    => "quou",
            QUOS    => "quos",
            REMU    => "remu",
            REMS    => "rems",
            RSR     => "rsr",
            WSR     => "wsr",
            SEXT    => "sext",
            CLAMPS  => "clamps",
            MIN     => "min",
            MAX     => "max",
            MINU    => "minu",
            MAXU    => "maxu",
            MOVEQZ  => "moveqz",
            MOVNEZ  => "movneqz",
            MOVLTZ  => "movltz",
            MOVGEZ  => "movgez",
            MOVF    => "movf",
            MOVT    => "movt",
            RUR     => "rur",
            WUR     => "wur",
            EXTUI   => "extui",

            L32R    => "l32r",

            J       => "j",
            BZ      => "bz",
            BEQI    => "beqi",
            BNEI    => "bnei",
            BLTI    => "blti",
            BGEI    => "bgei",
            ENTRY   => "entry",
            BF      => "bf",
            BT      => "bt",
            LOOP    => "loop",
            LOOPNEZ => "loopnez",
            LOOPGTZ => "loopgtz",
            BLTUI   => "bltui",
            BGEUI   => "bgeui",

            L32R_N  => "l32r.n",
            S32I_N  => "s32r.n",
            ADD_N   => "add.n",
            ADDI_N  => "addi.n",

            MOV_N   => "mov.n",
            RET_N   => "ret.n",
            RETW_N  => "retw.n",
            BREAK_N => "break.n",
            NOP_N   => "nop.n",

            RESERVED | RESERVED_N => "reserved",
        }
    }

    pub fn is_narrow(self) -> bool {
        match self {
            Id::MOV_N => true,
            _ => false,
        }
    }
}

macro_rules! decode {
    ($func:ident, $opcode:expr) => {{
        Ok(Instruction {
            id: $func($opcode)?,
            opcode: $opcode,
        })
    }};
}

#[derive(Debug)]
pub struct BRI12 {
    pub imm12: u16,
    pub s: u8,
    pub m: u8,
    pub n: u8,
}

#[derive(Debug)]
pub struct RRRN {
    pub r: u8,
    pub s: u8,
    pub t: u8,
}

#[derive(Debug)]
pub struct RI16 {
    pub imm16: u16,
    pub t: u8,
}

#[derive(Debug, Clone)]
pub struct Instruction {
    pub id: Id,
    pub opcode: u32,
}

impl Instruction {
    pub fn size(&self) -> usize {
        self.id.size()
    }

    pub fn to_bri12(&self) -> BRI12 {
        BRI12 {
            imm12: mask!(self.opcode, 12, 12) as u16,
            s: mask!(self.opcode, 4, 8) as u8,
            m: mask!(self.opcode, 2, 6) as u8,
            n: mask!(self.opcode, 2, 4) as u8,
        }
    }

    pub fn to_rrrn(&self) -> RRRN {
        RRRN {
            r: mask!(self.opcode, 4, 12) as u8,
            s: mask!(self.opcode, 4, 8) as u8,
            t: mask!(self.opcode, 4, 4) as u8,
        }
    }

    pub fn to_ri16(&self) -> RI16 {
        RI16 {
            imm16: mask!(self.opcode, 16, 8) as u16,
            t: mask!(self.opcode, 4, 4) as u8,
        }
    }
}

pub fn match_opcode(bytes: (u8, u8, u8)) -> Result<Instruction, Error> {
    let opcode = (u32::from(bytes.2) << 16) | (u32::from(bytes.1) << 8) | u32::from(bytes.0);

    println!("opcode = {:b}", opcode);

    let op0 = mask!(opcode, 4, 0);
    match op0 {
        0b0000 => decode!(qrst, opcode),
        0b0001 => Ok(Instruction {
            id: Id::L32R,
            opcode,
        }),
        0b0010 => decode!(lsai, opcode),
        0b0011 => decode!(lsci, opcode),
        0b0100 => decode!(mac16, opcode),
        0b0101 => decode!(calln, opcode),
        0b0110 => decode!(si, opcode),
        0b0111 => decode!(b, opcode),
        0b1000 => Ok(Instruction {
            id: Id::L32R_N,
            opcode,
        }),
        0b1001 => Ok(Instruction {
            id: Id::S32I_N,
            opcode,
        }),
        0b1010 => Ok(Instruction {
            id: Id::ADD_N,
            opcode,
        }),
        0b1011 => Ok(Instruction {
            id: Id::ADDI_N,
            opcode,
        }),

        0b1100 => decode!(st2, opcode),
        0b1101 => decode!(st3, opcode),
        0b1110 | 0b1111 => Err(Error::Reserved),

        _ => unreachable!(),
    }
}

fn qrst(opcode: u32) -> Result<Id, Error> {
    let op1 = mask!(opcode, 4, 16);

    match op1 {
        0b0000 => rst0(opcode),
        0b0001 => rst1(opcode),
        0b0010 => rst2(opcode),
        0b0011 => rst3(opcode),
        0b0100 | 0b0101 => Ok(Id::EXTUI),
        0b0110 => cust0(opcode),
        0b0111 => cust1(opcode),
        0b1000 => lscx(opcode),
        0b1001 => lsc4(opcode),
        0b1010 => fp0(opcode),
        0b1011 => fp1(opcode),
        0b1100..=0b1111 => Err(Error::Reserved),
        _ => unreachable!(),
    }
}

fn rst0(opcode: u32) -> Result<Id, Error> {
    let op2 = mask!(opcode, 4, 20);

    match op2 {
        0b0000 => st0(opcode),
        0b0001 => Ok(Id::AND),
        0b0010 => Ok(Id::OR),
        0b0011 => Ok(Id::XOR),
        0b0100 => st1(opcode),
        0b0101 => tlb(opcode),
        0b0110 => rt0(opcode),
        0b0111 => Err(Error::Reserved),
        0b1000 => Ok(Id::ADD),
        0b1001 => Ok(Id::ADDX2),
        0b1010 => Ok(Id::ADDX4),
        0b1011 => Ok(Id::ADDX8),
        0b1100 => Ok(Id::SUB),
        0b1101 => Ok(Id::SUBX2),
        0b1110 => Ok(Id::SUBX4),
        0b1111 => Ok(Id::SUBX8),
        _ => unreachable!(),
    }
}

fn st0(opcode: u32) -> Result<Id, Error> {
    let r = mask!(opcode, 4, 12);

    match r {
        0b0000 => smn0(opcode),
        0b0001 => Ok(Id::MOVSP),
        0b0010 => sync(opcode),
        0b0011 => rfei(opcode),
        0b0100 => Ok(Id::BREAK),
        0b0101 => Ok(Id::SYSCALL),
        0b0110 => Ok(Id::RSIL),
        0b0111 => Ok(Id::WAITI),
        0b1000 => Ok(Id::ANY4),
        0b1001 => Ok(Id::ALL4),
        0b1010 => Ok(Id::ANY8),
        0b1011 => Ok(Id::ALL8),
        0b1100..=0b1111 => Err(Error::Reserved),
        _ => unreachable!(),
    }
}

fn smn0(opcode: u32) -> Result<Id, Error> {
    let m = mask!(opcode, 2, 6);

    match m {
        0b00 => Ok(Id::ILL),
        0b01 => Err(Error::Reserved),
        0b10 => jr(opcode),
        0b11 => callx(opcode),
        _ => unreachable!(),
    }
}

fn jr(opcode: u32) -> Result<Id, Error> {
    let n = mask!(opcode, 2, 4);

    match n {
        0b00 => Ok(Id::RET),
        0b01 => Ok(Id::RETW),
        0b10 => Ok(Id::JX),
        0b11 => Err(Error::Reserved),
        _ => unreachable!(),
    }
}

fn callx(opcode: u32) -> Result<Id, Error> {
    let n = mask!(opcode, 2, 4);

    match n {
        0b00 => Ok(Id::CALLX0),
        0b01 => Ok(Id::CALLX4),
        0b10 => Ok(Id::CALLX8),
        0b11 => Ok(Id::CALLX12),
        _ => unreachable!(),
    }
}

fn sync(opcode: u32) -> Result<Id, Error> {
    let s = mask!(opcode, 4, 8);
    constraint!(s, 0);

    let t = mask!(opcode, 4, 4);

    match t {
        0b0000 => Ok(Id::ISYNC),
        0b0001 => Ok(Id::RSYNC),
        0b0010 => Ok(Id::ESYNC),
        0b0011 => Ok(Id::DSYNC),
        0b0100..=0b0111 => Err(Error::Reserved),
        0b1000 => Ok(Id::EXCW),
        0b1001..=0b1011 => Err(Error::Reserved),
        0b1100 => Ok(Id::MEMW),
        0b1101 => Ok(Id::EXTW),
        0b1110..=0b1111 => Err(Error::Reserved),
        _ => unreachable!(),
    }
}

fn rfei(opcode: u32) -> Result<Id, Error> {
    let t = mask!(opcode, 4, 4);

    match t {
        0b0000 => rfet(opcode),
        0b0001 => Ok(Id::RFI),
        0b0010 => {
            let s = mask!(opcode, 4, 8);
            constraint!(s, 0);
            Ok(Id::RFME)
        }
        0b0011..=0b1111 => Err(Error::Reserved),
        _ => unreachable!(),
    }
}

fn rfet(opcode: u32) -> Result<Id, Error> {
    let s = mask!(opcode, 4, 8);

    match s {
        0b0000 => Ok(Id::RFE),
        0b0001 => Ok(Id::RFUE),
        0b0010 => Ok(Id::RFDE),
        0b0011 => Err(Error::Reserved),
        0b0100 => Ok(Id::RFWO),
        0b0101 => Ok(Id::RFWU),
        0b0110..=0b1111 => Err(Error::Reserved),
        _ => unreachable!(),
    }
}

fn st1(opcode: u32) -> Result<Id, Error> {
    let r = mask!(opcode, 4, 12);
    let s = mask!(opcode, 4, 8);
    let t = mask!(opcode, 4, 4);

    match r {
        0b0000 => {
            constraint!(t, 0);
            Ok(Id::SSR)
        }
        0b0001 => {
            constraint!(t, 0);
            Ok(Id::SSL)
        }
        0b0010 => {
            constraint!(t, 0);
            Ok(Id::SSA8L)
        }
        0b0011 => {
            constraint!(t, 0);
            Ok(Id::SSA8B)
        }
        0b0100 => {
            constraint!(t, 0);
            Ok(Id::SSAI)
        }
        0b0101 => Err(Error::Reserved),
        0b0110 => Ok(Id::RER),
        0b0111 => Ok(Id::WER),
        0b1000 => {
            constraint!(s, 0);
            Ok(Id::ROTW)
        }
        0b1001..=0b1101 => Err(Error::Reserved),
        0b1110 => Ok(Id::NSA),
        0b1111 => Ok(Id::NSAU),
        _ => unreachable!(),
    }
}

fn tlb(opcode: u32) -> Result<Id, Error> {
    let r = mask!(opcode, 4, 12);
    let t = mask!(opcode, 4, 4);

    match r {
        0b0000..=0b0010 => Err(Error::Reserved),
        0b0011 => Ok(Id::RITLB0),
        0b0100 => {
            constraint!(t, 0);
            Ok(Id::IITLB)
        }
        0b0101 => Ok(Id::PITLB),
        0b0110 => Ok(Id::WITLB),
        0b0111 => Ok(Id::RITLB1),
        0b1000..=0b1010 => Err(Error::Reserved),
        0b1011 => Ok(Id::RDTLB0),
        0b1100 => {
            constraint!(t, 0);
            Ok(Id::IDTLB)
        }
        0b1101 => Ok(Id::PDTLB),
        0b1110 => Ok(Id::WDTLB),
        0b1111 => Ok(Id::RDTLB1),
        _ => unreachable!(),
    }
}

fn rt0(opcode: u32) -> Result<Id, Error> {
    let s = mask!(opcode, 4, 8);

    match s {
        0b0000 => Ok(Id::NEG),
        0b0001 => Ok(Id::ABS),
        0b0010..=0b1111 => Err(Error::Reserved),
        _ => unreachable!(),
    }
}

fn rst1(opcode: u32) -> Result<Id, Error> {
    let op2 = mask!(opcode, 4, 20);
    let s = mask!(opcode, 4, 8);
    let t = mask!(opcode, 4, 4);

    match op2 {
        0b0000 | 0b0001 => Ok(Id::SLLI),
        0b0010 | 0b0011 => Ok(Id::SRAI),
        0b0100 => Ok(Id::SRLI),
        0b0101 => Err(Error::Reserved),
        0b0110 => Ok(Id::XSR),
        0b0111 => accer(opcode),
        0b1000 => Ok(Id::SRC),
        0b1001 => {
            constraint!(s, 0);
            Ok(Id::SRL)
        }
        0b1010 => {
            constraint!(t, 0);
            Ok(Id::SLL)
        }
        0b1011 => {
            constraint!(s, 0);
            Ok(Id::SRA)
        }
        0b1100 => Ok(Id::MUL16U),
        0b1101 => Ok(Id::MUL16S),
        0b1110 => Err(Error::Reserved),
        0b1111 => imp(opcode),
        _ => unreachable!(),
    }
}

fn accer(opcode: u32) -> Result<Id, Error> {
    let r = mask!(opcode, 4, 12);

    match r {
        0b0000 => Ok(Id::RER),
        0b0001..=0b0111 => Err(Error::Reserved),
        0b1000 => Ok(Id::WER),
        0b1001..=0b1111 => Err(Error::Reserved),
        _ => unreachable!(),
    }
}

fn imp(opcode: u32) -> Result<Id, Error> {
    let r = mask!(opcode, 4, 12);

    match r {
        0b0000 => Ok(Id::LICT),
        0b0001 => Ok(Id::SICT),
        0b0010 => Ok(Id::LICW),
        0b0011 => Ok(Id::SICW),
        0b0100..=0b0111 => Err(Error::Reserved),
        0b1000 => Ok(Id::LDCT),
        0b1001 => Ok(Id::SDCT),
        0b1010..=0b1101 => Err(Error::Reserved),
        0b1110 => rfdx(opcode),
        0b1111 => Err(Error::Reserved),
        _ => unreachable!(),
    }
}

fn rfdx(opcode: u32) -> Result<Id, Error> {
    let s = mask!(opcode, 4, 8);

    let t = mask!(opcode, 4, 4);

    match t {
        0b0000 => {
            constraint!(s, 0);
            Ok(Id::RFDO)
        },
        0b0001 => {
            constraint!(s, 0, 1);
            Ok(Id::RFDD)
        },
        0b0010..=0b1111 => Err(Error::Reserved),
        _ => unreachable!(),
    }
}

fn rst2(opcode: u32) -> Result<Id, Error> {
    let op2 = mask!(opcode, 4, 20);

    match op2 {
        0b0000 => Ok(Id::ANDB),
        0b0001 => Ok(Id::ANDBC),
        0b0010 => Ok(Id::ORB),
        0b0011 => Ok(Id::ORBC),
        0b0100 => Ok(Id::XORB),
        0b0101..=0b0111 => Err(Error::Reserved),
        0b1000 => Ok(Id::MULL),
        0b1001 => Err(Error::Reserved),
        0b1010 => Ok(Id::MULUH),
        0b1011 => Ok(Id::MULSH),
        0b1100 => Ok(Id::QUOU),
        0b1101 => Ok(Id::QUOS),
        0b1110 => Ok(Id::REMU),
        0b1111 => Ok(Id::REMS),
        _ => unreachable!(),
    }
}

fn rst3(opcode: u32) -> Result<Id, Error> {
    let op2 = mask!(opcode, 4, 20);

    match op2 {
        0b0000 => Ok(Id::RSR),
        0b0001 => Ok(Id::WSR),
        0b0010 => Ok(Id::SEXT),
        0b0011 => Ok(Id::CLAMPS),
        0b0100 => Ok(Id::MIN),
        0b0101 => Ok(Id::MAX),
        0b0110 => Ok(Id::MINU),
        0b0111 => Ok(Id::MAXU),
        0b1000 => Ok(Id::MOVEQZ),
        0b1001 => Ok(Id::MOVNEZ),
        0b1010 => Ok(Id::MOVLTZ),
        0b1011 => Ok(Id::MOVGEZ),
        0b1100 => Ok(Id::MOVF),
        0b1101 => Ok(Id::MOVT),
        0b1110 => Ok(Id::RUR),
        0b1111 => Ok(Id::WUR),
        _ => unreachable!(),
    }
}

fn cust0(_: u32) -> Result<Id, Error> {
    unimplemented!();
}
fn cust1(_: u32) -> Result<Id, Error> {
    unimplemented!();
}
fn lscx(_: u32) -> Result<Id, Error> {
    unimplemented!();
}
fn lsc4(_: u32) -> Result<Id, Error> {
    unimplemented!();
}
fn fp0(_: u32) -> Result<Id, Error> {
    unimplemented!();
}
fn fp1(_: u32) -> Result<Id, Error> {
    unimplemented!();
}
fn lsai(_: u32) -> Result<Id, Error> {
    unimplemented!();
}
fn lsci(_: u32) -> Result<Id, Error> {
    unimplemented!();
}
fn mac16(_: u32) -> Result<Id, Error> {
    unimplemented!();
}
fn calln(_: u32) -> Result<Id, Error> {
    unimplemented!();
}

fn si(opcode: u32) -> Result<Id, Error> {
    let n = mask!(opcode, 2, 4);

    match n {
        0b00 => Ok(Id::J),
        0b01 => Ok(Id::BZ),
        0b10 => bi0(opcode),
        0b11 => bi1(opcode),
        _ => unreachable!(),
    }
}

fn bi0(opcode: u32) -> Result<Id, Error> {
    let m = mask!(opcode, 2, 6);

    match m {
        0b00 => Ok(Id::BEQI),
        0b01 => Ok(Id::BNEI),
        0b10 => Ok(Id::BLTI),
        0b11 => Ok(Id::BGEI),
        _ => unreachable!(),
    }
}

fn bi1(opcode: u32) -> Result<Id, Error> {
    let m = mask!(opcode, 2, 6);

    match m {
        0b00 => Ok(Id::ENTRY),
        0b01 => b1(opcode),
        0b10 => Ok(Id::BLTUI),
        0b11 => Ok(Id::BGEUI),
        _ => unreachable!(),
    }
}

fn b1(opcode: u32) -> Result<Id, Error> {
    let r = mask!(opcode, 2, 12);

    match r {
        0b0000 => Ok(Id::BF),
        0b0001 => Ok(Id::BT),
        0b0010..=0b0111 => Err(Error::Reserved),
        0b1000 => Ok(Id::LOOP),
        0b1001 => Ok(Id::LOOPNEZ),
        0b1010 => Ok(Id::LOOPGTZ),
        0b1011..=0b1111 => Err(Error::Reserved),
        _ => unreachable!(),
    }
}

fn b(_: u32) -> Result<Id, Error> {
    unimplemented!();
}

fn st2(_: u32) -> Result<Id, Error> {
    unimplemented!();
}
fn st3(opcode: u32) -> Result<Id, Error> {
    let r = mask!(opcode, 4, 12);

    match r {
        0b0000 => Ok(Id::MOV_N),
        0b0001..=0b1110 => Err(Error::Reserved),
        0b1111 => st3_n(opcode), /* TODO: check for s=0 */
        _ => unreachable!(),
    }
}

fn st3_n(opcode: u32) -> Result<Id, Error> {
    let t = mask!(opcode, 4, 4);

    match t {
        0b0000 => Ok(Id::RET_N),
        0b0001 => Ok(Id::RETW_N),
        0b0010 => Ok(Id::BREAK_N),
        0b0011 => Ok(Id::NOP_N),
        _ => Err(Error::Reserved),
    }
}
