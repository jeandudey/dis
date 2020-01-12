use falcon::analysis::calling_convention::CallingConvention;
use falcon::architecture::{Architecture, Endian};
use falcon::il::Scalar;
use falcon::translator::Translator;

#[derive(Debug, Clone)]
pub struct Mcu {
    pub nareg: u8,
    pub extended_l32r: bool,
}

impl Mcu {
    pub fn esp32() -> Mcu {
        Mcu {
            nareg: 64,
            extended_l32r: false,
        }
    }

    pub fn window_base_size(&self) -> usize {
        // log2(NAREG / 4)
        match self.nareg {
            32 => 3,
            64 => 4,
            _ => f64::from(self.nareg / 4).log2() as usize,
        }
    }
}

#[derive(Debug)]
pub struct XtensaLx6 {
    mcu: Mcu
}

impl XtensaLx6 {
    pub fn new(mcu: Mcu) -> XtensaLx6 {
        XtensaLx6 { mcu }
    }
}

impl Architecture for XtensaLx6 {
    fn name(&self) -> &str {
        &"xtensa-lx6"
    }
    fn endian(&self) -> Endian {
        // TODO: support big endian xtensa.
        Endian::Little
    }
    fn translator(&self) -> Box<dyn Translator> {
        Box::new(crate::translator::XtensaLx6::new(self.mcu.clone()))
    }
    fn calling_convention(&self) -> CallingConvention {
        // TODO: CallingConvention on falcon should be more general
        // and flexible for other implementations.
        unimplemented!();
    }
    fn stack_pointer(&self) -> Scalar {
        // TODO: what's the stack pointer register on the xtensa processor?
        unimplemented!();
    }
    fn word_size(&self) -> usize {
        64
    }
    fn box_clone(&self) -> Box<dyn Architecture> {
        // XXX: How to make this work on 2018 edition?
        unimplemented!()
    }
}
