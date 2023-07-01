#[derive(Clone, Copy)]
pub enum EmitRegion {
    Preprocessor,
    Prototype,
    Function,
    Header,
    Code,
}

const EMIT_REGION_COUNT: usize = 5;

pub struct Emitter {
    emit_regions: [String; EMIT_REGION_COUNT],
}

impl Emitter {

}