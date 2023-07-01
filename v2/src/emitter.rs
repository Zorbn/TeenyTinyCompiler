use std::io::Write;

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
    current_region: EmitRegion,
}

impl Emitter {
    pub fn new() -> Self {
        Self {
            emit_regions: Default::default(),
            current_region: EmitRegion::Code,
        }
    }

    pub fn set_region(&mut self, region: EmitRegion) {
        self.current_region = region;
    }

    pub fn emit(&mut self, code: &str) {
        self.emit_regions[self.current_region as usize].push_str(code);
    }

    pub fn emit_line(&mut self, code: &str) {
        self.emit(code);
        self.emit_regions[self.current_region as usize].push('\n');
    }

    pub fn write_file(&self, path: &str) -> std::io::Result<()> {
        let mut output = std::fs::File::create(path)?;
        write!(output, "{}", self.emit_regions[EmitRegion::Preprocessor as usize])?;
        write!(output, "{}", self.emit_regions[EmitRegion::Prototype as usize])?;
        write!(output, "{}", self.emit_regions[EmitRegion::Function as usize])?;
        write!(output, "{}", self.emit_regions[EmitRegion::Header as usize])?;
        write!(output, "{}", self.emit_regions[EmitRegion::Code as usize])?;

        Ok(())
    }
}