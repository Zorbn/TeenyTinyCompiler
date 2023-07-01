use std::io::Write;

#[derive(Clone, Copy)]
pub enum EmitRegion {
    Preprocessor,
    Prototype,
    Body,
}

const EMIT_REGION_COUNT: usize = 3;

pub struct Emitter {
    emit_regions: [String; EMIT_REGION_COUNT],
    is_region_at_line_start: [bool; EMIT_REGION_COUNT],
    emit_region_indent_levels: [usize; EMIT_REGION_COUNT],
    current_region: EmitRegion,
}

impl Emitter {
    pub fn new() -> Self {
        Self {
            emit_regions: Default::default(),
            is_region_at_line_start: [true; EMIT_REGION_COUNT],
            emit_region_indent_levels: [0; EMIT_REGION_COUNT],
            current_region: EmitRegion::Body,
        }
    }

    pub fn set_region(&mut self, region: EmitRegion) {
        self.current_region = region;
    }

    pub fn emit(&mut self, code: &str) {
        let region_i = self.current_region as usize;
        let emit_region = &mut self.emit_regions[region_i];
        let is_at_line_start = &mut self.is_region_at_line_start[region_i];
        let indent_level = self.emit_region_indent_levels[region_i];

        if *is_at_line_start {
            for _ in 0..indent_level {
                emit_region.push('\t');
            }
            *is_at_line_start = false;
        }

        emit_region.push_str(code);
    }

    pub fn emit_line(&mut self, code: &str) {
        self.emit(code);

        let region_i = self.current_region as usize;
        self.emit_regions[region_i].push('\n');
        self.is_region_at_line_start[region_i] = true;
    }

    pub fn indent(&mut self) {
        let region_i = self.current_region as usize;
        self.emit_region_indent_levels[region_i] += 1;
    }

    pub fn unindent(&mut self) {
        let region_i = self.current_region as usize;
        self.emit_region_indent_levels[region_i] -= 1;
    }

    pub fn write_file(&self, path: &str) -> std::io::Result<()> {
        let mut output = std::fs::File::create(path)?;
        writeln!(
            output,
            "{}",
            self.emit_regions[EmitRegion::Preprocessor as usize]
        )?;
        writeln!(
            output,
            "{}",
            self.emit_regions[EmitRegion::Prototype as usize]
        )?;
        write!(
            output,
            "{}",
            self.emit_regions[EmitRegion::Body as usize]
        )?;

        Ok(())
    }
}
