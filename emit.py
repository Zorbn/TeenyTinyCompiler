import enum

class EmitRegion(enum.Enum):
    PREPROCESSOR = 0
    PROTOTYPE = 1
    FUNCTION = 2
    HEADER = 3
    CODE = 4
    BUFFERED = 5

# TODO: Concatenating all of these strings may waste some time,
# some sort of string builder might do a better job here.
class Emitter:
    def __init__(self, full_path):
        self.full_path = full_path
        self.emit_regions = {}
        self.emit_regions[EmitRegion.PREPROCESSOR] = ""
        self.emit_regions[EmitRegion.PROTOTYPE] = ""
        self.emit_regions[EmitRegion.FUNCTION] = ""
        self.emit_regions[EmitRegion.HEADER] = ""
        self.emit_regions[EmitRegion.CODE] = ""
        self.emit_regions[EmitRegion.BUFFERED] = ""
        self.current_region = EmitRegion.CODE

    def set_region(self, region):
        self.current_region = region

    def match_region(self, region):
        return self.current_region == region

    def emit(self, code):
        self.emit_regions[self.current_region] += code

    def emit_line(self, code):
        self.emit_regions[self.current_region] += code + "\n"

    def clear_buffer(self):
        self.emit_regions[EmitRegion.BUFFERED] = ""

    def get_buffer(self):
        return self.emit_regions[EmitRegion.BUFFERED]

    def write_file(self):
        with open(self.full_path, "w") as output_file:
            output_file.write(self.emit_regions[EmitRegion.PREPROCESSOR])
            output_file.write(self.emit_regions[EmitRegion.PROTOTYPE])
            output_file.write(self.emit_regions[EmitRegion.FUNCTION])
            output_file.write(self.emit_regions[EmitRegion.HEADER])
            output_file.write(self.emit_regions[EmitRegion.CODE])