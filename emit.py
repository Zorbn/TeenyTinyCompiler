# TODO: Concatenating all of these strings may waste some time,
# some sort of string builder might do a better job here.
class Emitter:
    def __init__(self, full_path):
        self.full_path = full_path
        self.header = ""
        self.code = ""

    def emit(self, code):
        self.code += code

    def emit_line(self, code):
        self.code += code + "\n"

    def header_line(self, code):
        self.header += code + "\n"

    def write_file(self):
        with open(self.full_path, "w") as output_file:
            output_file.write(self.header + self.code)