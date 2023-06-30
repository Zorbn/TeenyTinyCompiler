class Environment:
    def __init__(self, enclosing):
        self.enclosing = enclosing
        self.symbols = {}

    def has_symbol(self, symbol):
        environment = self

        while environment is not None:
            if symbol in environment.symbols:
                return True

            environment = environment.enclosing

        return False

    def add_symbol(self, symbol, value_type):
        self.symbols[symbol] = value_type

    def get_symbol_type(self, symbol):
        environment = self

        while environment is not None:
            if symbol in environment.symbols:
                return environment.symbols[symbol]

            environment = environment.enclosing

        return None