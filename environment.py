class Environment:
    def __init__(self, enclosing):
        self.enclosing = enclosing
        self.symbols = set()

    def has_symbol(self, symbol):
        environment = self

        while environment is not None:
            if symbol in environment.symbols:
                return True

            environment = environment.enclosing

        return False

    def add_symbol(self, symbol):
        self.symbols.add(symbol)