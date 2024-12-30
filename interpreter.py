class Token:
    # Token types for integers, operators, and end-of-file
    INTEGER = 'INTEGER'
    PLUS = 'PLUS'
    MINUS = 'MINUS'
    EOF = 'EOF'

    def __init__(self, type_, value):
        self.type = type_
        self.value = value

    def __repr__(self):
        return f"Token({self.type}, {repr(self.value)})"


class Lexer:
    # Breaks input string into a stream of tokens
    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.current_char = self.text[self.pos] if self.text else None

    def error(self):
        raise Exception('Invalid character')

    def advance(self):
        # Move to the next character
        self.pos += 1
        self.current_char = self.text[self.pos] if self.pos < len(self.text) else None

    def skip_whitespace(self):
        # Ignore spaces in the input
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    def integer(self):
        # Build an integer from consecutive digits
        result = ''
        while self.current_char is not None and self.current_char.isdigit():
            result += self.current_char
            self.advance()
        return int(result)

    def get_next_token(self):
        # Generate the next token from input
        while self.current_char is not None:
            if self.current_char.isspace():
                self.skip_whitespace()
                continue

            if self.current_char.isdigit():
                return Token(Token.INTEGER, self.integer())

            if self.current_char == '+':
                self.advance()
                return Token(Token.PLUS, '+')

            if self.current_char == '-':
                self.advance()
                return Token(Token.MINUS, '-')

            self.error()

        return Token(Token.EOF, None)  # No more input


class Parser:
    # Ensures the stream of tokens follows valid arithmetic syntax
    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()

    def error(self):
        raise Exception('Invalid syntax')

    def eat(self, token_type):
        # Consume the current token if it matches the expected type
        if self.current_token.type == token_type:
            self.current_token = self.lexer.get_next_token()
        else:
            self.error()

    def expr(self):
        # Parse and evaluate an expression (supports + and -)
        result = self.term()  # Start with the first number
        while self.current_token.type in (Token.PLUS, Token.MINUS):
            token = self.current_token
            if token.type == Token.PLUS:
                self.eat(Token.PLUS)
                result += self.term()
            elif token.type == Token.MINUS:
                self.eat(Token.MINUS)
                result -= self.term()
        return result

    def term(self):
        # Parse an integer term
        token = self.current_token
        self.eat(Token.INTEGER)
        return token.value


class Interpreter:
    # Drives the parsing and evaluates the result
    def __init__(self, parser):
        self.parser = parser

    def interpret(self):
        return self.parser.expr()


def main():
    # Entry point for user input and evaluation
    while True:
        try:
            text = input(">>> ")
            if not text.strip():
                continue
            lexer = Lexer(text)  # Tokenize input
            parser = Parser(lexer)  # Parse tokens
            interpreter = Interpreter(parser)  # Interpret parsed result
            result = interpreter.interpret()
            print(f"Result: {result}")
        except Exception as e:
            print(f"Error: {e}")


if __name__ == "__main__":
    main()
