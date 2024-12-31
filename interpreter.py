class Token:
    # Token types for integers, operators, parentheses, and end-of-file
    INTEGER = 'INTEGER'
    PLUS = 'PLUS'
    MINUS = 'MINUS'
    MUL = 'MUL'
    DIV = 'DIV'
    LPAREN = 'LPAREN'
    RPAREN = 'RPAREN'
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

            if self.current_char == '*':
                self.advance()
                return Token(Token.MUL, '*')

            if self.current_char == '/':
                self.advance()
                return Token(Token.DIV, '/')

            if self.current_char == '(':
                self.advance()
                return Token(Token.LPAREN, '(')

            if self.current_char == ')':
                self.advance()
                return Token(Token.RPAREN, ')')

            self.error()

        return Token(Token.EOF, None)  # No more input


class Parser:
    # Ensures the stream of tokens follows valid arithmetic syntax
    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()

    def error(self):
        raise Exception('Invalid syntax')

    def consume(self, token_type):
        # Consume the current token if it matches the expected type
        if self.current_token.type == token_type:
            self.current_token = self.lexer.get_next_token()
        else:
            self.error()

    def factor(self):
        # Parse a factor: INTEGER or parenthesized expression
        token = self.current_token
        if token.type == Token.INTEGER:
            self.consume(Token.INTEGER)
            return token.value
        elif token.type == Token.LPAREN:
            self.consume(Token.LPAREN)
            result = self.expr()
            self.consume(Token.RPAREN)
            return result
        self.error()

    def term(self):
        # Parse a term: factor (MUL | DIV factor)*
        result = self.factor()
        while self.current_token.type in (Token.MUL, Token.DIV):
            token = self.current_token
            if token.type == Token.MUL:
                self.consume(Token.MUL)
                result *= self.factor()
            elif token.type == Token.DIV:
                self.consume(Token.DIV)
                result /= self.factor()
        return result

    def expr(self):
        # Parse an expression: term (PLUS | MINUS term)*
        result = self.term()
        while self.current_token.type in (Token.PLUS, Token.MINUS):
            token = self.current_token
            if token.type == Token.PLUS:
                self.consume(Token.PLUS)
                result += self.term()
            elif token.type == Token.MINUS:
                self.consume(Token.MINUS)
                result -= self.term()
        return result


class Interpreter:
    # Parsing and evaluates the result
    def __init__(self, parser):
        self.parser = parser

    def interpret(self):
        return self.parser.expr()


def main():
    # Used for user input and evaluation
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
