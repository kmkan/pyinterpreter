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


class AST:
    pass


class BinOp(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right


class Num(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value


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
            return Num(token)
        elif token.type == Token.LPAREN:
            self.consume(Token.LPAREN)
            node = self.expr()
            self.consume(Token.RPAREN)
            return node
        self.error()

    def term(self):
        # Parse a term: factor (MUL | DIV factor)*
        node = self.factor()
        while self.current_token.type in (Token.MUL, Token.DIV):
            token = self.current_token
            if token.type == Token.MUL:
                self.consume(Token.MUL)
            elif token.type == Token.DIV:
                self.consume(Token.DIV)
            node = BinOp(left=node, op=token, right=self.factor())
        return node

    def expr(self):
        # Parse an expression: term (PLUS | MINUS term)*
        node = self.term()
        while self.current_token.type in (Token.PLUS, Token.MINUS):
            token = self.current_token
            if token.type == Token.PLUS:
                self.consume(Token.PLUS)
            elif token.type == Token.MINUS:
                self.consume(Token.MINUS)
            node = BinOp(left=node, op=token, right=self.term())
        return node


class NodeVisitor:
    def visit(self, node):
        method_name = 'visit_' + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise Exception('No visit_{} method'.format(type(node).__name__))


class Interpreter(NodeVisitor):
    def __init__(self, parser):
        self.parser = parser

    def visit_BinOp(self, node):
        if node.op.type == Token.PLUS:
            return self.visit(node.left) + self.visit(node.right)
        elif node.op.type == Token.MINUS:
            return self.visit(node.left) - self.visit(node.right)
        elif node.op.type == Token.MUL:
            return self.visit(node.left) * self.visit(node.right)
        elif node.op.type == Token.DIV:
            return self.visit(node.left) / self.visit(node.right)

    def visit_Num(self, node):
        return node.value

    def interpret(self):
        tree = self.parser.expr()
        return self.visit(tree)


def main():
    # Used for user input and evaluation
    while True:
        try:
            text = input(">>> ")
            if not text.strip():
                continue
            lexer = Lexer(text)  # Tokenize input
            parser = Parser(lexer)  # Parse tokens into AST
            interpreter = Interpreter(parser)  # Interpret AST
            result = interpreter.interpret()
            print(f"{result}")
        except Exception as e:
            print(f"Error: {e}")


if __name__ == "__main__":
    main()
