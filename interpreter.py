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
    # Base class for all Abstract Syntax Tree nodes
    pass


class BinOp(AST):
    # Represents a binary operation (e.g., addition, multiplication)
    def __init__(self, op, children):
        self.token = self.op = op
        self.children = children  # List of children nodes


class Num(AST):
    # Represents a number in the AST
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

    def expr(self):
        # Parse an expression in prefix notation: (op expr expr ...)
        token = self.current_token

        if token.type == Token.LPAREN:
            self.consume(Token.LPAREN)
            operator = self.current_token
            self.consume(operator.type)  # Consume the operator

            children = []
            while self.current_token.type != Token.RPAREN:
                if self.current_token.type == Token.LPAREN:
                    children.append(self.expr())
                elif self.current_token.type == Token.INTEGER:
                    children.append(Num(self.current_token))
                    self.consume(Token.INTEGER)
                else:
                    self.error()

            self.consume(Token.RPAREN)
            return BinOp(op=operator, children=children)
        elif token.type == Token.INTEGER:
            self.consume(Token.INTEGER)
            return Num(token)
        self.error()


class NodeVisitor:
    # Base class for visiting nodes in the AST
    def visit(self, node):
        method_name = 'visit_' + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise Exception('No visit_{} method'.format(type(node).__name__))


class Interpreter(NodeVisitor):
    # Visits AST nodes to evaluate the expression
    def __init__(self, parser):
        self.parser = parser

    def visit_BinOp(self, node):
        operation = node.op.type
        values = [self.visit(child) for child in node.children]

        if operation == Token.PLUS:
            return sum(values)
        elif operation == Token.MINUS:
            if len(values) == 1:  # Unary negation
                return -values[0]
            return values[0] - sum(values[1:])
        elif operation == Token.MUL:
            result = 1
            for v in values:
                result *= v
            return result
        elif operation == Token.DIV:
            result = values[0]
            for v in values[1:]:
                result /= v
            return result

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
