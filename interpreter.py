class Token:
    # Token types for integers, operators, parentheses, assignments, and EOF
    INTEGER = 'INTEGER'
    PLUS = 'PLUS'
    MINUS = 'MINUS'
    MUL = 'MUL'
    DIV = 'DIV'
    LPAREN = 'LPAREN'
    RPAREN = 'RPAREN'
    BEGIN = 'BEGIN'
    END = 'END'
    ASSIGN = 'ASSIGN'
    SEMI = 'SEMI'
    DOT = 'DOT'
    ID = 'ID'
    EOF = 'EOF'

    def __init__(self, type_, value):
        # Initialize a token with its type and value
        self.type = type_
        self.value = value

    def __repr__(self):
        # String representation of the token for debugging
        return f"Token({self.type}, {repr(self.value)})"


class Lexer:
    # Breaks input string into a stream of tokens
    RESERVED_KEYWORDS = {
        'BEGIN': Token('BEGIN', 'BEGIN'),
        'END': Token('END', 'END'),
        'DIV': Token('DIV', 'DIV'),
    }

    def __init__(self, text):
        # Initialize the lexer with input text
        self.text = text
        self.pos = 0
        self.current_char = self.text[self.pos] if self.text else None

    def error(self):
        # Raise an error for invalid characters
        raise Exception('Invalid character')

    def advance(self):
        # Move to the next character in the input
        self.pos += 1
        self.current_char = self.text[self.pos] if self.pos < len(self.text) else None

    def skip_whitespace(self):
        # Ignore spaces in the input
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    def peek(self):
        # Look ahead at the next character without consuming it
        peek_pos = self.pos + 1
        return self.text[peek_pos] if peek_pos < len(self.text) else None

    def _id(self):
        # Handle identifiers and reserved keywords (case insensitive)
        result = ''
        while self.current_char is not None and (self.current_char.isalnum() or self.current_char == '_'):
            result += self.current_char
            self.advance()
        result_upper = result.upper()
        return self.RESERVED_KEYWORDS.get(result_upper, Token(Token.ID, result))

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

            if self.current_char.isalpha() or self.current_char == '_':
                return self._id()

            if self.current_char.isdigit():
                return Token(Token.INTEGER, self.integer())

            if self.current_char == ':' and self.peek() == '=':
                self.advance()
                self.advance()
                return Token(Token.ASSIGN, ':=')

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

            if self.current_char == ';':
                self.advance()
                return Token(Token.SEMI, ';')

            if self.current_char == '.':
                self.advance()
                return Token(Token.DOT, '.')

            self.error()

        return Token(Token.EOF, None)  # No more input


class AST:
    # Base class for all Abstract Syntax Tree nodes
    pass


class Compound(AST):
    # Represents a 'BEGIN ... END' block
    def __init__(self):
        # Initialize with a list of child nodes
        self.children = []


class Assign(AST):
    # Represents an assignment statement
    def __init__(self, left, op, right):
        # Left is the variable, op is ':=', and right is the expression
        self.left = left
        self.token = self.op = op
        self.right = right


class Var(AST):
    # Represents a variable
    def __init__(self, token):
        # The token contains the variable name
        self.token = token
        self.value = token.value


class NoOp(AST):
    # Represents an empty statement
    pass


class BinOp(AST):
    # Represents a binary operation (e.g., addition, multiplication, division)
    def __init__(self, op, children):
        # Operator and a list of child nodes
        self.token = self.op = op
        self.children = children


class Num(AST):
    # Represents a number in the AST
    def __init__(self, token):
        # Token contains the integer value
        self.token = token
        self.value = token.value


class Parser:
    # Ensures the stream of tokens follows valid arithmetic syntax
    def __init__(self, lexer):
        # Initialize with a lexer
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()

    def error(self):
        # Raise an error for invalid syntax
        raise Exception('Invalid syntax')

    def consume(self, token_type):
        # Consume the current token if it matches the expected type
        if self.current_token.type == token_type:
            self.current_token = self.lexer.get_next_token()
        else:
            self.error()

    def program(self):
        # program : compound_statement DOT
        node = self.compound_statement()
        self.consume(Token.DOT)
        return node

    def compound_statement(self):
        # compound_statement : BEGIN statement_list END
        self.consume(Token.BEGIN)
        nodes = self.statement_list()
        self.consume(Token.END)

        root = Compound()
        for node in nodes:
            root.children.append(node)

        return root

    def statement_list(self):
        # statement_list : statement | statement SEMI statement_list
        node = self.statement()
        results = [node]

        while self.current_token.type == Token.SEMI:
            self.consume(Token.SEMI)
            results.append(self.statement())

        return results

    def statement(self):
        # statement : compound_statement | assignment_statement | empty
        if self.current_token.type == Token.BEGIN:
            node = self.compound_statement()
        elif self.current_token.type == Token.ID:
            node = self.assignment_statement()
        else:
            node = self.empty()
        return node

    def assignment_statement(self):
        # assignment_statement : variable ASSIGN expr
        left = self.variable()
        token = self.current_token
        self.consume(Token.ASSIGN)
        right = self.expr()
        return Assign(left, token, right)

    def variable(self):
        # variable : ID
        node = Var(self.current_token)
        self.consume(Token.ID)
        return node

    def empty(self):
        # An empty production
        return NoOp()

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
                elif self.current_token.type == Token.ID:
                    children.append(self.variable())
                else:
                    self.error()

            self.consume(Token.RPAREN)
            return BinOp(op=operator, children=children)

        elif token.type == Token.INTEGER:
            self.consume(Token.INTEGER)
            return Num(token)

        elif token.type == Token.ID:
            return self.variable()

        self.error()


class NodeVisitor:
    # Base class for visiting nodes in the AST
    def visit(self, node):
        # Dynamically find the correct visit method
        method_name = 'visit_' + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        # Raise an error if no specific visit method exists
        raise Exception('No visit_{} method'.format(type(node).__name__))


class Interpreter(NodeVisitor):
    # Visits AST nodes to evaluate the expression
    def __init__(self, parser):
        # Initialize with a parser and symbol table
        self.parser = parser
        self.GLOBAL_SCOPE = {}

    def visit_Compound(self, node):
        # Visit each child node in the compound statement
        for child in node.children:
            self.visit(child)

    def visit_Assign(self, node):
        # Evaluate the right-hand side and assign it to the variable
        var_name = node.left.value
        self.GLOBAL_SCOPE[var_name.lower()] = self.visit(node.right)

    def visit_Var(self, node):
        # Look up the variable in the symbol table (case insensitive)
        var_name = node.value
        val = self.GLOBAL_SCOPE.get(var_name.lower())
        if val is None:
            raise NameError(repr(var_name))
        else:
            return val

    def visit_NoOp(self, node):
        # Do nothing for empty statements
        pass

    def visit_BinOp(self, node):
        # Evaluate binary operations
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
                result //= v  # Integer division
            return result

    def visit_Num(self, node):
        # Return the value of a number node
        return node.value

    def interpret(self):
        # Parse the program and evaluate it
        tree = self.parser.program()
        self.visit(tree)
        return self.GLOBAL_SCOPE


def main():
    # Used for user input and validation
    import sys
    text = open(sys.argv[1], 'r').read()

    lexer = Lexer(text)
    parser = Parser(lexer)
    interpreter = Interpreter(parser) 
    result = interpreter.interpret()
    print(interpreter.GLOBAL_SCOPE)


if __name__ == '__main__':
    main()
