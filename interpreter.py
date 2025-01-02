class Token:
    # Token types for integers, operators, types, and program structure
    INTEGER_CONST = 'INTEGER_CONST'
    REAL_CONST = 'REAL_CONST'
    INTEGER = 'INTEGER'
    REAL = 'REAL'
    PLUS = 'PLUS'
    MINUS = 'MINUS'
    MUL = 'MUL'
    INTEGER_DIV = 'INTEGER_DIV'
    FLOAT_DIV = 'FLOAT_DIV'
    LPAREN = 'LPAREN'
    RPAREN = 'RPAREN'
    ASSIGN = 'ASSIGN'
    SEMI = 'SEMI'
    DOT = 'DOT'
    COLON = 'COLON'
    COMMA = 'COMMA'
    ID = 'ID'
    PROGRAM = 'PROGRAM'
    VAR = 'VAR'
    BEGIN = 'BEGIN'
    END = 'END'
    EOF = 'EOF'

    def __init__(self, type_, value):
        # Initialize a token with its type and value
        self.type = type_
        self.value = value

    def __repr__(self):
        # String representation of the token for debugging
        return f"Token({self.type}, {repr(self.value)})"


class Lexer:
    RESERVED_KEYWORDS = {
        'PROGRAM': Token('PROGRAM', 'PROGRAM'),
        'VAR': Token('VAR', 'VAR'),
        'DIV': Token('INTEGER_DIV', 'DIV'),
        'INTEGER': Token('INTEGER', 'INTEGER'),
        'REAL': Token('REAL', 'REAL'),
        'BEGIN': Token('BEGIN', 'BEGIN'),
        'END': Token('END', 'END'),
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

    def skip_comment(self):
        # Ignore Pascal-style comments enclosed in { }
        while self.current_char != '}':
            self.advance()
        self.advance()  # Skip the closing '}'

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

    def number(self):
        # Handle integer and real (float) constants
        result = ''
        while self.current_char is not None and self.current_char.isdigit():
            result += self.current_char
            self.advance()

        if self.current_char == '.':
            result += self.current_char
            self.advance()
            while self.current_char is not None and self.current_char.isdigit():
                result += self.current_char
                self.advance()
            return Token('REAL_CONST', float(result))
        else:
            return Token('INTEGER_CONST', int(result))

    def get_next_token(self):
        # Generate the next token from input
        while self.current_char is not None:
            if self.current_char.isspace():
                self.skip_whitespace()
                continue

            if self.current_char == '{':
                self.advance()
                self.skip_comment()
                continue

            if self.current_char.isalpha() or self.current_char == '_':
                return self._id()

            if self.current_char.isdigit():
                return self.number()

            if self.current_char == ':' and self.peek() == '=':
                self.advance()
                self.advance()
                return Token(Token.ASSIGN, ':=')

            if self.current_char == ':':
                self.advance()
                return Token(Token.COLON, ':')

            if self.current_char == ',':
                self.advance()
                return Token(Token.COMMA, ',')

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
                return Token(Token.FLOAT_DIV, '/')

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

        return Token(Token.EOF, None)


class AST:
    # Base class for all Abstract Syntax Tree nodes
    pass


class Program(AST):
    # Represents the entire program
    def __init__(self, name, block):
        self.name = name
        self.block = block


class Block(AST):
    # Represents a block of declarations and compound statements
    def __init__(self, declarations, compound_statement):
        self.declarations = declarations
        self.compound_statement = compound_statement


class VarDecl(AST):
    # Represents a variable declaration
    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node


class Type(AST):
    # Represents a variable type (INTEGER or REAL)
    def __init__(self, token):
        self.token = token
        self.value = token.value


class Compound(AST):
    # Represents a BEGIN ... END block
    def __init__(self):
        self.children = []


class Assign(AST):
    # Represents an assignment statement
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right


class Var(AST):
    # Represents a variable
    def __init__(self, token):
        self.token = token
        self.value = token.value


class NoOp(AST):
    # Represents an empty statement
    pass


class BinOp(AST):
    # Represents a binary operation (e.g., addition, multiplication)
    def __init__(self, op, children):
        self.token = self.op = op
        self.children = children


class Num(AST):
    # Represents a numeric constant (integer or real)
    def __init__(self, token):
        self.token = token
        self.value = token.value


class Parser:
    # Parses the token stream and builds an Abstract Syntax Tree (AST)
    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()

    def error(self):
        raise Exception('Invalid syntax')

    def consume(self, token_type):
        if self.current_token.type == token_type:
            self.current_token = self.lexer.get_next_token()
        else:
            self.error()

    def program(self):
        # program : PROGRAM variable SEMI block DOT
        self.consume(Token.PROGRAM)
        var_node = self.variable()
        prog_name = var_node.value
        self.consume(Token.SEMI)
        block_node = self.block()
        self.consume(Token.DOT)
        return Program(prog_name, block_node)

    def block(self):
        # block : declarations compound_statement
        declarations = self.declarations()
        compound_statement = self.compound_statement()
        return Block(declarations, compound_statement)

    def declarations(self):
        # declarations : VAR (variable_declaration SEMI)+ | empty
        declarations = []
        if self.current_token.type == Token.VAR:
            self.consume(Token.VAR)
            while self.current_token.type == Token.ID:
                declarations.extend(self.variable_declaration())
                self.consume(Token.SEMI)
        return declarations

    def variable_declaration(self):
        # variable_declaration : ID (COMMA ID)* COLON type_spec
        var_nodes = [Var(self.current_token)]
        self.consume(Token.ID)

        while self.current_token.type == Token.COMMA:
            self.consume(Token.COMMA)
            var_nodes.append(Var(self.current_token))
            self.consume(Token.ID)

        self.consume(Token.COLON)
        type_node = self.type_spec()

        return [VarDecl(var_node, type_node) for var_node in var_nodes]

    def type_spec(self):
        # type_spec : INTEGER | REAL
        token = self.current_token
        if token.type == Token.INTEGER:
            self.consume(Token.INTEGER)
        else:
            self.consume(Token.REAL)
        return Type(token)

    def compound_statement(self):
        # compound_statement : BEGIN statement_list END
        self.consume(Token.BEGIN)
        nodes = self.statement_list()
        self.consume(Token.END)
        compound = Compound()
        compound.children = nodes
        return compound

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
            return self.compound_statement()
        elif self.current_token.type == Token.ID:
            return self.assignment_statement()
        else:
            return self.empty()

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
        # empty :
        return NoOp()

    def expr(self):
        # expr : term ((PLUS | MINUS) term)* (prefix notation)
        token = self.current_token

        if token.type == Token.LPAREN:
            self.consume(Token.LPAREN)
            operator = self.current_token
            self.consume(operator.type)

            children = []
            while self.current_token.type != Token.RPAREN:
                if self.current_token.type == Token.LPAREN:
                    children.append(self.expr())
                elif self.current_token.type in (Token.INTEGER_CONST, Token.REAL_CONST):
                    children.append(Num(self.current_token))
                    self.consume(self.current_token.type)
                elif self.current_token.type == Token.ID:
                    children.append(self.variable())
                else:
                    self.error()

            self.consume(Token.RPAREN)
            return BinOp(op=operator, children=children)

        elif token.type in (Token.INTEGER_CONST, Token.REAL_CONST):
            self.consume(token.type)
            return Num(token)

        elif token.type == Token.ID:
            return self.variable()

        self.error()


class NodeVisitor:
    # Visits AST nodes for evaluation
    def visit(self, node):
        method_name = 'visit_' + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise Exception(f'No visit_{type(node).__name__} method')


class Interpreter(NodeVisitor):
    # Visits AST nodes to interpret the program
    def __init__(self, parser):
        self.parser = parser
        self.GLOBAL_SCOPE = {}

    def visit_Program(self, node):
        self.visit(node.block)

    def visit_Block(self, node):
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_VarDecl(self, node):
        pass  

    def visit_Type(self, node):
        pass  

    def visit_Compound(self, node):
        for child in node.children:
            self.visit(child)

    def visit_Assign(self, node):
        var_name = node.left.value
        self.GLOBAL_SCOPE[var_name.lower()] = self.visit(node.right)

    def visit_Var(self, node):
        var_name = node.value
        value = self.GLOBAL_SCOPE.get(var_name.lower())
        if value is None:
            raise NameError(f'Variable {repr(var_name)} is not defined')
        return value

    def visit_NoOp(self, node):
        pass

    def visit_BinOp(self, node):
        operation = node.op.type
        values = [self.visit(child) for child in node.children]

        if operation == Token.PLUS:
            return sum(values)
        elif operation == Token.MINUS:
            return values[0] - sum(values[1:])
        elif operation == Token.MUL:
            result = 1
            for v in values:
                result *= v
            return result
        elif operation == Token.INTEGER_DIV:
            return values[0] // values[1]
        elif operation == Token.FLOAT_DIV:
            return values[0] / values[1]

    def visit_Num(self, node):
        return node.value

    def interpret(self):
        tree = self.parser.program()
        self.visit(tree)
        return self.GLOBAL_SCOPE


def main():
    import sys
    text = open(sys.argv[1], 'r').read()

    lexer = Lexer(text)
    parser = Parser(lexer)
    interpreter = Interpreter(parser)
    result = interpreter.interpret()
    print(interpreter.GLOBAL_SCOPE)


if __name__ == '__main__':
    main()
