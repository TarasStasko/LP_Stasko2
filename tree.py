import math
import re
from typing import Union, Dict, Tuple, Optional


class ASTNode:
    pass


class Number(ASTNode):
    def __init__(self, value: float):
        self.value = value

    def __repr__(self):
        return f"{self.value}"


class Variable(ASTNode):
    def __init__(self, name: str):
        self.name = name

    def __repr__(self):
        return self.name


class BinOp(ASTNode):
    def __init__(self, op: str, left: ASTNode, right: ASTNode):
        self.op = op
        self.left = left
        self.right = right

    def __repr__(self):
        return f"{self.op}({self.left},{self.right})"


class UnaryOp(ASTNode):
    def __init__(self, op: str, expr: ASTNode):
        self.op = op
        self.expr = expr

    def __repr__(self):
        return f"{self.op}({self.expr})"


class Parser:
    def __init__(self):
        self.tokens = []
        self.pos = 0

    def parse(self, expression: str) -> ASTNode:
        self.tokens = self.tokenize(expression)
        self.pos = 0
        return self._parse_expression()

    def tokenize(self, expression: str) -> list:
        token_spec = [
            ('FUNCTION', r'sqrt(?=\()'),
            ('NUMBER', r'\d+(\.\d*)?'),
            ('VARIABLE', r'[a-zA-Z_][a-zA-Z0-9_]*'),
            ('OPERATOR', r'[\+\-\*/%\^]'),
            ('LPAREN', r'\('),
            ('RPAREN', r'\)'),
            ('SKIP', r'[ \t\n]'),
        ]
        token_regex = '|'.join(f'(?P<{name}>{pattern})' for name, pattern in token_spec)
        tokens = []
        for match in re.finditer(token_regex, expression):
            kind = match.lastgroup
            value = match.group()
            if kind == 'SKIP':
                continue
            tokens.append((kind, value))
        return tokens

    def _current_token(self):
        return self.tokens[self.pos] if self.pos < len(self.tokens) else None

    def _consume(self, expected_kind=None):
        token = self._current_token()
        if expected_kind and token[0] != expected_kind:
            raise SyntaxError(f"Expected {expected_kind}, got {token[0]}")
        self.pos += 1
        return token[1]

    def _parse_expression(self) -> ASTNode:
        return self._parse_binop()

    def _parse_binop(self, precedence=0) -> ASTNode:
        operators = [
            [('+', 0, 'left'), ('-', 0, 'left')],
            [('*', 1, 'left'), ('/', 1, 'left'), ('%', 1, 'left')],
            [('^', 2, 'right')]
        ]

        if precedence >= len(operators):
            return self._parse_unary()

        left = self._parse_binop(precedence + 1)

        while True:
            token = self._current_token()
            if not token or token[0] != 'OPERATOR':
                break

            op_info = None
            for op, prec, assoc in operators[precedence]:
                if token[1] == op:
                    op_info = (op, prec, assoc)
                    break

            if not op_info:
                break

            op, _, assoc = op_info
            self._consume('OPERATOR')
            right = self._parse_binop(precedence + 1 if assoc == 'right' else precedence)
            left = BinOp(self._map_op(op), left, right)

        return left

    def _map_op(self, op: str) -> str:
        op_map = {
            '+': 'add',
            '-': 'sub',
            '*': 'mul',
            '/': 'dvd',
            '^': 'exp',
            '%': 'mod'
        }
        return op_map.get(op, op)

    def _parse_unary(self) -> ASTNode:
        token = self._current_token()
        if token and token[0] == 'FUNCTION' and token[1] == 'sqrt':
            self._consume('FUNCTION')
            self._consume('LPAREN')
            expr = self._parse_expression()
            self._consume('RPAREN')
            return UnaryOp('sqrt', expr)
        return self._parse_primary()

    def _parse_primary(self) -> ASTNode:
        token = self._current_token()
        if not token:
            raise SyntaxError("Unexpected end of input")

        if token[0] == 'NUMBER':
            value = float(self._consume('NUMBER'))
            return Number(value)
        elif token[0] == 'VARIABLE':
            name = self._consume('VARIABLE')
            return Variable(name)
        elif token[0] == 'LPAREN':
            self._consume('LPAREN')
            expr = self._parse_expression()
            self._consume('RPAREN')
            return expr
        else:
            raise SyntaxError(f"Unexpected token: {token[1]}")


class Interpreter:
    def __init__(self):
        self.env = {}

    def eval(self, node: ASTNode) -> float:
        if isinstance(node, Number):
            return node.value
        elif isinstance(node, Variable):
            if node.name not in self.env:
                raise NameError(f"Undefined variable: {node.name}")
            return self.env[node.name]
        elif isinstance(node, BinOp):
            left = self.eval(node.left)
            right = self.eval(node.right)
            if node.op == 'add':
                return left + right
            elif node.op == 'sub':
                return left - right
            elif node.op == 'mul':
                return left * right
            elif node.op == 'dvd':
                if right == 0:
                    raise ZeroDivisionError("Division by zero")
                return left / right
            elif node.op == 'exp':
                return left ** right
            elif node.op == 'mod':
                if right == 0:
                    raise ZeroDivisionError("Modulo by zero")
                return left % right
        elif isinstance(node, UnaryOp):
            if node.op == 'sqrt':
                val = self.eval(node.expr)
                if val < 0:
                    raise ValueError("Negative sqrt argument")
                return math.sqrt(val)
        raise ValueError(f"Unknown node type: {type(node)}")


def test_expression(expr: str):
    try:
        parser = Parser()
        interpreter = Interpreter()

        # Parse
        ast = parser.parse(expr)
        print(f"Вираз: {expr}")
        print(f"AST: {ast}")


        result = interpreter.eval(ast)
        print(f"Результат: {result}")
        print("-" * 40)
    except Exception as e:
        print(f"Помилка у виразі '{expr}': {str(e)}")
        print("-" * 40)


# Тестові вирази
test_expressions = [
    "sqrt(9) * 2^3 + 2 / 5 % 3",
    "(2 + 3) * 4",
    "2.5 * (3 + 1)",
    "5 % 2",
    "sqrt(16) * 2"
]

if __name__ == "__main__":
    for expr in test_expressions:
        test_expression(expr)