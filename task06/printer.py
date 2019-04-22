from model import *


def print_expr(expr):
    return expr.accept(PrettyPrinter(depth=0, is_statement=False))


class PrettyPrinter(ASTNodeVisitor):
    def visit_block(self, block):
        program = '{\n'
        for statement in block or []:
            program += statement.accept(
                           PrettyPrinter(self.depth + 1, is_statement=True))
        program += self.indent + '}\n'
        return program

    def __init__(self, depth, is_statement):
        assert is_statement or depth == 0
        self.indent = '    ' * depth
        self.depth = depth
        self.is_statement = is_statement

    def wrap(self, program):
        if not self.is_statement:
            return program
        if program.endswith('}\n'):
            return self.indent + program
        return self.indent + program + ';\n'

    def visit_number(self, node):
        return self.wrap(str(node.value))

    def visit_function_definition(self, node):
        return self.wrap('def ' + node.name + '(' +
                         ', '.join(node.function.args) + ') ' +
                         self.visit_block(node.function.body))

    def visit_conditional(self, node):
        program = ('if (' + print_expr(node.condition) + ') ' +
                   self.visit_block(node.if_true))
        if node.if_false:
            program = program[:-1] + ' else ' + self.visit_block(node.if_false)
        return self.wrap(program)

    def visit_print(self, node):
        return self.wrap('print ' + print_expr(node.expr))

    def visit_read(self, node):
        return self.wrap('read ' + node.name)

    def visit_function_call(self, node):
        return self.wrap(print_expr(node.fun_expr) + '(' +
                         ', '.join(map(print_expr, node.args or [])) +
                         ')')

    def visit_reference(self, node):
        return self.wrap(node.name)

    def visit_binary_operation(self, node):
        return self.wrap('(' +
                         print_expr(node.lhs) +
                         ') ' + node.op + ' (' +
                         print_expr(node.rhs) + ')')

    def visit_unary_operation(self, node):
        return self.wrap(node.op + '(' + print_expr(node.expr) + ')')


def pretty_print(node):
    print(node.accept(PrettyPrinter(depth=0, is_statement=True)), end='')
