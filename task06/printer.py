from model import *


def print_expr(expr):
    return expr.accept(PrettyPrinter(depth=0, is_statement=False))


class PrettyPrinter(ASTNodeVisitor):
    def visit_block(self, block):
        program = '{\n'
        for statement in block:
            program += statement.accept(
                           PrettyPrinter(self.depth + 1, is_statement=True))
        program += self.indent + '}'
        return program

    def __init__(self, depth, is_statement):
        assert is_statement or depth == 0
        self.indent = '    ' * depth
        self.depth = depth
        self.is_statement = is_statement

    def terminate(self, program):
        if not self.is_statement:
            return program
        if program.endswith('}'):
            return program + '\n'
        return program + ';\n'

    def visit_number(self, node):
        return self.terminate(self.indent + str(node.value))

    def visit_function_definition(self, node):
        args = ', '.join(node.function.args)
        program = self.indent + 'def ' + node.name + '(' + args + ') '
        program += self.visit_block(node.function.body)
        return self.terminate(program)

    def visit_conditional(self, node):
        program = self.indent + 'if (' + print_expr(node.condition) + ') '
        program += self.visit_block(node.if_true or [])
        if node.if_false:
            program += ' else ' + self.visit_block(node.if_false)
        return self.terminate(program)

    def visit_print(self, node):
        return self.terminate(self.indent + 'print ' + print_expr(node.expr))

    def visit_read(self, node):
        return self.terminate(self.indent + 'read ' + node.name)

    def visit_function_call(self, node):
        args = ', '.join(map(print_expr, node.args or []))
        return self.terminate(self.indent + print_expr(node.fun_expr) +
                              '(' + args + ')')

    def visit_reference(self, node):
        return self.terminate(self.indent + node.name)

    def visit_binary_operation(self, node):
        return (self.terminate(self.indent + '(' +
                print_expr(node.lhs) +
                ') ' + node.op + ' (' +
                print_expr(node.rhs) + ')'))

    def visit_unary_operation(self, node):
        return (self.terminate(self.indent + node.op + '(' +
                print_expr(node.expr) + ')'))


def pretty_print(node):
    print(node.accept(PrettyPrinter(depth=0, is_statement=True)), end='')
