from model import *


def print_statements(statements, depth, is_statement):
    program = ''
    for statement in statements:
        program += statement.accept(PrettyPrinter(depth, is_statement))
    return program


def print_expr(expr):
    return expr.accept(PrettyPrinter(depth=0, is_statement=False))


class PrettyPrinter(ASTNodeVisitor):
    def __init__(self, depth, is_statement):
        self.indent = '    ' * depth if is_statement else ''
        self.end_with_semicolon = ';\n' if is_statement else ''
        self.end = '\n' if is_statement else ''
        self.depth = depth

    def visit_number(self, node):
        return self.indent + str(node.value) + self.end_with_semicolon

    def visit_function_definition(self, node):
        args = ', '.join(node.function.args)
        program = self.indent + 'def ' + node.name + '(' + args + ') {\n'
        program += print_statements(node.function.body,
                                    self.depth + 1,
                                    is_statement=True)
        program += self.indent + '}' + self.end
        return program

    def visit_conditional(self, node):
        program = (self.indent + 'if (' +
                   print_expr(node.condition) + ') {\n')
        program += print_statements(node.if_true or [],
                                    self.depth + 1,
                                    is_statement=True)
        if node.if_false:
            program += (self.indent + '} else {\n' +
                        print_statements(node.if_false,
                                         self.depth + 1,
                                         is_statement=True))
        program += self.indent + '}' + self.end
        return program

    def visit_print(self, node):
        return (self.indent + 'print ' + print_expr(node.expr) +
                self.end_with_semicolon)

    def visit_read(self, node):
        return self.indent + 'read ' + node.name + self.end_with_semicolon

    def visit_function_call(self, node):
        args = ', '.join([
            print_expr(arg)
            for arg in node.args]) if node.args else ''
        return (self.indent +
                node.fun_expr.accept(PrettyPrinter(depth=self.depth,
                                                   is_statement=False)) +
                '(' + args + ')' + self.end_with_semicolon)

    def visit_reference(self, node):
        return self.indent + node.name + self.end_with_semicolon

    def visit_binary_operation(self, node):
        return (self.indent + '(' +
                print_expr(node.lhs) +
                ') ' + node.op + ' (' +
                print_expr(node.rhs) +
                ')' + self.end_with_semicolon)

    def visit_unary_operation(self, node):
        return (self.indent + node.op + '(' + print_expr(node.expr) +
                ')' + self.end_with_semicolon)


def pretty_print(node):
    print(node.accept(PrettyPrinter(depth=0, is_statement=True)), end='')
