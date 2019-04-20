from model import *


def accept_statement(statement, depth, is_command):
    return statement.accept(PrettyPrinter(depth, is_command))


A_COMMAND = True
NOT_A_COMMAND = False


class PrettyPrinter(ASTNodeVisitor):
    def __init__(self, depth, is_command):
        self.tabs = '    ' * depth if is_command else ''
        self.end_with_semicolon = ';\n' if is_command else ''
        self.end = '\n' if is_command else ''
        self.depth = depth

    def visit_number(self, node):
        return self.tabs + str(node.value) + self.end_with_semicolon

    def visit_function_definition(self, node):
        args = ', '.join(node.function.args)
        program = self.tabs + 'def ' + node.name + '(' + args + ') {\n'
        for statement in node.function.body or []:
            program += (statement.accept(
                           PrettyPrinter(self.depth + 1, A_COMMAND)))
        program += self.tabs + '}' + self.end
        return program

    def visit_conditional(self, node):
        program = self.tabs + 'if (' + accept_statement(
                                           node.condition,
                                           self.depth,
                                           NOT_A_COMMAND) + ') {\n'
        for if_true_command in node.if_true or []:
            program += accept_statement(
                           if_true_command,
                           self.depth + 1,
                           A_COMMAND)
        if node.if_false:
            program += self.tabs + '} else {\n'
            for if_false_command in node.if_false:
                program += accept_statement(
                               if_false_command,
                               self.depth + 1,
                               A_COMMAND)
        program += self.tabs + '}' + self.end
        return program

    def visit_print(self, node):
        return (self.tabs + 'print ' +
                accept_statement(node.expr, self.depth, NOT_A_COMMAND) +
                self.end_with_semicolon)

    def visit_read(self, node):
        return self.tabs + 'read ' + node.name + self.end_with_semicolon

    def visit_function_call(self, node):
        args = ', '.join([
            accept_statement(arg, self.depth, NOT_A_COMMAND)
            for arg in node.args]) if node.args else ''
        return (self.tabs +
                accept_statement(node.fun_expr, self.depth, NOT_A_COMMAND) +
                '(' + args + ')' + self.end_with_semicolon)

    def visit_reference(self, node):
        return self.tabs + node.name + self.end_with_semicolon

    def visit_binary_operation(self, node):
        return (self.tabs + '(' +
                accept_statement(node.lhs, self.depth, NOT_A_COMMAND) +
                ') ' + node.op + ' (' +
                accept_statement(node.rhs, self.depth, NOT_A_COMMAND) +
                ')' + self.end_with_semicolon)

    def visit_unary_operation(self, node):
        return (self.tabs + node.op + '(' +
                accept_statement(node.expr, self.depth, NOT_A_COMMAND) +
                ')' + self.end_with_semicolon)


def pretty_print(node):
    print(node.accept(PrettyPrinter(0, A_COMMAND)), end='')
