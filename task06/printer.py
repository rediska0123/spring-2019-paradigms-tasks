from model import *


class PrettyPrinter(ASTNodeVisitor):
    def __init__(self, depth=0, is_command=True):
        self.tabs = '\t' * depth if is_command else ''
        self.end_with_semicolon = ';\n' if is_command else ''
        self.end = '\n' if is_command else ''
        self.depth = depth

    def visit_number(self, node):
        return self.tabs + str(node.value) + self.end_with_semicolon

    def visit_functionDefinition(self, node):
        args = node.function.args
        args = ', '.join(args) if args else ''
        program = self.tabs + 'def ' + node.name + '(' + args + ') {\n'
        for command in node.function.body or []:
            program += command.accept(PrettyPrinter(self.depth + 1, True))
        program += self.tabs + '}' + self.end
        return program

    def visit_conditional(self, node):
        program = self.tabs + 'if (' + \
                  node.condition.accept(
                      PrettyPrinter(
                          self.depth,
                          False
                      )
                  ) + ') {\n'
        for if_true_command in node.if_true or []:
            program += if_true_command.accept(
                           PrettyPrinter(
                               self.depth + 1,
                               True
                           )
                       )
        if node.if_false:
            program += self.tabs + '} else {\n'
            for if_false_command in node.if_false:
                program += if_false_command.accept(
                               PrettyPrinter(
                                   self.depth + 1,
                                   True
                               )
                           )
        program += self.tabs + '}' + self.end
        return program

    def visit_print(self, node):
        return self.tabs + 'print ' + \
               node.expr.accept(PrettyPrinter(self.depth, False)) + \
               self.end_with_semicolon

    def visit_read(self, node):
        return self.tabs + 'read ' + node.name + self.end_with_semicolon

    def visit_functionCall(self, node):
        args = ', '.join([arg.accept(
            PrettyPrinter(
                self.depth,
                False
            )
        ) for arg in node.args]) if node.args else ''
        return self.tabs + node.fun_expr.accept(
                               PrettyPrinter(
                                   self.depth,
                                   False
                               )
                           ) + '(' + args + ')' + self.end_with_semicolon

    def visit_reference(self, node):
        return self.tabs + node.name + self.end_with_semicolon

    def visit_binaryOperation(self, node):
        return self.tabs + '(' + \
               node.lhs.accept(
                   PrettyPrinter(
                       self.depth,
                       False
                   )
               ) + ') ' + node.op + ' (' + \
               node.rhs.accept(
                   PrettyPrinter(
                       self.depth,
                       False
                   )
               ) + ')' + self.end_with_semicolon

    def visit_unaryOperation(self, node):
        return self.tabs + node.op + '(' + \
               node.expr.accept(
                   PrettyPrinter(
                       self.depth,
                       False
                   )
               ) + \
               ')' + self.end_with_semicolon


def pretty_print(node):
    print(node.accept(PrettyPrinter(0, True)), end='')
