from model import *


class PrettyPrinter(ASTNodeVisitor):
    def __init__(self, depth, is_command):
        self.tabs = ' ' * 4 * depth if is_command else ''
        self.end_with_semicolon = ';\n' if is_command else ''
        self.end = '\n' if is_command else ''
        self.depth = depth

    def visit_Number(self, number):
        return self.tabs + str(number.value) + self.end_with_semicolon

    def visit_FunctionDefinition(self, function_definition):
        program = self.tabs + 'def ' + function_definition.name + '(' + \
                  ', '.join(function_definition.function.args) + ') {\n'
        for command in function_definition.function.body:
            program += command.accept(PrettyPrinter(self.depth + 1, True))
        program += self.tabs + '}' + self.end
        return program

    def visit_Conditional(self, conditional):
        program = self.tabs + 'if (' + \
                  conditional.condition.accept(
                      PrettyPrinter(
                          self.depth,
                          False
                      )
                  ) + ') {\n'
        for if_true_command in conditional.if_true:
            program += if_true_command.accept(
                           PrettyPrinter(
                               self.depth + 1,
                               True
                           )
                       )
        if (conditional.if_false):
            program += self.tabs + '} else {\n'
            for if_false_command in conditional.if_false:
                program += if_false_command.accept(
                               PrettyPrinter(
                                   self.depth + 1,
                                   True
                               )
                           )
        program += self.tabs + '}' + self.end
        return program

    def visit_Print(self, print_):
        return self.tabs + 'print ' + \
               print_.expr.accept(PrettyPrinter(self.depth, False)) + \
               self.end_with_semicolon

    def visit_Read(self, read):
        return self.tabs + 'read ' + read.name + self.end_with_semicolon

    def visit_FunctionCall(self, function_call):
        return self.tabs + \
                function_call.fun_expr.accept(
                    PrettyPrinter(
                        self.depth,
                        False
                    )
                ) + '(' + \
                ', '.join([arg.accept(
                               PrettyPrinter(
                                   self.depth,
                                   False
                               )
                           ) for arg in function_call.args]) + \
                ')' + self.end_with_semicolon

    def visit_Reference(self, reference):
        return self.tabs + reference.name + self.end_with_semicolon

    def visit_BinaryOperation(self, binary_operation):
        return self.tabs + '(' + \
               binary_operation.lhs.accept(
                   PrettyPrinter(
                       self.depth,
                       False
                   )
               ) + ') ' + binary_operation.op + ' (' + \
               binary_operation.rhs.accept(
                   PrettyPrinter(
                       self.depth,
                       False
                   )
               ) + ')' + self.end_with_semicolon

    def visit_UnaryOperation(self, unary_operation):
        return self.tabs + unary_operation.op + '(' + \
               unary_operation.expr.accept(
                   PrettyPrinter(
                       self.depth,
                       False
                   )
               ) + \
               ')' + self.end_with_semicolon


def pretty_print(node):
    print(node.accept(PrettyPrinter(0, True)), end='')
