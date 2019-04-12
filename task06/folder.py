from model import *


def simplify(constantFolder, args):
    return [arg.accept(constantFolder) for arg in args] if args else []


class ConstantFolder(ASTNodeVisitor):

    def visit_number(self, node):
        return node

    def visit_function(self, node):
        args = node.args
        body = node.body
        return Function(args, simplify(self, body))

    def visit_functionDefinition(self, node):
        name = node.name
        function = node.function
        return FunctionDefinition(name, function.accept(self))

    def visit_conditional(self, node):
        condition = node.condition
        if_true, if_false = node.if_true, node.if_false
        return Conditional(
            condition.accept(self),
            simplify(self, if_true),
            simplify(self, if_false)
        )

    def visit_print(self, node):
        return Print(node.expr.accept(self))

    def visit_read(self, node):
        return node

    def visit_functionCall(self, node):
        return FunctionCall(node.func_expr, simplify(node.args))

    def visit_reference(self, node):
        return node

    def visit_binaryOperation(self, node):
        lhs, rhs = node.lhs, node.rhs
        simplified_lhs = lhs.accept(self)
        simplified_rhs = rhs.accept(self)
        simplified_binary_operation = BinaryOperation(
            simplified_lhs,
            node.op,
            simplified_rhs
        )
        if (isinstance(simplified_lhs, Number) and
                isinstance(simplified_rhs, Number)):
            return simplified_binary_operation.evaluate(Scope())
        elif (isinstance(simplified_lhs, Number) and
                not bool(simplified_lhs) and
                isinstance(simplified_rhs, Reference) and
                node.op == '*'):
            return Number(0)
        elif (isinstance(simplified_rhs, Number) and
                not bool(simplified_rhs) and
                isinstance(simplified_lhs, Reference) and
                node.op == '*'):
            return Number(0)
        elif (isinstance(simplified_lhs, Reference) and
                isinstance(simplified_rhs, Reference) and
                simplified_lhs.name == simplified_rhs.name and
                node.op == '-'):
            return Number(0)
        return simplified_binary_operation

    def visit_unaryOperation(self, node):
        expr = node.expr
        simplified_expr = expr.accept(self)
        simplified_unary_operation = UnaryOperation(
            node.op,
            simplified_expr
        )
        if isinstance(simplified_expr, Number):
            return simplified_unary_operation.evaluate(Scope())
        return simplified_unary_operation


def fold_constants(node):
    return node.accept(ConstantFolder())
