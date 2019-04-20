from model import *


class ConstantFolder(ASTNodeVisitor):
    def simplify(self, args):
        return [arg.accept(self) for arg in args] if args else []

    def visit_number(self, node):
        return node

    def visit_function(self, node):
        return Function(node.args, simplify(node.body))

    def visit_function_definition(self, node):
        return FunctionDefinition(node.name, node.function.accept(self))

    def visit_conditional(self, node):
        condition = node.condition
        if_true, if_false = node.if_true, node.if_false
        return Conditional(
            condition.accept(self),
            simplify(if_true),
            simplify(if_false)
        )

    def visit_print(self, node):
        return Print(node.expr.accept(self))

    def visit_read(self, node):
        return node

    def visit_function_call(self, node):
        return FunctionCall(node.fun_expr, simplify(node.args))

    def visit_reference(self, node):
        return node

    def visit_binary_operation(self, node):
        lhs = node.lhs.accept(self)
        rhs = node.rhs.accept(self)
        simplified_binary_operation = BinaryOperation(
            lhs,
            node.op,
            rhs
        )
        if (isinstance(lhs, Number) and
                isinstance(rhs, Number)):
            return simplified_binary_operation.evaluate(Scope())
        elif (isinstance(lhs, Number) and
                lhs == Number(0) and
                isinstance(rhs, Reference) and
                node.op == '*'):
            return Number(0)
        elif (isinstance(rhs, Number) and
                rhs == Number(0) and
                isinstance(lhs, Reference) and
                node.op == '*'):
            return Number(0)
        elif (isinstance(lhs, Reference) and
                isinstance(rhs, Reference) and
                lhs.name == rhs.name and
                node.op == '-'):
            return Number(0)
        return simplified_binary_operation

    def visit_unary_operation(self, node):
        expr = node.expr.accept(self)
        simplified_unary_operation = UnaryOperation(
            node.op,
            expr
        )
        if isinstance(expr, Number):
            return simplified_unary_operation.evaluate(Scope())
        return simplified_unary_operation


def fold_constants(node):
    return node.accept(ConstantFolder())
