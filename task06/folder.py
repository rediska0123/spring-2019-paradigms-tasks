from model import *


def simplify(args):
    return [arg.accept(ConstantFolder()) for arg in args]


class ConstantFolder(ASTNodeVisitor):

    def visit_Number(self, number):
        return number

    def visit_Function(self, function):
        args = function.args
        body = function.body
        return Function(args, simplify(body))

    def visit_FunctionDefinition(self, function_definition):
        name = function_definition.name
        function = function_definition.function
        return FunctionDefinition(name, function.accept(ConstantFolder()))

    def visit_Conditional(self, conditional):
        condition = conditional.condition
        if_true, if_false = conditional.if_true, conditional.if_false
        return Conditional(
            condition.accept(ConstantFolder()),
            simplify(if_true),
            simplify(if_false)
        )

    def visit_Print(self, print_):
        return Print(print_.expr.accept(ConstantFolder()))

    def visit_Read(self, read):
        return read

    def visit_FunctionCall(self, function_call):
        return FunctionCall(function_call.func_expr, simplify(args))

    def visit_Reference(self, reference):
        return reference

    def visit_BinaryOperation(self, binary_operation):
        lhs, rhs = binary_operation.lhs, binary_operation.rhs
        simplified_lhs = lhs.accept(ConstantFolder())
        simplified_rhs = rhs.accept(ConstantFolder())
        simplified_binary_operation = BinaryOperation(
            simplified_lhs,
            binary_operation.op,
            simplified_rhs
        )
        if (isinstance(simplified_lhs, Number) and
                isinstance(simplified_rhs, Number)):
            return simplified_binary_operation.evaluate(Scope())
        if (isinstance(simplified_lhs, Number) and
                not bool(simplified_lhs) and
                isinstance(simplified_rhs, Reference)):
            return Number(0)
        if (isinstance(simplified_rhs, Number) and
                not bool(simplified_rhs) and
                isinstance(simplified_lhs, Reference)):
            return Number(0)
        if (isinstance(simplified_lhs, Reference) and
                isinstance(rhs, Reference) and
                simplified_lhs.name == simplified_rhs.name and
                binary_operation.op == '-'):
            return Number(0)
        return simplified_binary_operation

    def visit_UnaryOperation(self, unary_operation):
        expr = unary_operation.expr
        simplified_expr = expr.accept(ConstantFolder())
        simplified_unary_operation = UnaryOperation(
            unary_operation.op,
            simplified_expr
        )
        if isinstance(simplified_expr, Number):
            return simplified_unary_operation.evaluate(Scope())
        return simplified_unary_operation


def fold_constants(node):
    return node.accept(ConstantFolder())
