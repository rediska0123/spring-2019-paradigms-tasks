from model import *


class ConstantFolder(ASTNodeVisitor):
    def visit_list(self, statements):
        return [statement.accept(self) for statement in statements or []]

    def visit_number(self, node):
        return Number(node.value)

    def visit_function(self, node):
        return Function(node.args, self.visit_list(node.body))

    def visit_function_definition(self, node):
        return FunctionDefinition(node.name, node.function.accept(self))

    def visit_conditional(self, node):
        return Conditional(
            node.condition.accept(self),
            self.visit_list(node.if_true),
            self.visit_list(node.if_false)
        )

    def visit_print(self, node):
        return Print(node.expr.accept(self))

    def visit_read(self, node):
        return Read(node.name)

    def visit_function_call(self, node):
        return FunctionCall(node.fun_expr.accept(self),
                            self.visit_list(node.args))

    def visit_reference(self, node):
        return Reference(node.name)

    def visit_binary_operation(self, node):
        lhs = node.lhs.accept(self)
        rhs = node.rhs.accept(self)
        folded_node = BinaryOperation(
            lhs,
            node.op,
            rhs
        )
        if (isinstance(lhs, Number) and isinstance(rhs, Number)):
            return folded_node.evaluate(Scope())
        elif (node.op == '*' and
                isinstance(lhs, Number) and lhs == Number(0) and
                isinstance(rhs, Reference)):
            return Number(0)
        elif (node.op == '*' and
                isinstance(rhs, Number) and rhs == Number(0) and
                isinstance(lhs, Reference)):
            return Number(0)
        elif (node.op == '-' and
                isinstance(lhs, Reference) and
                isinstance(rhs, Reference) and
                lhs.name == rhs.name):
            return Number(0)
        return folded_node

    def visit_unary_operation(self, node):
        expr = node.expr.accept(self)
        folded_node = UnaryOperation(
            node.op,
            expr
        )
        if isinstance(expr, Number):
            return folded_node.evaluate(Scope())
        return folded_node


def fold_constants(node):
    return node.accept(ConstantFolder())
