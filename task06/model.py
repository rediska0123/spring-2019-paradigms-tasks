#!/usr/bin/env python3
import abc


class Scope:
    def __init__(self, parent=None):
        self.parent = parent
        self.dictionary = {}

    def __getitem__(self, name):
        if name in self.dictionary:
            return self.dictionary[name]
        if self.parent:
            return self.parent[name]
        raise KeyError(name)

    def __setitem__(self, name, value):
        self.dictionary[name] = value


class ASTNode(metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def evaluate(self, scope):
        pass

    def accept(self, visitor):
        pass


class ASTNodeVisitor(metaclass=abc.ABCMeta):
    @abc.abstractmethod
    def visit_number(self, node):
        pass

    def visit_function(self, node):
        pass

    def visit_functionDefinition(self, node):
        pass

    def visit_conditional(self, node):
        pass

    def visit_print(self, node):
        pass

    def visit_read(self, node):
        pass

    def visit_functionCall(self, node):
        pass

    def visit_reference(self, node):
        pass

    def visit_binaryOperation(delf, node):
        pass

    def visit_unaryOperation(delf, node):
        pass


class Number(ASTNode):

    def __init__(self, value):
        if isinstance(value, int):
            self.value = value
        if isinstance(value, bool):
            self.value = 1 if value else 0

    def accept(self, visitor):
        return visitor.visit_number(self)

    def evaluate(self, scope):
        return self

    def __eq__(self, other):
        if isinstance(other, Number):
            return Number(self.value == other.value)

    def __bool__(self):
        return bool(self.value)

    def __hash__(self):
        return hash(self.value)


class Function(ASTNode):

    def __init__(self, args, body):
        self.args = args
        self.body = body

    def accept(self, visitor):
        return visitor.visit_function(self)

    def evaluate(self, scope):
        return self


class FunctionDefinition(ASTNode):

    def __init__(self, name, function):
        self.name = name
        self.function = function

    def accept(self, visitor):
        return visitor.visit_functionDefinition(self)

    def evaluate(self, scope):
        scope[self.name] = self.function
        return self.function


class Conditional(ASTNode):

    def __init__(self, condition, if_true, if_false=None):
        self.condition = condition
        self.if_true = if_true
        self.if_false = if_false

    def accept(self, visitor):
        return visitor.visit_conditional(self)

    def evaluate(self, scope):
        condition_result = self.condition.evaluate(scope)
        commands = self.if_true if bool(condition_result) else self.if_false
        res = Number(0)
        for command in commands or []:
            res = command.evaluate(scope)
        return res


class Print(ASTNode):

    def __init__(self, expr):
        self.expr = expr

    def accept(self, visitor):
        return visitor.visit_print(self)

    def evaluate(self, scope):
        result = self.expr.evaluate(scope)
        print(result.value)
        return result


class Read(ASTNode):

    def __init__(self, name):
        self.name = name

    def accept(self, visitor):
        return visitor.visit_read(self)

    def evaluate(self, scope):
        val = int(input())
        scope[self.name] = Number(val)
        return scope[self.name]


class FunctionCall(ASTNode):

    def __init__(self, fun_expr, args):
        self.fun_expr = fun_expr
        self.args = args

    def accept(self, visitor):
        return visitor.visit_functionCall(self)

    def evaluate(self, scope):
        function = self.fun_expr.evaluate(scope)
        evaluated_args = []
        for arg in self.args:
            evaluated_args.append(arg.evaluate(scope))
        call_scope = Scope(scope)
        for (i, arg) in enumerate(function.args):
            call_scope[arg] = evaluated_args[i]

        res = Number(0)
        for command in function.body or []:
            res = command.evaluate(call_scope)
        return res


class Reference(ASTNode):

    def __init__(self, name):
        self.name = name

    def accept(self, visitor):
        return visitor.visit_reference(self)

    def evaluate(self, scope):
        return scope[self.name]


class BinaryOperation(ASTNode):
    # '+', '-', '*', '/', '%', '==', '!=', '<', '>', '<=', '>=', '&&', '||'

    def __init__(self, lhs, op, rhs):
        self.lhs = lhs
        self.op = op
        self.rhs = rhs
        self.operations = {
            '+': lambda x, y: x + y,
            '-': lambda x, y: x - y,
            '*': lambda x, y: x * y,
            '/': lambda x, y: x // y,
            '%': lambda x, y: x % y,
            '==': lambda x, y: x == y,
            '!=': lambda x, y: x != y,
            '<': lambda x, y: x < y,
            '>': lambda x, y: x > y,
            '<=': lambda x, y: x <= y,
            '>=': lambda x, y: x >= y,
            '&&': lambda x, y: x and y,
            '||': lambda x, y: x or y
        }

    def accept(self, visitor):
        return visitor.visit_binaryOperation(self)

    def evaluate(self, scope):
        lres = self.lhs.evaluate(scope).value
        rres = self.rhs.evaluate(scope).value
        return Number(self.operations[self.op](lres, rres))


class UnaryOperation(ASTNode):

    def __init__(self, op, expr):
        self.op = op
        self.expr = expr
        self.operations = {
            '-': lambda x: -x,
            '!': lambda x: not x
        }

    def accept(self, visitor):
        return visitor.visit_unaryOperation(self)

    def evaluate(self, scope):
        expr_res = self.expr.evaluate(scope).value
        return Number(self.operations[self.op](expr_res))
