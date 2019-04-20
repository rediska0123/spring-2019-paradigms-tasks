#!/usr/bin/env python3
import pytest
from model import *
from folder import *


def test_number():
    program = Number(2)
    assert program.accept(ConstantFolder()) == Number(2)


def test_binary_operation_number_number():
    program = BinaryOperation(Number(2), '*', Number(4))
    assert program.accept(ConstantFolder()) == Number(8)


def test_binary_operation_number_reference():
    program = BinaryOperation(
        Number(0),
        '*',
        Reference('a')
    )
    assert program.accept(ConstantFolder()) == Number(0)


def test_binary_operation_reference_number():
    program = BinaryOperation(
        Reference('a'),
        '*',
        Number(0)
    )
    assert program.accept(ConstantFolder()) == Number(0)


def test_binary_operation_reference_reference():
    program = BinaryOperation(Reference('a'), '-', Reference('a'))
    assert program.accept(ConstantFolder()) == Number(0)


def test_unary_operation_number():
    program = UnaryOperation('-', Number(-5))
    assert program.accept(ConstantFolder()) == Number(5)


def test_end_to_end_many_binary_operations():
    program = fold_constants(
        BinaryOperation(
            Number(10),
            '-',
            UnaryOperation(
                '-',
                BinaryOperation(
                    Number(3),
                    '+',
                    BinaryOperation(
                        Reference('x'),
                        '-',
                        Reference('x')
                    )
                )
            )
        )
    )
    assert program.accept(ConstantFolder()) == Number(13)


def test_end_to_end_gcd():
    definition = FunctionDefinition('gcd', Function(
        ['a', 'b'],
        [
            Conditional(
                BinaryOperation(Reference('b'), '==', Number(0)),
                [Reference('a')],
                [FunctionCall(Reference('gcd'), [
                    Reference('b'),
                    BinaryOperation(Reference('a'), '%', Reference('b'))
                ])]
            )
        ]
    ))

    assert isinstance(definition.accept(ConstantFolder()), FunctionDefinition)


def test_end_to_end_function_call():
    program = FunctionCall(Function(
        [],
        [BinaryOperation(
            Number(5),
            '-',
            Number(3)
        )]
    ), [])
    result = program.accept(ConstantFolder())
    assert (isinstance(result, FunctionCall) and
            result.fun_expr.body[0].value == 2)


if __name__ == '__main__':
    pytest.main()
