#!/usr/bin/env python3
import pytest
from model import *
from folder import *


def test_number():
    program = Number(2)
    result = program.accept(ConstantFolder())
    assert result == Number(2)


def test_binaryOperation_Number_Number():
    program = BinaryOperation(Number(2), '*', Number(4))
    result = program.accept(ConstantFolder())
    assert result == Number(8)


def test_binaryOperation_Number_Reference():
    program = BinaryOperation(
        BinaryOperation(Number(2), '-', Number(2)),
        '*',
        Reference('a')
    )
    result = program.accept(ConstantFolder())
    assert result == Number(0)


def test_binaryOperation_Reference_Number():
    program = BinaryOperation(
        Reference('a'),
        '*',
        BinaryOperation(Number(2), '-', Number(2))
    )
    result = program.accept(ConstantFolder())
    assert result == Number(0)


def test_binaryOperation_Reference_Reference():
    program = BinaryOperation(Reference('a'), '-', Reference('a'))
    result = program.accept(ConstantFolder())
    assert result == Number(0)


def test_unaryOperation_Number():
    program = UnaryOperation('-', Number(-5))
    result = program.accept(ConstantFolder())
    assert result == Number(5)


def test_end_to_end1():
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
    result = program.accept(ConstantFolder())
    assert result == Number(13)


def test_end_to_end2():
    program = fold_constants(
        Print(
            BinaryOperation(
                UnaryOperation('-', Number(4)),
                '+',
                BinaryOperation(Reference('x'), '-', Reference('x'))
            )
        )
    )
    result = program.accept(ConstantFolder())
    assert isinstance(result, Print) and result.expr == Number(-4)


def test_end_to_end3():
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

    def_result = definition.accept(ConstantFolder())

    assert isinstance(def_result, FunctionDefinition)


def test_end_to_end4():
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


if __name__ == "__main__":
    pytest.main()
