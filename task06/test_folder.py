#!/usr/bin/env python3
import pytest
from model import *
from folder import *


def test_BinaryOperation_Number_Number():
    program = BinaryOperation(Number(2), '*', Number(4))
    result = program.accept(ConstantFolder())
    assert result == Number(8)


def test_BinaryOperation_Number_Reference():
    program = BinaryOperation(Number(0), '*', Reference('a'))
    result = program.accept(ConstantFolder())
    assert result == Number(0)


def test_BinaryOperation_Reference_Number():
    program = BinaryOperation(Reference('a'), '*', Number(0))
    result = program.accept(ConstantFolder())
    assert result == Number(0)


def test_BinaryOperation_Reference_Reference():
    program = BinaryOperation(Reference('a'), '-', Reference('a'))
    result = program.accept(ConstantFolder())
    assert result == Number(0)


def test_UnaryOperation_Number():
    program = UnaryOperation('-', Number(-5))
    result = program.accept(ConstantFolder())
    assert result == Number(5)


def test_end_to_end():
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


if __name__ == "__main__":
    pytest.main()
