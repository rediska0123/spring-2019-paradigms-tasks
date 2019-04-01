#!/usr/bin/env python3
import pytest
from model import *
from printer import *


def test_Conditional():
    program = Conditional(Number(42), [], [])
    pretty_print_program = program.accept(PrettyPrinter(0, True))
    answer = 'if (42) {\n};\n'
    assert pretty_print_program == answer


def test_FunctionDefinition():
    program = FunctionDefinition("foo", Function([], []))
    pretty_print_program = program.accept(PrettyPrinter(0, True))
    answer = 'def foo() {\n};\n'
    assert pretty_print_program == answer


def test_Print():
    program = Print(Number(42))
    pretty_print_program = program.accept(PrettyPrinter(0, True))
    answer = 'print 42;\n'
    assert pretty_print_program == answer


def test_Read():
    program = Read('x')
    pretty_print_program = program.accept(PrettyPrinter(0, True))
    answer = 'read x;\n'
    assert pretty_print_program == answer


def test_Number():
    program = Number(10)
    pretty_print_program = program.accept(PrettyPrinter(0, True))
    answer = '10;\n'
    assert pretty_print_program == answer


def test_Reference():
    program = Reference('x')
    pretty_print_program = program.accept(PrettyPrinter(0, True))
    answer = 'x;\n'
    assert pretty_print_program == answer


def test_BinaryOperation():
    add = BinaryOperation(Number(2), '+', Number(3))
    mul = BinaryOperation(Number(1), '*', add)
    pretty_print_program = mul.accept(PrettyPrinter(0, True))
    answer = '(1) * ((2) + (3));\n'
    assert pretty_print_program == answer


def test_UnaryOperation():
    program = UnaryOperation('-', Number(42))
    pretty_print_program = program.accept(PrettyPrinter(0, True))
    answer = '-(42);\n'
    assert pretty_print_program == answer


def test_end_to_end(capsys):
    pretty_print(FunctionDefinition('main', Function(['arg1'], [
        Read('x'),
        Print(Reference('x')),
        Conditional(
            BinaryOperation(Number(2), '==', Number(3)),
            [
                Conditional(Number(1), [], [])
            ],
            [
                FunctionCall(Reference('exit'), [
                    UnaryOperation('-', Reference('arg1'))
                ])
            ],
        ),
    ])))
    out, err = capsys.readouterr()
    answer = '''def main(arg1) {
    read x;
    print x;
    if ((2) == (3)) {
        if (1) {
        };
    } else {
        exit(-(arg1));
    };
};
'''
    assert out == answer


if __name__ == "__main__":
    pytest.main()
