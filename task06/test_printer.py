#!/usr/bin/env python3
import pytest
from model import *
from printer import *


def test_conditional(depth=0, is_command=True):
    program = Conditional(Number(42), [], [])
    prettyPrinter = PrettyPrinter(depth, is_command)
    pretty_print_program = program.accept(prettyPrinter)
    answer = 'if (42) {\n}\n'
    assert pretty_print_program == answer


def test_functionDefinition(depth=0, is_command=True):
    program = FunctionDefinition("foo", Function([], []))
    prettyPrinter = PrettyPrinter(depth, is_command)
    pretty_print_program = program.accept(prettyPrinter)
    answer = 'def foo() {\n}\n'
    assert pretty_print_program == answer


def test_print(depth=0, is_command=True):
    program = Print(Number(42))
    prettyPrinter = PrettyPrinter(depth, is_command)
    pretty_print_program = program.accept(prettyPrinter)
    answer = 'print 42;\n'
    assert pretty_print_program == answer


def test_read(depth=0, is_command=True):
    program = Read('x')
    prettyPrinter = PrettyPrinter(depth, is_command)
    pretty_print_program = program.accept(prettyPrinter)
    answer = 'read x;\n'
    assert pretty_print_program == answer


def test_number(depth=0, is_command=True):
    program = Number(10)
    prettyPrinter = PrettyPrinter(depth, is_command)
    pretty_print_program = program.accept(prettyPrinter)
    answer = '10;\n'
    assert pretty_print_program == answer


def test_reference(depth=0, is_command=True):
    program = Reference('x')
    prettyPrinter = PrettyPrinter(depth, is_command)
    pretty_print_program = program.accept(prettyPrinter)
    answer = 'x;\n'
    assert pretty_print_program == answer


def test_binaryOperation(depth=0, is_command=True):
    add = BinaryOperation(Number(2), '+', Number(3))
    mul = BinaryOperation(Number(1), '*', add)
    prettyPrinter = PrettyPrinter(depth, is_command)
    pretty_print_program = mul.accept(prettyPrinter)
    answer = '(1) * ((2) + (3));\n'
    assert pretty_print_program == answer


def test_unaryOperation(depth=0, is_command=True):
    program = UnaryOperation('-', Number(42))
    prettyPrinter = PrettyPrinter(depth, is_command)
    pretty_print_program = program.accept(prettyPrinter)
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
\tread x;
\tprint x;
\tif ((2) == (3)) {
\t\tif (1) {
\t\t}
\t} else {
\t\texit(-(arg1));
\t}
}
'''
    assert out == answer


if __name__ == "__main__":
    pytest.main()
