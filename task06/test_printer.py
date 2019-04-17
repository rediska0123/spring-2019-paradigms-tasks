#!/usr/bin/env python3
import pytest
from model import *
from printer import *


def test_conditional():
    program = Conditional(Number(42), [], [])
    prettyPrinter = PrettyPrinter()
    pretty_print_program = program.accept(prettyPrinter)
    answer = '''if (42) {
}
'''
    assert pretty_print_program == answer


def test_conditional_empty():
    program = Conditional(Number(0), None, None)
    prettyPrinter = PrettyPrinter()
    pretty_print_program = program.accept(prettyPrinter)
    answer = '''if (0) {
}
'''
    assert pretty_print_program == answer


def test_functionDefinition():
    program = FunctionDefinition("foo", Function(None, None))
    prettyPrinter = PrettyPrinter()
    pretty_print_program = program.accept(prettyPrinter)
    answer = '''def foo() {
}
'''
    assert pretty_print_program == answer


def test_functionCall():
    program = FunctionCall(Reference('empty_func'), None)
    prettyPrinter = PrettyPrinter()
    pretty_print_program = program.accept(prettyPrinter)
    answer = '''empty_func();
'''
    assert pretty_print_program == answer


def test_print():
    program = Print(Number(42))
    prettyPrinter = PrettyPrinter()
    pretty_print_program = program.accept(prettyPrinter)
    answer = 'print 42;\n'
    assert pretty_print_program == answer


def test_read():
    program = Read('x')
    prettyPrinter = PrettyPrinter()
    pretty_print_program = program.accept(prettyPrinter)
    answer = 'read x;\n'
    assert pretty_print_program == answer


def test_number():
    program = Number(10)
    prettyPrinter = PrettyPrinter()
    pretty_print_program = program.accept(prettyPrinter)
    answer = '10;\n'
    assert pretty_print_program == answer


def test_reference():
    program = Reference('x')
    prettyPrinter = PrettyPrinter()
    pretty_print_program = program.accept(prettyPrinter)
    answer = 'x;\n'
    assert pretty_print_program == answer


def test_binaryOperation():
    add = BinaryOperation(Number(2), '+', Number(3))
    mul = BinaryOperation(Number(1), '*', add)
    prettyPrinter = PrettyPrinter()
    pretty_print_program = mul.accept(prettyPrinter)
    answer = '(1) * ((2) + (3));\n'
    assert pretty_print_program == answer


def test_unaryOperation():
    program = UnaryOperation('-', Number(42))
    prettyPrinter = PrettyPrinter()
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
