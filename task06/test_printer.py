#!/usr/bin/env python3
import pytest
from model import *
from printer import *
from textwrap import dedent


def test_conditional():
    program = Conditional(Number(42), [], [])
    pretty_print_program = program.accept(PrettyPrinter(0, True))
    assert pretty_print_program == dedent('''\
        if (42) {
        }
    ''')


def test_conditional_empty():
    program = Conditional(Number(0), None, None)
    pretty_print_program = program.accept(PrettyPrinter(0, True))
    assert pretty_print_program == dedent('''\
        if (0) {
        }
    ''')


def test_function_definition():
    program = FunctionDefinition("foo", Function([], []))
    pretty_print_program = program.accept(PrettyPrinter(0, True))
    assert pretty_print_program == dedent('''\
        def foo() {
        }
    ''')


def test_function_call():
    program = FunctionCall(Reference('empty_func'), None)
    pretty_print_program = program.accept(PrettyPrinter(0, True))
    assert pretty_print_program == dedent('''\
        empty_func();
    ''')


def test_print():
    program = Print(Number(42))
    pretty_print_program = program.accept(PrettyPrinter(0, True))
    assert pretty_print_program == dedent('''\
        print 42;
    ''')


def test_read():
    program = Read('x')
    pretty_print_program = program.accept(PrettyPrinter(0, True))
    assert pretty_print_program == dedent('''\
        read x;
    ''')


def test_number():
    program = Number(10)
    pretty_print_program = program.accept(PrettyPrinter(0, True))
    assert pretty_print_program == dedent('''\
        10;
    ''')


def test_reference():
    program = Reference('x')
    pretty_print_program = program.accept(PrettyPrinter(0, True))
    assert pretty_print_program == dedent('''\
        x;
    ''')


def test_binary_operation():
    add = BinaryOperation(Number(2), '+', Number(3))
    mul = BinaryOperation(Number(1), '*', add)
    pretty_print_program = mul.accept(PrettyPrinter(0, True))
    assert pretty_print_program == dedent('''\
        (1) * ((2) + (3));
    ''')


def test_unary_operation():
    program = UnaryOperation('-', Number(42))
    pretty_print_program = program.accept(PrettyPrinter(0, True))
    assert pretty_print_program == dedent('''\
        -(42);
    ''')


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
    assert out == dedent('''\
        def main(arg1) {
            read x;
            print x;
            if ((2) == (3)) {
                if (1) {
                }
            } else {
                exit(-(arg1));
            }
        }
    ''')


if __name__ == '__main__':
    pytest.main()
