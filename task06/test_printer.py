#!/usr/bin/env python3
import pytest
from model import *
from printer import *
from textwrap import dedent


def test_conditional():
    program = Conditional(Number(42), [], [])
    pretty_print_program = program.accept(PrettyPrinter(0, True))
    answer = '''\
        if (42) {
        }
    '''
    assert pretty_print_program == dedent(answer)


def test_conditional_empty():
    program = Conditional(Number(0), None, None)
    prettyPrinter = PrettyPrinter(0, True)
    pretty_print_program = program.accept(prettyPrinter)
    answer = '''\
        if (0) {
        }
    '''
    assert pretty_print_program == dedent(answer)


def test_function_definition():
    program = FunctionDefinition("foo", Function([], []))
    prettyPrinter = PrettyPrinter(0, True)
    pretty_print_program = program.accept(prettyPrinter)
    answer = '''\
        def foo() {
        }
    '''
    assert pretty_print_program == dedent(answer)


def test_function_call():
    program = FunctionCall(Reference('empty_func'), None)
    prettyPrinter = PrettyPrinter(0, True)
    pretty_print_program = program.accept(prettyPrinter)
    answer = '''\
        empty_func();
    '''
    assert pretty_print_program == dedent(answer)


def test_print():
    program = Print(Number(42))
    prettyPrinter = PrettyPrinter(0, True)
    pretty_print_program = program.accept(prettyPrinter)
    answer = '''\
        print 42;
    '''
    assert pretty_print_program == dedent(answer)


def test_read():
    program = Read('x')
    prettyPrinter = PrettyPrinter(0, True)
    pretty_print_program = program.accept(prettyPrinter)
    answer = '''\
        read x;
    '''
    assert pretty_print_program == dedent(answer)


def test_number():
    program = Number(10)
    prettyPrinter = PrettyPrinter(0, True)
    pretty_print_program = program.accept(prettyPrinter)
    answer = '''\
        10;
    '''
    assert pretty_print_program == dedent(answer)


def test_reference():
    program = Reference('x')
    prettyPrinter = PrettyPrinter(0, True)
    pretty_print_program = program.accept(prettyPrinter)
    answer = '''\
        x;
    '''
    assert pretty_print_program == dedent(answer)


def test_binary_operation():
    add = BinaryOperation(Number(2), '+', Number(3))
    mul = BinaryOperation(Number(1), '*', add)
    prettyPrinter = PrettyPrinter(0, True)
    pretty_print_program = mul.accept(prettyPrinter)
    answer = '''\
        (1) * ((2) + (3));
    '''
    assert pretty_print_program == dedent(answer)


def test_unary_operation():
    program = UnaryOperation('-', Number(42))
    prettyPrinter = PrettyPrinter(0, True)
    pretty_print_program = program.accept(prettyPrinter)
    answer = '''\
        -(42);
    '''
    assert pretty_print_program == dedent(answer)


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
    answer = '''\
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
    '''
    assert out == dedent(answer)


if __name__ == '__main__':
    pytest.main()
