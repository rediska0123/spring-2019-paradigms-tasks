#!/usr/bin/env python3

from model import *
import math
import pytest


def test_scope_simple():
    scope = Scope()
    scope['a'] = 1
    scope['b'] = 2
    assert scope['a'] == 1
    assert scope['b'] == 2


def test_scope_grandparent():
    scope = Scope()
    ch_scope = Scope(scope)
    ch_ch_scope = Scope(ch_scope)
    scope['a'] = 1
    scope['b'] = 2
    ch_scope['a'] = 3
    ch_scope['c'] = 4
    assert ch_ch_scope['a'] == 3
    assert ch_ch_scope['b'] == 2
    assert ch_ch_scope['c'] == 4
    assert ch_scope['a'] == 3
    assert ch_scope['b'] == 2


def test_scope_keyerror():
    scope = Scope()
    ch_scope = Scope(scope)
    ch_ch_scope = Scope(ch_scope)
    scope['a'] = 1
    ch_scope['b'] = 2
    ch_ch_scope['c'] = 3
    with pytest.raises(KeyError):
        ch_ch_scope['d']


def test_function_definition():
    definition = FunctionDefinition('add', Function(
        ['a', 'b'],
        [
            BinaryOperation(
                Reference('a'),
                '+',
                Reference('b')
            )
        ]
    ))

    call = FunctionCall(Reference('add'), [Number(2), Number(3)])

    s = Scope()
    assert isinstance(definition.evaluate(s), Function)
    assert call.evaluate(s) == Number(5)


def test_conditional_two_branches():
    definition = FunctionDefinition('is_odd', Function(
        ['a'],
        [
            Conditional(
                BinaryOperation(
                    BinaryOperation(Reference('a'), '%', Number(2)),
                    '==',
                    Number(0)
                ),
                [Number(0)],
                [Number(1)]
            )
        ]
    ))

    call1 = FunctionCall(Reference('is_odd'), [Number(1)])
    call2 = FunctionCall(Reference('is_odd'), [Number(2)])

    s = Scope()
    definition.evaluate(s)
    assert call1.evaluate(s) == Number(1)
    assert call2.evaluate(s) == Number(0)


def test_conditional_one_branch():
    definition = FunctionDefinition('make_odd', Function(
        ['a'],
        [
            Conditional(
                BinaryOperation(
                    BinaryOperation(Reference('a'), '%', Number(2)),
                    '==',
                    Number(1)
                ),
                [BinaryOperation(Reference('a'), '*', Number(2))],
                None
            )
        ]
    ))

    call1 = FunctionCall(Reference('make_odd'), [Number(3)])
    call2 = FunctionCall(Reference('make_odd'), [Number(2)])

    s = Scope()
    definition.evaluate(s)
    assert call1.evaluate(s) == Number(6)
    assert call2.evaluate(s) == Number(0)


def test_print(capsys):
    definition = FunctionDefinition('print', Function(
        ['a'],
        [Print(Reference('a'))]
    ))

    call = FunctionCall(Reference('print'), [Number(1)])

    s = Scope()
    assert isinstance(definition.evaluate(s), Function)
    call.evaluate(s)
    out, err = capsys.readouterr()
    assert not err
    assert out == '1\n'


def test_read(monkeypatch):
    a = 10
    monkeypatch.setattr('builtins.input', lambda: "{}\n".format(a))
    s = Scope()
    assert isinstance(Read('a').evaluate(s), Number)
    assert Reference('a').evaluate(s) == Number(a)


def test_function_call():
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

    call = FunctionCall(Reference('gcd'), [Number(8), Number(12)])

    s = Scope()
    definition.evaluate(s)
    assert call.evaluate(s) == Number(4)


def test_function_call_empty_body():
    definition = FunctionDefinition('empty_func', Function(
        ['a'],
        None
    ))

    call = FunctionCall(Reference('empty_func'), [Number(6)])

    s = Scope()
    definition.evaluate(s)
    assert call.evaluate(s) == Number(0)


def test_reference():
    definition = FunctionDefinition('prod', Function(
        ['a', 'b'],
        [BinaryOperation(Reference('a'), '*', Reference('b'))]
    ))

    a, b = 10**2, 10**5
    call = FunctionCall(Reference('prod'), [Number(a), Number(b)])

    s = Scope()
    definition.evaluate(s)
    assert call.evaluate(s) == Number(a * b)


def check_binary_operation(a, op, b, res):
    return BinaryOperation(
        Number(a),
        op,
        Number(b)
    ).evaluate(Scope()) == Number(res)


def test_binary_operation():
    assert check_binary_operation(1, '+', 2, 3)
    assert check_binary_operation(1, '-', 2, -1)
    assert check_binary_operation(2, '*', 2, 4)
    assert check_binary_operation(3, '/', 2, 1)
    assert check_binary_operation(3, '%', 2, 1)
    assert check_binary_operation(2, '==', 2, True)
    assert check_binary_operation(1, '!=', 2, True)
    assert check_binary_operation(1, '<', 2, True)
    assert check_binary_operation(1, '>', 2, False)
    assert check_binary_operation(1, '<=', 2, True)
    assert check_binary_operation(1, '>=', 2, False)
    assert check_binary_operation(2, '&&', 0, False)
    assert (check_binary_operation(3, '||', 8, 3) or
            check_binary_operation(3, '||', 8, 8))


def test_unary_operation():
    s = Scope()
    assert UnaryOperation('-', Number(8)).evaluate(s) == Number(-8)
    assert UnaryOperation('!', Number(8)).evaluate(s) == Number(False)
    assert UnaryOperation('!', Number(0)).evaluate(s) == Number(True)


def test_factorial():
    definition = FunctionDefinition('fac', Function(
        ['n'],
        [
            Conditional(
                BinaryOperation(Reference('n'), '==', Number(1)),
                [Reference('n')],
                [BinaryOperation(
                    Reference('n'),
                    '*',
                    FunctionCall(
                        Reference('fac'),
                        [BinaryOperation(Reference('n'), '-', Number(1))]
                    )
                )]
            )
        ]
    ))

    n = 10
    call = FunctionCall(Reference('fac'), [Number(n)])

    s = Scope()
    definition.evaluate(s)
    assert call.evaluate(s) == Number(math.factorial(n))


if __name__ == "__main__":
    pytest.main()
