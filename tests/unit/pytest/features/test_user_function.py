#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

def get_answer_to_universe():
    return 42

spec_file_user_function_in_expr = """

let Schema = struct {
    contents: [] byte;
};

"""

data_file_user_function_in_expr = """
"Some useless contents"
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_user_function_in_expr,
        'data': data_file_user_function_in_expr,
    }])
def params_user_function_in_expr(request):
    return conftest.make_testcase(request.param)


def test_user_function_in_expr(params_user_function_in_expr):
    params = params_user_function_in_expr
    board, dtree = params['board'], params['dtree']

    board.register_function('get_answer_to_universe', get_answer_to_universe)

    assert dtree.eval_expr('get_answer_to_universe') == 42


spec_file_user_function_extern = """

// declare external function
extern get_answer_to_universe;

// TODO: use operators on external function returned value, not working
// now because of strict type matching for operators

let the_answer = get_answer_to_universe;

let Schema = struct {
    contents: [] byte;
};

"""

data_file_user_function_extern = """
"Some useless contents"
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_user_function_extern,
        'data': data_file_user_function_extern,
    }])
def params_user_function_extern(request):
    return conftest.make_testcase(request.param)


def test_user_function_extern(params_user_function_extern):
    params = params_user_function_extern
    board, dtree = params['board'], params['dtree']

    board.register_function('get_answer_to_universe', get_answer_to_universe)

    assert board.eval_expr('Spec.the_answer') == 42


def test_user_function_extern_use_spec():
    board = model.Board()
    board.use_spec(spec_file_user_function_extern)
    board.register_function('get_answer_to_universe', get_answer_to_universe)

    assert board.eval_expr('the_answer') == 42
