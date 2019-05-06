#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

class Godify(object):
    def __init__(self):
        self.godify_dict = {
            'he': 'He',
            'him': 'Him',
            'you': 'thou',
        }

    def read(self, input_data):
        words = str(bytearray(input_data)).split(' ')
        return ' '.join(map(self._godify_word, words))

    def _godify_word(self, word):
        if word in self.godify_dict:
            return self.godify_dict[word]
        return word

spec_file_user_filter_in_expr = """

let Schema = struct {
    contents: [] byte;
};

"""

data_file_user_filter_in_expr = """
"What you see is what you get"
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_user_filter_in_expr,
        'data': data_file_user_filter_in_expr,
    }])
def params_user_filter_in_expr(request):
    return conftest.make_testcase(request.param)


def test_user_filter_in_expr(params_user_filter_in_expr):
    params = params_user_filter_in_expr
    board, dtree = params['board'], params['dtree']

    board.register_filter('godify', Godify)

    assert dtree.eval_expr('contents <> godify') == \
        "What thou see is what thou get"


spec_file_user_filter_extern = """

// declare external filter
extern godify {
    @in: 'bytes';
    @out: 'bytes';
};

let GodifiedSentence = string { @boundary: '\\n'; } <> godify;

let Schema = struct {
    contents: [] GodifiedSentence;
};

"""

data_file_user_filter_extern = """
"Do as she wants\n"
"Do as he wants\n"
"Do as you want\n"
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_user_filter_extern,
        'data': data_file_user_filter_extern,
    }])
def params_user_filter_extern(request):
    return conftest.make_testcase(request.param)


def test_user_filter_extern(params_user_filter_extern):
    params = params_user_filter_extern
    board, dtree = params['board'], params['dtree']

    board.register_filter('godify', Godify)

    assert model.make_python_object(
        dtree.eval_expr('contents')) == \
        ["Do as she wants",
         "Do as He wants",
         "Do as thou want"]


def test_user_filter_extern_use_spec():
    board = model.Board()
    board.add_data_source('data', conftest.to_bytes(
        data_file_user_filter_extern))
    board.use_spec(spec_file_user_filter_extern)
    board.register_filter('godify', Godify)

    dtree = board.eval_expr('data <> Schema')
    assert model.make_python_object(
        dtree.eval_expr('contents')) == \
        ["Do as she wants",
         "Do as He wants",
         "Do as thou want"]
