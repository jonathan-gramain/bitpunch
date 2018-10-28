#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

spec_empty_struct = """

file {
    empty: struct {};
    foo: byte;
}

"""

data_empty_struct = """
00
"""


@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_empty_struct,
        'data': data_empty_struct,
    }])
def params_empty_struct(request):
    return conftest.make_testcase(request.param)


def test_empty_struct(params_empty_struct):
    params = params_empty_struct
    dtree = params['dtree']

    assert len(list(dtree.empty.iter_keys())) == 0
    assert memoryview(dtree.empty) == ''
    assert model.make_python_object(dtree.empty) == {}
    assert dtree.eval_expr('sizeof empty') == 0
