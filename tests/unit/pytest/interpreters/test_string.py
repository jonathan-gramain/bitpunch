#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

#
# Test string type
#

expected_string_table = ['Bonjour', 'Hello', 'Guten Tag', 'Hola', 'Privet']

spec_string_table = """
let cstr = [] byte: string { boundary: '\\0'; };

file {
    string_table: [] cstr;
}
"""

data_string_table_1 = """
"Bonjour" 00 "Hello" 00 "Guten Tag" 00 "Hola" 00 "Privet" 00
"""



spec_string_table_multi_char_boundary = """
let cstr = [] byte: string { boundary: '--delimiter--'; };

file {
    string_table: [] cstr;
}
"""

data_string_table_multi_char_boundary_1 = """
"Bonjour"
"--delimiter--"
"Hello"
"--delimiter--"
"Guten Tag"
"--delimiter--"
"Hola"
"--delimiter--"
"Privet"
"""



@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_string_table,
        'data': data_string_table_1,
        'expected': expected_string_table,
    }, {
        'spec': spec_string_table_multi_char_boundary,
        'data': data_string_table_multi_char_boundary_1,
        'expected': expected_string_table,
    }])
def params_string(request):
    return conftest.make_testcase(request.param)

def test_string(params_string):
    params = params_string
    dtree, expected = params['dtree'], params['expected']

    assert len(dtree.string_table) == len(expected)
    for i, s in enumerate(dtree.string_table):
        assert model.make_python_object(dtree.string_table[i]) == expected[i]
