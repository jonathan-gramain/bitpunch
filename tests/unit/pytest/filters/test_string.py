#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

#
# Test string type
#


spec_string_table = """
let cstr = string { @boundary: '\\0'; };

env("DATASOURCE") <> struct {
    string_table: [] cstr;
};
"""

data_string_table_1 = """
"Bonjour" 00 "Hello" 00 "Guten Tag" 00 "Hola" 00 "Privet" 00
"""



spec_string_table_multi_char_boundary = """
let cstr = string { @boundary: '--delimiter--'; };

env("DATASOURCE") <> struct {
    string_table: [] cstr;
};
"""

data_string_table_multi_char_boundary = """
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


spec_string_table_dynamic_boundary_1 = """
env("DATASOURCE") <> struct {
    let cstr = string { @boundary: delimiter; };

    delimiter: string { @boundary: "\\0"; };
    string_table: [] cstr;
};
"""

data_string_table_dynamic_boundary_1 = """
"<coOldeLimiTer>" 00
"Bonjour"
"<coOldeLimiTer>"
"Hello"
"<coOldeLimiTer>"
"Guten Tag"
"<coOldeLimiTer>"
"Hola"
"<coOldeLimiTer>"
"Privet"
"""

spec_string_table_conditional_boundary = """
env("DATASOURCE") <> struct {
    let cstr = string {
        if (is_multichar == 1) {
            @boundary: "|---|";
        } else {
            @boundary: "|";
        }
    };

    is_multichar: byte <> integer { @signed: false; };
    string_table: [] cstr;
};
"""

data_string_table_conditional_boundary_single = """
00
"Bonjour|Hello|Guten Tag|Hola|Privet"
"""

data_string_table_conditional_boundary_multi = """
01
"Bonjour|---|Hello|---|Guten Tag|---|Hola|---|Privet"
"""

spec_file_string_table_dynamic_boundary_2 = """

let u8 = byte <> integer { @signed: false; };

env("DATASOURCE") <> struct {
    let BoundedString = string { @boundary: boundary; };

    boundary_size: u8;
    boundary: [boundary_size] byte <> string;

    string_table: [] BoundedString;
};

"""

data_file_string_table_dynamic_boundary_2_1 = """
# delimiter size
01
# delimiter
00
# values
"Bonjour" 00
"Hello" 00
"Guten Tag" 00
"Hola" 00
"Privet" 00
"""

data_file_string_table_dynamic_boundary_2_2 = """
# delimiter size
02
# delimiter
"  "
# values
"Bonjour  Hello  Guten Tag  Hola  Privet"
"""

data_file_string_table_dynamic_boundary_2_3 = """
# delimiter size
09
# delimiter
"--delim--"
# values
"Bonjour--delim--Hello--delim--Guten Tag--delim--Hola--delim--Privet--delim--"
"""



@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_string_table,
        'data': data_string_table_1,
    }, {
        'spec': spec_string_table_multi_char_boundary,
        'data': data_string_table_multi_char_boundary,
    }, {
        'spec': spec_string_table_dynamic_boundary_1,
        'data': data_string_table_dynamic_boundary_1,
    }, {
        'spec': spec_file_string_table_dynamic_boundary_2,
        'data': data_file_string_table_dynamic_boundary_2_1,
    }, {
        'spec': spec_file_string_table_dynamic_boundary_2,
        'data': data_file_string_table_dynamic_boundary_2_2,
    }, {
        'spec': spec_file_string_table_dynamic_boundary_2,
        'data': data_file_string_table_dynamic_boundary_2_3,
    }, {
        'spec': spec_string_table_conditional_boundary,
        'data': data_string_table_conditional_boundary_single,
    }, {
        'spec': spec_string_table_conditional_boundary,
        'data': data_string_table_conditional_boundary_multi,
    }])
def params_string(request):
    return conftest.make_testcase(request.param)

def test_string(params_string):
    params = params_string
    dtree = params['dtree']

    assert len(dtree.string_table) == 5
    for i, s in enumerate(dtree.string_table):
        assert model.make_python_object(dtree.string_table) == \
            ['Bonjour', 'Hello', 'Guten Tag', 'Hola', 'Privet']
