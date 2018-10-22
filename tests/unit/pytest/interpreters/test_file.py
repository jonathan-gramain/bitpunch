#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

#
# Test file filter
#


spec_file_1 = """

let pouet = _file_ { @path = "/etc/hosts"; };

file {}
"""

data_file_1 = """
"Bonjour" 00 "Hello" 00 "Guten Tag" 00 "Hola" 00 "Privet" 00
"""



@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_1,
        'data': data_file_1,
    }])
def params_file(request):
    return conftest.make_testcase(request.param)

def test_file(params_file):
    params = params_file
    dtree = params['dtree']

    assert len(dtree.string_table) == 5
    for i, s in enumerate(dtree.string_table):
        assert model.make_python_object(dtree.string_table) == \
            ['Bonjour', 'Hello', 'Guten Tag', 'Hola', 'Privet']
