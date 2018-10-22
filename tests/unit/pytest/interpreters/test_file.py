#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

#
# Test file filter
#


spec_file_1 = """

let pouet = _file_ { @path = "/tmp/bitpunch.test.file"; };

file {}
"""

data_file_1 = """
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

    with open('/tmp/bitpunch.test.file', 'w') as f:
        f.write('foobar');

    assert len(dtree.pouet) == 6
