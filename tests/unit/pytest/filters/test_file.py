#!/usr/bin/env python

import os

import pytest

from bitpunch import model
import conftest

TEST_FILE_PATH = '/tmp/bitpunch.test.file'

#
# Test file filter
#


spec_file_basic = """

let root = _file_ {{ @path = "{file_path}"; }};

file {{}}
""".format(file_path=TEST_FILE_PATH)

data_file_basic = """
"""



@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_basic,
        'data': data_file_basic,
    }])
def params_file_basic(request):
    return conftest.make_testcase(request.param)

def test_file_basic(params_file_basic):
    params = params_file_basic
    dtree = params['dtree']

    # since evaluation of file filter is deferred, it's ok to create
    # the file after creating the dtree
    with open('/tmp/bitpunch.test.file', 'w') as f:
        f.write('foobar');

    assert len(dtree.root) == 6
    assert dtree.root == 'foobar'
    assert dtree.eval_expr('root') == 'foobar'
    assert dtree.eval_expr('root[..3]') == 'foo'

    os.unlink(TEST_FILE_PATH)



spec_file_struct = """

let root = _file_ {{ @path = "{file_path}"; }} <> struct {{
    contents: [] byte;
}};

file {{}}
""".format(file_path=TEST_FILE_PATH)

data_file_struct = """
"""



@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_struct,
        'data': data_file_struct,
    }])
def params_file_struct(request):
    return conftest.make_testcase(request.param)

def test_file_struct(params_file_struct):
    params = params_file_struct
    dtree = params['dtree']

    # since evaluation of file filter is deferred, it's ok to create
    # the file after creating the dtree
    with open('/tmp/bitpunch.test.file', 'w') as f:
        f.write('foobar');

    assert len(dtree.root.contents) == 6
    assert dtree.root.contents == 'foobar'
    assert dtree.eval_expr('root.contents') == 'foobar'
    assert model.make_python_object(dtree.root) == {
        'contents': 'foobar',
    }
    assert dtree.root.contents[:3] == 'foo'
    assert dtree.eval_expr('root.contents[..3]') == 'foo'

    os.unlink(TEST_FILE_PATH)
