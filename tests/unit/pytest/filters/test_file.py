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


spec_file_with_outer_scope = """

let UnsignedInt = integer {{ @signed = false; @endian = 'little'; }};
let u8  = [1] byte <> UnsignedInt;

file {{
    file_attr: struct {{
        nb_bytes: u8;
    }};
    let root = _file_ {{ @path = "{file_path}"; }} <> struct {{
        contents: [file_attr.nb_bytes] byte;
        junk: [] byte;
    }};
}}
""".format(file_path=TEST_FILE_PATH)

data_file_with_outer_scope = """
03     # limit contents size to 3 bytes
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_with_outer_scope,
        'data': data_file_with_outer_scope,
    }])
def params_file_with_outer_scope(request):
    return conftest.make_testcase(request.param)

def test_file_with_outer_scope(params_file_with_outer_scope):
    params = params_file_with_outer_scope
    dtree = params['dtree']

    # since evaluation of file filter is deferred, it's ok to create
    # the file after creating the dtree
    with open('/tmp/bitpunch.test.file', 'w') as f:
        f.write('foobar');

    assert len(dtree.root.contents) == 3
    assert len(dtree.root.junk) == 3
    assert dtree.root.contents == 'foo'
    assert dtree.root.junk == 'bar'
    assert dtree.eval_expr('root.contents') == 'foo'
    assert dtree.eval_expr('root.junk') == 'bar'
    assert model.make_python_object(dtree.root) == {
        'contents': 'foo',
        'junk': 'bar',
    }
    assert dtree.root.contents[:2] == 'fo'
    assert dtree.root.junk[:2] == 'ba'
    assert dtree.eval_expr('root.contents[..2]') == 'fo'
    assert dtree.eval_expr('root.junk[..2]') == 'ba'

    os.unlink(TEST_FILE_PATH)
