#!/usr/bin/env python

import os

import pytest

from bitpunch import model
import conftest

TEST_FILE_PATH = '/tmp/bitpunch.test.file'

def update_test_file(contents):
    with open(TEST_FILE_PATH, 'w') as f:
        f.write(contents);
    model.notify_file_change(TEST_FILE_PATH)


def make_testcase(param):
    update_test_file(param['file_data'])

    board = model.Board()
    board.add_spec('Spec', param['spec'])
    if 'data' in param:
        # data is provided externally to the spec (other data sources
        # may be defined in the spec but not top-level)
        data = conftest.to_bytes(param['data'])
        board.add_data_source('data', data)
        param['dtree'] = board.eval_expr('data <> Spec.Schema')
        param['data'] = data
    else:
        # data is defined in the spec as "data" item
        param['dtree'] = board.eval_expr('Spec.data')

    if 'BITPUNCH_TEST_ENABLE_CLI' in os.environ:
        cli = CLI()
        cli.attach_data_tree(param['dtree'])
        cli.cmdloop()

    return param

#
# Test file filter
#


spec_file_basic = """

let data = file {{ @path: "{file_path}"; }};

""".format(file_path=TEST_FILE_PATH)

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_basic,
        'file_data': 'foobar',
    }])
def params_file_basic(request):
    return make_testcase(request.param)

def test_file_basic(params_file_basic):
    params = params_file_basic
    dtree = params['dtree']

    assert len(dtree) == 6
    assert dtree == 'foobar'
    assert dtree.eval_expr('self') == 'foobar'
    assert dtree.eval_expr('self[..3]') == 'foo'

    os.unlink(TEST_FILE_PATH)


spec_file_struct = """

let data = file {{ @path: "{file_path}"; }} <> struct {{
    contents: [] byte;
}};

""".format(file_path=TEST_FILE_PATH)

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_struct,
        'file_data': 'foobar',
    }])
def params_file_struct(request):
    return make_testcase(request.param)

def test_file_struct(params_file_struct):
    params = params_file_struct
    dtree = params['dtree']

    assert len(dtree.contents) == 6
    assert dtree.contents == 'foobar'
    assert dtree.eval_expr('contents') == 'foobar'
    assert model.make_python_object(dtree) == {
        'contents': 'foobar',
    }
    assert dtree.contents[:3] == 'foo'
    assert dtree.eval_expr('contents[..3]') == 'foo'

    os.unlink(TEST_FILE_PATH)


spec_file_anonymous_struct = """

let FILE = file {{ @path: "{file_path}"; }};

let data = FILE <> struct {{
    contents: [] byte;
}};

""".format(file_path=TEST_FILE_PATH)



spec_file_with_outer_scope = """

let UnsignedInt = integer {{ @signed: false; @endian: 'little'; }};
let u8  = [1] byte <> UnsignedInt;

let Schema = struct {{
    file_attr: struct {{
        nb_bytes: u8;
    }};
    let root = file {{ @path: "{file_path}"; }} <> struct {{
        contents: [file_attr.nb_bytes] byte;
        junk: [] byte;
    }};
}};
""".format(file_path=TEST_FILE_PATH)

data_file_with_outer_scope = """
03     # limit contents size to 3 bytes
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_with_outer_scope,
        'data': data_file_with_outer_scope,
        'file_data': 'foobar',
    }])
def params_file_with_outer_scope(request):
    return make_testcase(request.param)

def test_file_with_outer_scope(params_file_with_outer_scope):
    params = params_file_with_outer_scope
    dtree = params['dtree']

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


spec_file_string_array = """

let data = file {{ @path: "{file_path}"; }}
    <> [] string {{ @boundary: '\\n'; }};

""".format(file_path=TEST_FILE_PATH)


@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_string_array,
        'file_data': 'hello\nhola\nbonjour\n',
    }])
def params_file_string_array(request):
    return make_testcase(request.param)

def test_file_string_array(params_file_string_array):
    params = params_file_string_array
    dtree = params['dtree']

    assert len(dtree) == 3
    assert model.make_python_object(dtree) == ['hello', 'hola', 'bonjour']
    assert dtree.eval_expr('self[1]') == 'hola'
    assert dtree.eval_expr('^self[1]') == 'hola'
    assert dtree.eval_expr('^self') == 'hello\nhola\nbonjour\n'
    assert dtree.eval_expr('^^self') == 'hello\nhola\nbonjour\n'

    os.unlink(TEST_FILE_PATH)
