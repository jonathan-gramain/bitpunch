#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

spec_file_bp_1 = """

let u8 = [4] byte <> integer { @signed: false; };

let Foobar = struct {
    value: [6] byte <> string;
};

let Schema = struct {
    foobars: [5] Foobar;

    let ?foobar_slice = foobars[2..];
    let ?last_foobar = ?foobar_slice[-1];
    let ?last_foobar_value = ?last_foobar.value;
};
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_bp_1,
    }])
def params_bp(request):
    return request.param

def test_bp(params_bp):
    params = params_bp
    spec = params['spec']

    data = 'foobarfoobarfoobarfoobarfoobar'
    board = model.Board()
    board.add_spec('Spec', spec)
    board.add_data_source('data', data)
    dtree = board.eval_expr('data <> Spec.Schema')


spec_file_invalid_attribute = """

let u8 = [4] byte <> integer { @signed: false; };

let Foobar = struct {
    value: [6] byte <> string;

    $foo: 42; // invalid
};

let Schema = struct {
    foobars: [5] Foobar;
};
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_invalid_attribute,
    }])
def params_invalid_bp(request):
    return request.param

def test_invalid_bp(params_invalid_bp):
    params = params_invalid_bp
    spec = params['spec']

    with pytest.raises(OSError):
        board = model.Board()
        board.add_spec('Spec', spec)
