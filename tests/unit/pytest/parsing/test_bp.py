#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

spec_file_bp_1 = """

let u8 = byte[4]: integer { signed: false; };

let Foobar = struct {
    value: byte[6]: string;
};

file {
    foobars: Foobar[5];

    let ?foobar_slice = foobars[2..];
    let ?last_foobar = ?foobar_slice[-1];
    let ?last_foobar_value = ?last_foobar.value;
}
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
    dtree = model.DataTree(data, spec)
