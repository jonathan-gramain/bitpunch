#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

spec_file_1 = """

type u8 byte: integer(signed=false);

struct Thing {
    u8 nb_props;
    ThingProp[nb_props] props;
};

struct ThingProp {
    byte[4] name: string;
};

file {
    u8 nb_things;
    Thing[nb_things] things;
}

"""

data_file_1 = """
03
# Item 0
02
"abcd"
"efgh"
# Item 1
01
"ijkl"
# Item 2
03
"mnop"
"qrst"
"uvwx"
"""


@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_1,
        'data': data_file_1,
    }])
def params_scoped_eval(request):
    return conftest.make_testcase(request.param)


def test_scoped_eval(params_scoped_eval):
    params = params_scoped_eval
    dtree = params['dtree']

    assert memoryview(model.eval('things[1].props[0]', dtree)) == 'ijkl'

    thing = dtree.things[1]
    assert memoryview(model.eval('props[0]', thing)) == 'ijkl'
