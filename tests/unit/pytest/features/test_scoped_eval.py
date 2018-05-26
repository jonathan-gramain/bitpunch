#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

spec_file_1 = """

let u8 = byte <> integer { @signed: false; };

let Thing = struct {
    nb_props: u8;
    props:    [nb_props] ThingProp;
};

let ThingProp = struct {
    name: [4] byte <> string;
};

let AsArray = struct {
    values: [4] u8;
};

file {
    nb_things: u8;
    things:    [nb_things] Thing;
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
    int_values = [ord('i'), ord('j'), ord('k'), ord('l')]

    assert memoryview(dtree.eval_expr('things[1].props[0]')) == 'ijkl'
    values = dtree.eval_expr('(things[1].props[0] <> AsArray).values')
    assert model.make_python_object(values) == int_values

    thing = dtree.things[1]
    assert memoryview(thing.eval_expr('props[0]')) == 'ijkl'
    thing_values = thing.eval_expr('(props[0] <> AsArray).values')
    assert model.make_python_object(thing_values) == int_values

    prop = thing.props[0]
    assert memoryview(prop.eval_expr('name')) == 'ijkl'
    prop_values = prop.eval_expr('(name <> AsArray).values')
    assert model.make_python_object(prop_values) == int_values

    # check that 'require_dpath=True' is honored
    prop_first_value = prop.eval_expr('(name <> AsArray).values[0]')
    # a value is returned by default for integers, cannot use
    # memoryview() on it
    with pytest.raises(TypeError):
        memoryview(prop_first_value)
    # 'require_dpath=True' forces to return a dpath object for
    # items, which implements buffer interface
    prop_first_value_tracker = prop.eval_expr('(name <> AsArray).values[0]',
                                              require_dpath=True)
    memoryview(prop_first_value_tracker)
