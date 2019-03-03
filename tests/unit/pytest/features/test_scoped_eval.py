#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

spec_file_1 = """

let u8 = byte <> integer { @signed: false; };

let Thing = struct {
    nb_props: u8;
    props:    [nb_props] ThingBox.ThingProp;
};

let AsArray = struct {
    values: [4] u8;
};

let ThingBox = struct {
    nb_things: u8;
    things:    [nb_things] Thing;

    let ThingProp = struct {
        name: [4] byte <> string;
    };
};

let Schema = ThingBox;

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

spec_file_2 = """

let u8 = byte <> integer { @signed: false; };

let Thing = struct {
    nb_props: u8;
    props:    [nb_props] ThingBox.ThingProp;
};

let AsArray = struct {
    values: [4] u8;
};

let ThingBox = struct {
    nb_things: u8;
    things:    [nb_things] Thing;
    prop_names: [] byte;

    let ThingProp = struct {
        start_off: u8;
        end_off: u8;

        let name = prop_names[start_off .. end_off] <> string;
    };
};

let Schema = ThingBox;

"""

data_file_2 = """
03
# Item 0
02
04 08 # -> "abcd"
00 04 # -> "efgh"
# Item 1
01
09 0d # -> "ijkl"
# Item 2
03
15 19 # -> "mnop"
0d 11 # -> "qrst"
11 15 # -> "uvwx"
# prop_names
"efgh"
"abcd"
00 # dummy padding byte
"ijkl"
"qrst"
"uvwx"
"mnop"
"""

spec_file_3a = """

let u8 = byte <> integer { @signed: false; };

let Thing = struct {
    nb_props: u8;
    props:    [nb_props] ThingProp;
    prop_names: [nb_props * 4] byte;
};

let AsArray = struct {
    values: [4] u8;
};

let ThingProp = struct {
    start_off: u8;
    end_off: u8;

    let name = Thing::prop_names[start_off .. end_off] <> string;
};

let ThingBox = struct {
    nb_things: u8;
    things:    [nb_things] Thing;
};

let Schema = ThingBox;

"""

spec_file_3b = """

let u8 = byte <> integer { @signed: false; };

let AsArray = struct {
    values: [4] u8;
};

let ThingProp = struct {
    start_off: u8;
    end_off: u8;

    let name = ThingBox::Thing::prop_names[start_off .. end_off] <> string;
};

let ThingBox = struct {
    nb_things: u8;
    things:    [nb_things] Thing;

    let Thing = struct {
        nb_props: u8;
        props:    [nb_props] ThingProp;
        prop_names: [nb_props * 4] byte;
    };
};

let Schema = ThingBox;

"""

data_file_3 = """
03
# Item 0
02
04 08 # -> "abcd"
00 04 # -> "efgh"
# prop_names for Item 0
"efgh"
"abcd"
# Item 1
01
00 04 # -> "ijkl"
# prop_names for Item 1
"ijkl"
# Item 2
03
08 0c # -> "mnop"
00 04 # -> "qrst"
04 08 # -> "uvwx"
# prop_names for Item 2
"qrst"
"uvwx"
"mnop"
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_1,
        'data': data_file_1,
    }, {
        'spec': spec_file_2,
        'data': data_file_2,
    }, {
        'spec': spec_file_3a,
        'data': data_file_3,
    }, {
        'spec': spec_file_3b,
        'data': data_file_3,
    }])
def params_scoped_eval(request):
    return conftest.make_testcase(request.param)


def test_scoped_eval(params_scoped_eval):
    params = params_scoped_eval
    dtree = params['dtree']
    int_values = [ord('i'), ord('j'), ord('k'), ord('l')]

    assert memoryview(dtree.eval_expr('things[1].props[0].name')) == 'ijkl'
    values = dtree.eval_expr('(things[1].props[0].name <> Spec.AsArray).values')
    assert model.make_python_object(values) == int_values

    thing = dtree.things[1]
    assert memoryview(thing.eval_expr('props[0].name')) == 'ijkl'
    thing_values = thing.eval_expr('(props[0].name <> Spec.AsArray).values')
    assert model.make_python_object(thing_values) == int_values

    prop = thing.props[0]
    assert memoryview(prop.eval_expr('name')) == 'ijkl'
    prop_values = prop.eval_expr('(name <> Spec.AsArray).values')
    assert model.make_python_object(prop_values) == int_values

    prop_first_value = prop.eval_expr('(name <> Spec.AsArray).values[0]')
    assert memoryview(prop_first_value) == 'i'

    
spec_file_nested_scoping_eval = """

let u8 = byte <> integer { @signed: false; };

let Foo = struct {
    bar: [2 * sizeof (Bar)] byte;
    let ?first_bar = bar[0..sizeof(Bar)] <> Bar;
    name: [3] byte <> string;

    let Bar = struct {
        name: [3] byte <> string;
        let ?foo_name = Foo::name;
    };
};

let Root = struct {
    foo: [sizeof (Foo)] byte;
};

let Schema = Root;

"""

data_file_nested_scoping_eval = """
"bar" # Bar #0 name
"bar" # Bar #1 name
"foo" # Foo name
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_nested_scoping_eval,
        'data': data_file_nested_scoping_eval,
    }])
def params_nested_scoping_eval(request):
    return conftest.make_testcase(request.param)


def test_nested_scoping_eval(params_nested_scoping_eval):
    params = params_nested_scoping_eval
    dtree = params['dtree']

    foo = dtree.eval_expr('foo <> Spec.Foo')
    bar = foo.eval_expr('bar[0..sizeof(Spec.Foo.Bar)] <> Spec.Foo.Bar')
    assert bar.eval_expr('?foo_name') == 'foo'

    two_bars = foo.eval_expr('bar[0..2 * sizeof(Spec.Foo.Bar)] <> [2] Spec.Foo.Bar')
    assert two_bars.eval_expr('self[1].?foo_name') == 'foo'

    assert dtree.eval_expr(
        '(foo <> Spec.Foo).?first_bar.?foo_name') == 'foo'
    # this checks that expression scoping retains outer filter scopes
    # during evaluation of members (here, evaluation of
    # <instance of Foo.Bar>.?foo_name expects to find a Foo type in its
    # scope, which should have been set and retained by evaluation of
    # (foo <> Foo)).
    assert dtree.eval_expr(
        '((foo <> Spec.Foo).bar[0..sizeof(Spec.Foo.Bar)]' +
        ' <> Spec.Foo.Bar).?foo_name') == 'foo'

    # variants with a few extra inserted filters
    assert dtree.eval_expr(
        '((foo <> Spec.Foo <> [] byte <> Spec.Foo)' +
        '.bar[0..sizeof(Spec.Foo.Bar)]' +
        ' <> [] byte <> Spec.Foo.Bar).?foo_name') == 'foo'

    assert dtree.eval_expr('(foo <> Spec.Foo <> Spec.Foo.Bar).?foo_name') == 'foo'

    # no Foo type in the expression, so ?foo_name cannot be evaluated
    with pytest.raises(ValueError):
        dtree.eval_expr('(foo <> Spec.Foo.Bar).?foo_name')
