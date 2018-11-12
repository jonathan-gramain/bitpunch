#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

spec_file_named_exprs_1_1 = """

let u16 = [2] byte <> integer { @signed = false; @endian = 'little'; };

let Number = struct {
    raw: [2] byte;
    let ?value = raw <> integer { @signed = false; @endian = 'little'; };
};

env("DATASOURCE") <> struct {
    typed:   [3] u16;
    untyped: [3] Number;
};

"""

data_file_named_exprs_1_1 = """
01 00 02 00 03 00
01 00 02 00 03 00
"""


spec_file_named_exprs_1_2 = """

let u16 = [2] byte <> integer { @signed = false; @endian = 'little'; };

let Number = struct {
    raw: [?byte_count] byte;

    let ?byte_count = 2;
    let ?value = raw <> integer { @signed = false; @endian = 'little'; };
};

env("DATASOURCE") <> struct {
    typed:   [3] u16;
    untyped: [3] Number;
};

"""

data_file_named_exprs_1_2 = """
01 00 02 00 03 00
01 00 02 00 03 00
"""


spec_file_named_exprs_1_3 = """

let ?global_byte_count = 2;

let u16 = [?global_byte_count] byte <> integer { @signed = false;
                                                 @endian = 'little'; };

let Number = struct {
    raw: [?byte_count] byte;

    let ?byte_count = ?global_byte_count;
    let ?value = raw <> integer { @signed = false; @endian = 'little'; };

    @key = raw <> integer { @signed = false; @endian = 'little'; };
};

env("DATASOURCE") <> struct {
    typed:   [3] u16;
    untyped: [3] Number;
};

"""

data_file_named_exprs_1_3 = """
01 00 02 00 03 00
01 00 02 00 03 00
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_named_exprs_1_1,
        'data': data_file_named_exprs_1_1,
    }, {
        'spec': spec_file_named_exprs_1_2,
        'data': data_file_named_exprs_1_2,
    }, {
        'spec': spec_file_named_exprs_1_3,
        'data': data_file_named_exprs_1_3,
    }])
def params_named_exprs_1(request):
    return conftest.make_testcase(request.param)


def test_named_exprs_1(params_named_exprs_1):
    params = params_named_exprs_1
    dtree = params['dtree']
    for i in range(3):
        assert dtree.typed[i] == i + 1
        assert dtree.untyped[i]['?value'] == i + 1
        assert dtree.eval_expr('untyped[{0}].?value'.format(i)) == i + 1
        assert (dtree.eval_expr('&untyped[{0}].?value'.format(i))
                == 6 + 2 * i)


spec_file_named_exprs_2_1 = """

let u16 = [2] byte <> integer { @signed = false; @endian = 'little'; };

env("DATASOURCE") <> struct {
    typed:   [3] u16;
    untyped: [6] byte;

    let ?untyped_as_typed = untyped <> [] u16;
};

"""

spec_file_named_exprs_2_2 = """

let u16 = [2] byte <> integer { @signed = false; @endian = 'little'; };

let Foo = struct {
    bar: [] byte;
};

env("DATASOURCE") <> struct {
    typed:   [3] u16;
    untyped: [6] byte;

    let ?untyped_as_typed = untyped <> [] byte <> [] u16;
};

"""

data_file_named_exprs_2 = """
01 00 02 00 03 00
01 00 02 00 03 00
"""


@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_named_exprs_2_1,
        'data': data_file_named_exprs_2,
    }, {
        'spec': spec_file_named_exprs_2_2,
        'data': data_file_named_exprs_2,
    }])
def params_named_exprs_2(request):
    return conftest.make_testcase(request.param)


def test_named_exprs_2(params_named_exprs_2):
    params = params_named_exprs_2
    dtree = params['dtree']
    for i in range(3):
        assert dtree.typed[i] == i + 1
        assert dtree['?untyped_as_typed'][i] == i + 1
        assert dtree.eval_expr('?untyped_as_typed[{0}]'.format(i)) == i + 1


spec_file_named_exprs_3 = """

let u16 = [2] byte <> integer { @signed = false; @endian = 'little'; };

let Number = struct {
    value: u16;

    @key = value;
};

env("DATASOURCE") <> struct {
    typed:   [3] u16;
    numbers: [3] Number;

    let ?numbers = numbers;
};

"""

data_file_named_exprs_3 = """
01 00 02 00 03 00
01 00 02 00 03 00
"""


@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_named_exprs_3,
        'data': data_file_named_exprs_3,
    }])
def params_named_exprs_3(request):
    return conftest.make_testcase(request.param)


def test_named_exprs_3(params_named_exprs_3):
    params = params_named_exprs_3
    dtree = params['dtree']
    for i in range(3):
        assert dtree.typed[i] == i + 1
        assert dtree['?numbers'][i].value == i + 1
        assert dtree.eval_expr('?numbers[{0}].value'.format(i)) == i + 1


spec_file_named_exprs_4 = """

let u16 = [2] byte <> integer { @signed = false; @endian = 'little'; };

let Number = struct {
    value: u16;
};

env("DATASOURCE") <> struct {
    data: [] byte;
    let ?second_and_third_numbers = data[sizeof(Number)..] <> [] Number;
};

"""

data_file_named_exprs_4 = """
01 00 02 00 03 00
"""


@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_named_exprs_4,
        'data': data_file_named_exprs_4,
    }])
def params_named_exprs_4(request):
    return conftest.make_testcase(request.param)


def test_named_exprs_4(params_named_exprs_4):
    params = params_named_exprs_4
    dtree = params['dtree']
    numbers = dtree['?second_and_third_numbers']
    assert numbers[0].value == 2
    assert numbers[1].value == 3



spec_file_named_exprs_5 = """

let ?one = ?minus_one_plus_two;
let ?minus_one_plus_two = -1 + 2;

let ?false = false;
let ?little = 'little';

let u8 = [?one] byte <> integer { @signed = ?false; @endian = ?little; };
let u16 = [?two] byte <> integer { @signed = ?false; @endian = ?little; };

let ?two = ?one + ?one;

let Series = struct {
    values:        [?series_length] u16;

    let ?my_index = index(series, self);
    let ?series_length = series_length[?my_index];
};

env("DATASOURCE") <> struct {
    nb_series:     u8;
    series_length: [nb_series] u8;
    series:        [nb_series] Series;
    let ?series_type = Series;
    let ?first_series = series[0];
    let ?first_series_first_value = series[0].values[0];
};

"""

data_file_named_exprs_5 = """
03
03 02 04
01 00 02 00 03 00
01 00 02 00
01 00 02 00 03 00 04 00
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_named_exprs_5,
        'data': data_file_named_exprs_5,
    }])
def params_named_exprs_5(request):
    return conftest.make_testcase(request.param)


def test_named_exprs_5(params_named_exprs_5):
    params = params_named_exprs_5
    dtree = params['dtree']
    assert dtree.nb_series == 3
    assert model.make_python_object(dtree.series[2].values) == [1, 2, 3, 4]
    assert model.make_python_object(dtree.series[1].values) == [1, 2]
    assert model.make_python_object(dtree.series[0].values) == [1, 2, 3]
    assert dtree['?first_series_first_value'] == 1
    assert dtree.eval_expr('?first_series.values[0]') == 1


spec_file_named_exprs_polymorphic = """

let Selector = byte <> integer { @signed = false; };
let Count = [2] byte <> integer { @signed = false; @endian = 'big'; };
let Value = Selector;


let T = struct {
    let IntArray = struct {
        count: Count;
        values: [count] Value;
        let ?my_type = ?type;
    };

    let Message = struct {
        value: string { @boundary = '\\n'; };
        let ?my_type = ?type;
    };

    type: Selector;
    if (type == 1) {
        arr: IntArray;
        let ?item = arr;
        let ?type = 'array';
    }
    if (type == 2) {
        message: Message;
        let ?item = message;
        let ?type = 'message';
    }
};

env("DATASOURCE") <> struct {
    items: [] T;
};

"""

spec_file_named_exprs_polymorphic_in_anonymous_1 = """

let Selector = byte <> integer { @signed = false; };
let Count = [2] byte <> integer { @signed = false; @endian = 'big'; };
let Value = Selector;


let T = struct {
    SubT;
};

let SubT = struct {
    let IntArray = struct {
        count: Count;
        values: [count] Value;
        let ?my_type = ?type;
    };

    let Message = struct {
        value: string { @boundary = '\\n'; };
        let ?my_type = ?type;
    };

    type: Selector;
    if (type == 1) {
        arr: IntArray;
        let ?item = arr;
        let ?type = 'array';
    }
    if (type == 2) {
        message: Message;
        let ?item = message;
        let ?type = 'message';
    }
};

env("DATASOURCE") <> struct {
    items: [] T;
};

"""

data_file_named_exprs_polymorphic = """
02 // Message
    "hello\n"
02 // Message
    "bonjour\n"
01 // IntArray
    00 05 // count
    01 02 03 04 05
02 // Message
    "last message\n"
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_named_exprs_polymorphic,
        'data': data_file_named_exprs_polymorphic,
    }, {
        'spec': spec_file_named_exprs_polymorphic_in_anonymous_1,
        'data': data_file_named_exprs_polymorphic,
    }])
def params_named_exprs_polymorphic(request):
    return conftest.make_testcase(request.param)


def test_named_exprs_polymorphic(params_named_exprs_polymorphic):
    params = params_named_exprs_polymorphic
    dtree = params['dtree']
    assert len(dtree.items) == 4
    assert str(dtree.items[1].message.value) == 'bonjour'
    assert str(dtree.items[1]['?item'].value) == 'bonjour'
    assert str(dtree.eval_expr('items[1].message.value')) == 'bonjour'
    assert isinstance(dtree.eval_expr('items[1].?item.value'),
                      model.DataItem)
    assert str(dtree.eval_expr('items[1].?item.value')) == 'bonjour'
    assert dtree.items[1]['?type'] == 'message'
    assert dtree.eval_expr('items[1].?type') == 'message'
    assert dtree.eval_expr('items[1].?type == "message"') == True
    assert dtree.eval_expr('items[1].?item.?my_type') == 'message'

    assert model.make_python_object(
        dtree.items[2].arr.values) == [1, 2, 3, 4, 5]
    assert model.make_python_object(
        dtree.items[2]['?item'].values) == [1, 2, 3, 4, 5]
    assert model.make_python_object(
        dtree.eval_expr('items[2].arr.values')) == [1, 2, 3, 4, 5]
    assert model.make_python_object(
        dtree.eval_expr('items[2].arr.values[1]')) == 2
    assert model.make_python_object(
        dtree.eval_expr('items[2].?item.values[1]')) == 2
    assert dtree.items[2]['?type'] == 'array'
    assert dtree.eval_expr('items[2].?type') == 'array'
    assert dtree.eval_expr('items[2].?type == "array"') == True
    assert dtree.eval_expr('items[2].?item.?my_type') == 'array'


spec_file_named_exprs_polymorphic_dpath_or_value = """

let Selector = byte <> integer { @signed = false; };

let T = struct {
    type: Selector;
    if (type == 1) {
        let ?poly = 'a_value';
    }
    if (type == 2) {
        let ?poly = a_dpath;
        a_dpath: [7] byte <> string;
    }
};

env("DATASOURCE") <> struct {
    items: [] T;
};

"""

data_file_named_exprs_polymorphic_dpath_or_value = """
01 // value
02 // dpath
    "a_dpath"
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_named_exprs_polymorphic_dpath_or_value,
        'data': data_file_named_exprs_polymorphic_dpath_or_value,
    }])
def params_named_exprs_polymorphic_dpath_or_value(request):
    return conftest.make_testcase(request.param)


def test_named_exprs_polymorphic_dpath_or_value(
        params_named_exprs_polymorphic_dpath_or_value):
    params = params_named_exprs_polymorphic_dpath_or_value
    dtree = params['dtree']
    assert len(dtree.items) == 2
    assert str(dtree.eval_expr('items[0].?poly')) == 'a_value'
    assert str(dtree.items[0].eval_expr('?poly')) == 'a_value'
    with pytest.raises(AttributeError):
        dtree.items[0].eval_expr('?poly').get_location()
    with pytest.raises(AttributeError):
        dtree.eval_expr('items[0].?poly').get_location()
    assert str(dtree.items[1].eval_expr('?poly')) == 'a_dpath'
    assert str(dtree.eval_expr('items[1].?poly')) == 'a_dpath'
    assert dtree.items[1].eval_expr('?poly').get_location() == (2, 7)
    assert dtree.eval_expr('items[1].?poly').get_location() == (2, 7)


spec_file_named_exprs_polymorphic_hydra = """

let Selector = byte <> integer { @signed = false; };

let T = struct {
    SubT;
    if (type == 8) {
        let ?item = 8;
    }
};

let SubT = struct {
    type: Selector;
    if (type != 8) {
        if (type == 1) {
            Sub2T;
        } 
        if (type == 7) {
            let ?item = 7;
        }
        if (type == 2 || type == 5 || type == 6) {
            Sub3T;
        }
        if (type == 3) {
            let ?item = type;
        }
        if (type == 4) {
            struct {
                struct {
                    let ?item = 2 + 2;
                    let ?item = 10;
                };
            };
        }
        let Sub2T = struct {
            let ?item = type;
        };

        let Sub3T = struct {
            if (type == 2) {
                Sub4T;
            } else if (type == 5) {
                Sub5T;
            } else {
                let ?item = 6;
            }
        };

        let Sub4T = struct {
            let ?item = 2;
        };

        let Sub5T = struct {
            let ?item = 5;
        };
    }
};

env("DATASOURCE") <> struct {
    items: [] T;
};

"""

data_file_named_exprs_polymorphic_hydra = """
01 02 03 04 05 06 07 08
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_named_exprs_polymorphic_hydra,
        'data': data_file_named_exprs_polymorphic_hydra,
    }])
def params_named_exprs_polymorphic_hydra(request):
    return conftest.make_testcase(request.param)


def test_named_exprs_polymorphic_hydra(params_named_exprs_polymorphic_hydra):
    params = params_named_exprs_polymorphic_hydra
    dtree = params['dtree']

    i = 1
    assert len(dtree.items) == 8
    for item in dtree.items:
        assert item['?item'] == i
        i += 1


spec_file_named_exprs_polymorphic_attribute = """

env("DATASOURCE") <> struct {
    encoded: byte <> integer { @signed = false; };
    data: [] byte;
    if (encoded == 0) {
        let ?items = data <> [] Item;
    } else {
        let ?items = data <> [] byte <> base64 <> [] Item;
    }

    let Item = struct {
        value: byte <> integer { @signed = false; };
        @last = value == 0;
    };
};

"""

data_file_named_exprs_polymorphic_attribute_1 = """
00
01 02 03 04 05 00
"""

data_file_named_exprs_polymorphic_attribute_2 = """
01
"AQIDBAUA"
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_named_exprs_polymorphic_attribute,
        'data': data_file_named_exprs_polymorphic_attribute_1,
    }, {
        'spec': spec_file_named_exprs_polymorphic_attribute,
        'data': data_file_named_exprs_polymorphic_attribute_2,
    }])
def params_named_exprs_polymorphic_attribute(request):
    return conftest.make_testcase(request.param)


def test_named_exprs_polymorphic_attribute(params_named_exprs_polymorphic_attribute):
    params = params_named_exprs_polymorphic_attribute
    dtree = params['dtree']

    i = 1
    assert len(dtree['?items']) == 6
    for item in dtree['?items']:
        if i == 6:
            assert item.value == 0
            assert dtree.eval_expr('?items[{0}].@last'.format(i - 1))
        else:
            assert item.value == i
            assert not dtree.eval_expr('?items[{0}].@last'.format(i - 1))
        i += 1


spec_file_named_exprs_invalid_1 = """

let u16 = [2] byte <> integer { @signed = false; @endian = 'little'; };

let Number = struct {
    value: u16;
};

env("DATASOURCE") <> struct {
    data: [] byte;
    let ?something = does_not_exist;
};

"""

spec_file_named_exprs_invalid_2 = """

let u16 = [2] byte <> integer { @signed = false; @endian = 'little'; };

let Number = struct {
    value: u16;
};

env("DATASOURCE") <> struct {
    data: [] byte;
    let ?something = ?does_not_exist;
};

"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_named_exprs_invalid_1,
    }, {
        'spec': spec_file_named_exprs_invalid_2,
    }])
def params_named_exprs_invalid(request):
    return request.param


def test_named_exprs_invalid(params_named_exprs_invalid):
    params = params_named_exprs_invalid
    spec = params['spec']
    with pytest.raises(OSError):
        dtree = model.DataTree('', spec)
