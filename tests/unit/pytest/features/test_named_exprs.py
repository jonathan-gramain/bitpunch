#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

spec_file_named_exprs_1_1 = """

let u16 = byte[2]: integer { signed: false; endian: 'little'; };

let Number = struct {
    raw: byte[2];
    let ?value = raw: integer { signed: false; endian: 'little'; };
};

file {
    typed:   u16[3];
    untyped: Number[3];
}

"""

data_file_named_exprs_1_1 = """
01 00 02 00 03 00
01 00 02 00 03 00
"""


spec_file_named_exprs_1_2 = """

let u16 = byte[2]: integer { signed: false; endian: 'little'; };

let Number = struct {
    raw: byte[?byte_count];

    let ?byte_count = 2;
    let ?value = raw: integer { signed: false; endian: 'little'; };
};

file {
    typed:   u16[3];
    untyped: Number[3];
}

"""

data_file_named_exprs_1_2 = """
01 00 02 00 03 00
01 00 02 00 03 00
"""


spec_file_named_exprs_1_3 = """

let ?global_byte_count = 2;

let u16 = byte[?global_byte_count]: integer { signed: false;
                                              endian: 'little'; };

let Number = struct {
    raw: byte[?byte_count];

    let ?byte_count = ?global_byte_count;
    let ?value = raw: integer { signed: false; endian: 'little'; };

    key ?value: integer { signed: false; endian: 'little'; };
};

file {
    typed:   u16[3];
    untyped: Number[3];
}

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

let u16 = byte[2]: integer { signed: false; endian: 'little'; };

file {
    typed:   u16[3];
    untyped: byte[6];

    let ?untyped_as_typed = untyped: u16[];
}

"""

spec_file_named_exprs_2_2 = """

let u16 = byte[2]: integer { signed: false; endian: 'little'; };

let Foo = struct {
    bar: byte[];
};

file {
    typed:   u16[3];
    untyped: byte[6];

    let ?untyped_as_typed = untyped: byte[]: u16[];
}

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

let u16 = byte[2]: integer { signed: false; endian: 'little'; };

let Number = struct {
    value: u16;

    key value;
};

file {
    typed:   u16[3];
    numbers: Number[3];

    let ?numbers = numbers;
}

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

let u16 = byte[2]: integer { signed: false; endian: 'little'; };

let Number = struct {
    value: u16;
};

file {
    data: byte[];
    let ?second_and_third_numbers = data[sizeof(Number)..]: Number[];
}

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

let u8 = byte[?one]: integer { signed: ?false; endian: ?little; };
let u16 = byte[?two]: integer { signed: ?false; endian: ?little; };

let ?two = ?one + ?one;

let Series = struct {
    values:        u16[?series_length];

    let ?my_index = index(series, self);
    let ?series_length = series_length[?my_index];
};

file {
    nb_series:     u8;
    series_length: u8[nb_series];
    series:        Series[nb_series];
    let ?series_type = Series;
    let ?first_series = series[0];
    let ?first_series_first_value = series[0].values[0];
}

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

let Selector = byte: integer { signed: false; };
let Count = byte[2]: integer { signed: false; endian: 'big'; };
let Value = Selector;


let IntArray = struct {
    count: Count;
    values: Value[count];
};

let Message = struct {
    value: byte[]: string { boundary: '\\n'; };
};

let T = struct {
    type: Selector;
    if (type == 1) {
        array: IntArray;
        let ?item = array;
        let ?type = 'array';
    }
    if (type == 2) {
        message: Message;
        let ?item = message;
        let ?type = 'message';
    }
};

file {
    items: T[];
}

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
    }])
def params_named_exprs_polymorphic(request):
    return conftest.make_testcase(request.param)


def test_named_exprs_polymorphic(params_named_exprs_polymorphic):
    params = params_named_exprs_polymorphic
    dtree = params['dtree']
    assert len(dtree.items) == 4
    assert dtree.items[1].message.value == 'bonjour'
    assert dtree.items[1]['?item'].value == 'bonjour'
    assert dtree.eval_expr('items[1].?item.value') == 'bonjour'
    assert dtree.items[1]['?type'] == 'message'
    assert dtree.eval_expr('items[1].?type') == 'message'
    assert dtree.eval_expr('items[1].?type == "message"') == True

    assert model.make_python_object(
        dtree.items[2].array.values) == [1, 2, 3, 4, 5]
    assert model.make_python_object(
        dtree.items[2]['?item'].values) == [1, 2, 3, 4, 5]
    assert model.make_python_object(
        dtree.eval_expr('items[2].array.values')) == [1, 2, 3, 4, 5]
    assert dtree.items[2]['?type'] == 'array'
    assert dtree.eval_expr('items[2].?type') == 'array'
    assert dtree.eval_expr('items[2].?type == "array"') == True


spec_file_named_exprs_invalid_1 = """

let u16 = byte[2]: integer { signed: false; endian: 'little'; };

let Number = struct {
    value: u16;
};

file {
    data: byte[];
    let ?something = does_not_exist;
}

"""

spec_file_named_exprs_invalid_2 = """

let u16 = byte[2]: integer { signed: false; endian: 'little'; };

let Number = struct {
    value: u16;
};

file {
    data: byte[];
    let ?something = ?does_not_exist;
}

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
