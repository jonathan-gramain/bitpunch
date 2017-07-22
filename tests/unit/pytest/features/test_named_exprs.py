#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

spec_file_named_exprs_1 = """

type u16 = byte[2]: integer(signed=false, endian=little);

struct Number {
    byte[2] raw;
    let ?value = raw: integer(signed=false, endian=little);
};

file {
    u16[3] typed;
    Number[3] untyped;
}

"""

data_file_named_exprs_1 = """
01 00 02 00 03 00
01 00 02 00 03 00
"""


@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_named_exprs_1,
        'data': data_file_named_exprs_1,
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
        assert (dtree.eval_expr('& untyped[{0}].?value'.format(i))
                == 6 + 2 * i)


spec_file_named_exprs_2_1 = """

type u16 = byte[2]: integer(signed=false, endian=little);

file {
    u16[3] typed;
    byte[6] untyped;
    let ?untyped_as_typed = untyped: u16[];
}

"""

spec_file_named_exprs_2_2 = """

type u16 = byte[2]: integer(signed=false, endian=little);

struct Foo {
    byte[] bar;
};

file {
    u16[3] typed;
    byte[6] untyped;
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

type u16 = byte[2]: integer(signed=false, endian=little);

struct Number {
    u16 value;
};

file {
    u16[3] typed;
    Number[3] numbers;
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

type u16 = byte[2]: integer(signed=false, endian=little);

struct Number {
    u16 value;
};

file {
    byte[] data;
    let ?first_and_second_numbers = data[sizeof(Number)..]: Number[];
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
    numbers = dtree['?first_and_second_numbers']
    assert numbers[0].value == 2
    assert numbers[1].value == 3


spec_file_named_exprs_invalid_1 = """

type u16 = byte[2]: integer(signed=false, endian=little);

struct Number {
    u16 value;
};

file {
    byte[] data;
    let ?something = does_not_exist;
}

"""

spec_file_named_exprs_invalid_2 = """

type u16 = byte[2]: integer(signed=false, endian=little);

struct Number {
    u16 value;
};

file {
    byte[] data;
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
