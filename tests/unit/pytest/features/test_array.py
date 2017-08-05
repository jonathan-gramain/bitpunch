#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

spec_file_array_static_sized = """

let u32 = byte[4]: integer { signed: false; endian: 'big'; };

let Item = struct {
    value: u32;
};

file {
    integers: Item[10];
}

"""

data_file_array_static_sized = """
00 00 00 00
00 00 00 01
00 00 00 02
00 00 00 03
00 00 00 04
00 00 00 05
00 00 00 06
00 00 00 07
00 00 00 08
00 00 00 09
"""


spec_file_array_byte_items = """

let Item = struct {
    value: byte: integer { signed: false; };
};

file {
    integers: Item[10];
}

"""

data_file_array_byte_items = """
00 01 02 03 04 05 06 07 08 09
"""


@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_array_static_sized,
        'data': data_file_array_static_sized,
        'sizeof_array': 40,
    }, {
        'spec': spec_file_array_byte_items,
        'data': data_file_array_byte_items,
        'sizeof_array': 10,
    }])
def params_array_items(request):
    return conftest.make_testcase(request.param)


def test_array_items(params_array_items):
    params = params_array_items
    dtree, sizeof_array = params['dtree'], params['sizeof_array']
    assert len(dtree.integers) == 10
    assert dtree.integers.get_size() == sizeof_array
    for i in range(10):
        mapped_i = (i * 7) % 10
        assert dtree.integers[mapped_i].value == mapped_i


spec_file_array_bytes_as_integers_1 = """

let u8 = byte: integer { signed: false; };

file {
    integers: u8[10];
}

"""

spec_file_array_bytes_as_integers_2 = """

let u8 = byte: integer { signed: false; };

file {
    integers: byte[10]: byte[]: byte[10]: u8[];
}

"""

data_file_array_bytes_as_integers = """
00 01 02 03 04 05 06 07 08 09
"""


spec_file_array_bytes_as_integers_filtered = """

let u8 = byte: integer { signed: false; };

file {
    integers: byte[]: base64: u8[];
}

"""

data_file_array_bytes_as_integers_filtered = """
"AAECAwQFBgcICQ=="
"""


@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_array_bytes_as_integers_1,
        'data': data_file_array_bytes_as_integers,
        'sizeof_array': 10,
    }, {
        'spec': spec_file_array_bytes_as_integers_2,
        'data': data_file_array_bytes_as_integers,
        'sizeof_array': 10,
    }, {
        'spec': spec_file_array_bytes_as_integers_filtered,
        'data': data_file_array_bytes_as_integers_filtered,
        'sizeof_array': 16,
    }])
def params_array_flat(request):
    return conftest.make_testcase(request.param)


def test_array_flat(params_array_flat):
    params = params_array_flat
    dtree, sizeof_array = params['dtree'], params['sizeof_array']
    assert len(dtree.integers) == 10
    assert dtree.integers.get_size() == sizeof_array
    for i in range(10):
        mapped_i = (i * 7) % 10
        assert dtree.integers[mapped_i] == mapped_i
