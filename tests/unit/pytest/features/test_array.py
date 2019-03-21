#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

spec_file_array_const_sized = """

let u32 = [4] byte <> integer { @signed: false; @endian: 'big'; };

let Item = struct {
    value: u32;
};

let Schema = struct {
    integers: [10] Item;
    garbage: [] Item;
};

"""

data_file_array_const_sized = """
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
ff ff ff ff
ff ff ff ff
"""


spec_file_array_byte_items = """

let Item = struct {
    value: byte <> integer { @signed: false; };
};

let Schema = struct {
    integers: [10] Item;
    garbage: [] Item;
};

"""

spec_file_array_byte_items_with_last = """

let Item = struct {
    value: byte <> integer { @signed: false; };
    if (value == 9) {
        @last: true;
    }
};

let Schema = struct {
    integers: [] Item;
    garbage: [] Item;
};

"""

data_file_array_byte_items = """
00 01 02 03 04 05 06 07 08 09 ff ff ff ff ff
"""


@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_array_const_sized,
        'data': data_file_array_const_sized,
        'sizeof_array': 40,
    }, {
        'spec': spec_file_array_byte_items,
        'data': data_file_array_byte_items,
        'sizeof_array': 10,
    }, {
        'spec': spec_file_array_byte_items_with_last,
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

    assert dtree.integers[4:8][1].value == 5
    assert dtree.integers[:][4:8][1].value == 5
    assert dtree.integers[2:][2:6][1].value == 5
    assert dtree.integers[2:][:6][1].value == 3
    assert model.make_python_object(dtree.integers[5:9]) == [
        { 'value': 5 }, { 'value': 6 }, { 'value': 7 }, { 'value': 8 }]
    assert model.make_python_object(dtree.integers[:][5:9]) == [
        { 'value': 5 }, { 'value': 6 }, { 'value': 7 }, { 'value': 8 }]
    assert model.make_python_object(dtree.integers[:9][5:]) == [
        { 'value': 5 }, { 'value': 6 }, { 'value': 7 }, { 'value': 8 }]


spec_file_array_bytes_as_integers_1 = """

let u8 = byte <> integer { @signed: false; };

let Schema = struct {
    integers: [10] u8;
};

"""

spec_file_array_bytes_as_integers_2 = """

let u8 = byte <> integer { @signed: false; };

let Schema = struct {
    integers: [10] byte <> [] byte <> [10] byte <> [] u8;
};

"""

spec_file_array_bytes_as_integers_explicit = """

let u8 = byte <> integer { @signed: false; };

let Schema = struct {
    integers: array {
        @item: u8;
        @length: 10;
    };
};

"""

data_file_array_bytes_as_integers = """
00 01 02 03 04 05 06 07 08 09
"""


spec_file_array_bytes_as_integers_filtered_1 = """

let u8 = byte <> integer { @signed: false; };

let Schema = struct {
    integers: base64 <> [] u8;
};

"""

spec_file_array_bytes_as_integers_filtered_2 = """

let u8 = byte <> integer { @signed: false; };

let Schema = struct {
    integers: [] byte <> base64 <> [] u8;
};

"""

data_file_array_bytes_as_integers_filtered = """
"AAECAwQFBgcICQ=="
"""


@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_array_bytes_as_integers_1,
        'data': data_file_array_bytes_as_integers,
    }, {
        'spec': spec_file_array_bytes_as_integers_2,
        'data': data_file_array_bytes_as_integers,
    }, {
        'spec': spec_file_array_bytes_as_integers_explicit,
        'data': data_file_array_bytes_as_integers,
    }, {
        'spec': spec_file_array_bytes_as_integers_filtered_1,
        'data': data_file_array_bytes_as_integers_filtered,
    }, {
        'spec': spec_file_array_bytes_as_integers_filtered_2,
        'data': data_file_array_bytes_as_integers_filtered,
    }])
def params_array_flat(request):
    return conftest.make_testcase(request.param)


def test_array_flat(params_array_flat):
    params = params_array_flat
    dtree = params['dtree']
    assert len(dtree.integers) == 10
    assert dtree.integers.get_size() == 10
    for i in range(10):
        mapped_i = (i * 7) % 10
        assert dtree.integers[mapped_i] == mapped_i

    assert dtree.integers[4:8][1] == 5
    assert dtree.integers[:][4:8][1] == 5
    assert dtree.integers[2:][2:6][1] == 5
    assert dtree.integers[2:][:6][1] == 3
    assert model.make_python_object(dtree.integers[5:9]) == [5, 6, 7, 8]
    assert model.make_python_object(dtree.integers[:][5:9]) == [5, 6, 7, 8]
    assert model.make_python_object(dtree.integers[:9][5:]) == [5, 6, 7, 8]



spec_file_array_raw_bytes_1 = """

let Schema = struct {
    integers: [] byte;

    let ?integers = integers <> [] byte;
};

"""

spec_file_array_raw_bytes_2 = """

let Schema = struct {
    integers: [10] byte;

    let ?integers = integers <> [10] byte;
};

"""

data_file_array_raw_bytes = """
00 01 02 03 04 05 06 07 08 09
"""


@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_array_raw_bytes_1,
        'data': data_file_array_raw_bytes,
    }, {
        'spec': spec_file_array_raw_bytes_2,
        'data': data_file_array_raw_bytes,
    }])
def params_array_raw_bytes(request):
    return conftest.make_testcase(request.param)


def test_array_raw_bytes(params_array_raw_bytes):
    params = params_array_raw_bytes
    dtree = params['dtree']
    assert len(dtree.integers) == 10
    assert dtree.integers.get_size() == 10
    for i in range(10):
        mapped_i = (i * 7) % 10
        assert model.make_python_object(dtree.integers[mapped_i]) \
            == chr(mapped_i)
    assert dtree.eval_expr('?integers[5..]') == '\x05\x06\x07\x08\x09'




spec_file_array_two_dimensional_items_with_last = """

let u8 = byte <> integer { @signed: false; };

let Item = struct {
    value: u8;
    if (value == 0xff) {
        @last: true;
    }
};

let Table = struct {
    integers: [] Row;

    let Row = struct {
        values: [] Item;
        if (values[0].value == 0xff) {
            @last: true;
        }
    };
};

let Schema = struct {
    table: Table;
    garbage: [] byte;
};

"""

data_file_array_two_dimensional_items_with_last = """
00 ff
01 02 ff
03 04 05 06 07 ff
ff
42 42 42 42 42
"""

spec_file_array_two_dimensional_items_with_last_2 = """

let u8 = byte <> integer { @signed: false; };

let Item = struct {
    value: u8;
};

let Table = struct {
    integers: [] Row;

    let Row = struct {
        marker: u8;
        if (marker != 255) {
            nb_values: u8;
            values: [nb_values] Item;
        } else {
            @last: true;
        }
    };
};

let Schema = struct {
    table: Table;
    garbage: [] byte;
};

"""


data_file_array_two_dimensional_items_with_last_2 = """
42 01 00
42 02 01 02
42 05 03 04 05 06 07
ff
42 42 42 42 42
"""
@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_array_two_dimensional_items_with_last,
        'data': data_file_array_two_dimensional_items_with_last,
        'table_size': 12,
        'row1_size': 3,
    }, {
        'spec': spec_file_array_two_dimensional_items_with_last_2,
        'data': data_file_array_two_dimensional_items_with_last_2,
        'table_size': 15,
        'row1_size': 4,
    }])
def params_array_two_dimensional_items(request):
    return conftest.make_testcase(request.param)


def test_array_two_dimensional_items(params_array_two_dimensional_items):
    params = params_array_two_dimensional_items
    dtree, table_size, row1_size = \
        params['dtree'], params['table_size'], params['row1_size']
    assert len(dtree.table.integers) == 4
    assert dtree.table.get_size() == table_size
    assert dtree.eval_expr('sizeof(table)') == table_size
    assert dtree.table.integers.get_size() == table_size
    assert dtree.eval_expr('sizeof(table.integers)') == table_size
    assert dtree.table.integers[1].get_size() == row1_size
    assert dtree.eval_expr('sizeof(table.integers[1])') == row1_size
    assert dtree.table.integers[2].values[3].value == 6
    assert dtree.eval_expr('table.integers[2].values[3].value') == 6
