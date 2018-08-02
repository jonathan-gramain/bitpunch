#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

spec_file_array_static_sized = """

let u32 = [4] byte <> integer { @signed = false; @endian = 'big'; };

let Item = struct {
    value: u32;
};

file {
    integers: [10] Item;
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
    value: byte <> integer { @signed = false; };
};

file {
    integers: [10] Item;
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
    assert dtree.integers['@length'] == 10
    assert dtree.integers.get_size() == sizeof_array
    for i in range(10):
        mapped_i = (i * 7) % 10
        assert dtree.integers[mapped_i].value == mapped_i


spec_file_array_bytes_as_integers_1 = """

let u8 = byte <> integer { @signed = false; };

file {
    integers: [10] u8;
}

"""

spec_file_array_bytes_as_integers_2 = """

let u8 = byte <> integer { @signed = false; };

file {
    integers: [10] byte <> [] byte <> [10] byte <> [] u8;
}

"""

data_file_array_bytes_as_integers = """
00 01 02 03 04 05 06 07 08 09
"""


spec_file_array_bytes_as_integers_filtered = """

let u8 = byte <> integer { @signed = false; };

file {
    integers: [] byte <> base64 <> [] u8;
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
    }, {
        'spec': spec_file_array_bytes_as_integers_2,
        'data': data_file_array_bytes_as_integers,
    }, {
        'spec': spec_file_array_bytes_as_integers_filtered,
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



spec_file_array_keyed_items = """

let u32 = [4] byte <> integer { @signed = false; @endian = 'little'; };

let Item = struct {
    name: [] byte <> string { @boundary = '\\0'; };
    value: u32;
    @key = name;
};

file {
    integers: [] Item;
}

"""

data_file_array_keyed_items = """
"alpha" 00 01 00 00 00
"bravo" 00 02 00 00 00
"charlie" 00 03 00 00 00
"""

spec_file_array_keyed_filtered_keys = """

let u32 = [4] byte <> integer { @signed = false; @endian = 'little'; };

let Item = struct {
    name: [] byte <> string { @boundary = '\\0'; } <> base64 <> string;
    value: u32;
    @key = name;
};

file {
    integers: [] Item;
}

"""

data_file_array_keyed_filtered_keys = """
"YWxwaGE=" 00 01 00 00 00
"YnJhdm8=" 00 02 00 00 00
"Y2hhcmxpZQ==" 00 03 00 00 00
"""

spec_file_array_keyed_filtered_items = """

let u32 = [4] byte <> integer { @signed = false; @endian = 'little'; };

let Item = [] byte <> string { @boundary = '\\n'; } <> base64 <> struct {
    name: [] byte <> string { @boundary = '\\0'; };
    value: u32;
    @key = name;
};

file {
    integers: [] Item;
}

"""

data_file_array_keyed_filtered_items = """
"YWxwaGEAAQAAAA==\n"
"YnJhdm8AAgAAAA==\n"
"Y2hhcmxpZQADAAAA\n"
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_array_keyed_items,
        'data': data_file_array_keyed_items,
    }, {
        'spec': spec_file_array_keyed_filtered_keys,
        'data': data_file_array_keyed_filtered_keys,
    }, {
        'spec': spec_file_array_keyed_filtered_items,
        'data': data_file_array_keyed_filtered_items,
    }])
def params_array_keyed_items(request):
    return conftest.make_testcase(request.param)


def test_array_keyed_items(params_array_keyed_items):
    params = params_array_keyed_items
    dtree = params['dtree']
    assert len(dtree.integers) == 3
    assert dtree.integers['charlie'].value == 3
    assert dtree.integers['alpha'].value == 1
    assert dtree.integers['bravo'].value == 2
    assert dtree.eval_expr('integers["charlie"].value') == 3

    assert ([str(key) for key in dtree.integers.iter_keys()]
            == ['alpha', 'bravo', 'charlie'])



spec_file_array_raw_bytes = """

file {
    integers: [] byte;
}

"""

data_file_array_raw_bytes = """
00 01 02 03 04 05 06 07 08 09
"""


@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_array_raw_bytes,
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
