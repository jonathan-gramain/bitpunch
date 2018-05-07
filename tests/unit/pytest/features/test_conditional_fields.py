#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

spec_file_conditional_fields = """

let u8 = byte: integer { signed: false; };
let u32 = [4] byte: integer { signed: false; endian: 'big'; };

let IntegerItem = struct {
    value: u32;
};

let StringItem = struct {
    value: [] byte: string { boundary: '...'; };
};

let RawItem = struct {
    size: u32;
    value: [size] byte;
};

let Item = struct {
    item_type: [8] byte: string { boundary: ' '; };
    if (item_type == 'integer') {
        item: IntegerItem;
    } else if (item_type == 'string') {
        item: StringItem;
    } else if (item_type == 'raw') {
        item: RawItem;
    }
};

file {
    items: [] Item;
}

"""

data_file_conditional_fields_1 = """
"integer "
00 00 00 01
"string  "
"aloha!..."
"string  "
"mahalo!..."
"integer "
00 00 00 02
"raw     "
00 00 00 05
"abcde"
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_conditional_fields,
        'data': data_file_conditional_fields_1,
    }])
def params_conditional_fields(request):
    return conftest.make_testcase(request.param)


def test_conditional_fields(params_conditional_fields):
    params = params_conditional_fields
    dtree = params['dtree']

    expected_values = [1, 'aloha!', 'mahalo!', 2, 'abcde']
    assert len(dtree.items) == len(expected_values)
    for i in range(len(expected_values)):
        item = dtree.items[i].item
        assert model.make_python_object(item.value) == expected_values[i]
