#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

#
# Variable length integers
#

spec_varint = """
file {
    byte[] value: varint();
    byte[] trailer;
}
"""

tcases_varint = [{ 'spec': spec_varint,
                   'data': '42 ff ff', 'value': 0x42 },
                 { 'spec': spec_varint,
                   'data': '84 33 ff ff', 'value': (0x33 << 7) + 0x04 },
                 { 'spec': spec_varint,
                   'data': 'ac ce ef 91 4b ff ff',
                   'value': (((((((0x4b << 7) + 0x11) << 7)
                                + 0x6f) << 7) + 0x4e) << 7) + 0x2c }]

@pytest.fixture(
    scope='module',
    params=tcases_varint)
def params_varint(request):
    return conftest.make_testcase(request.param)

def test_varint(params_varint):
    params = params_varint
    dtree, value = params['dtree'], params['value']

    assert dtree.value == value


#
# Test that a series of varints with varying lengths are parsed correctly
#

spec_varint_pools = """
type u8 byte[1]: integer(signed=false);
type v8 byte[1]: varint();
type v16 byte[2]: varint();
type v32 byte[4]: varint();
type v64 byte[8]: varint();
type vx byte[]: varint();

file {
    u8 v8_count;
    v8[v8_count] v8_array;
    u8 v16_count;
    v16[v16_count] v16_array;
    u8 v32_count;
    v32[v32_count] v32_array;
    u8 v64_count;
    v64[v64_count] v64_array;
    u8 vx_count;
    vx[vx_count] vx_array;
}
"""

data_varint_pools_1 = """
# v8
05
00 01 02 03 04
# v16
05
80 00 81 00 82 00 83 00 84 00
# v32
05
80 80 80 00 81 80 80 00 82 80 80 00 83 80 80 00 84 80 80 00
# v64
00
# vx
00
"""

data_varint_pools_2 = """
# v8
00
# v16
05
00 42 01 42 82 00 03 42 84 00
# v32
05
80 00 42 42 81 80 00 42 02 42 42 42 83 80 80 00 84 80 00 42
# v64
01
80 80 80 80 80 80 00 42
# vx
10
00 01 02 83 00 84 80 00 85 80 80 00 86 80 80 80 00 87 80 80 80 80 80 00
88 80 80 80 80 80 80 00 89 80 80 80 80 80 00 8a 80 80 80 80 00
8b 80 80 80 00 8c 80 80 00 8d 80 00 8e 00 0f
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_varint_pools,
        'data': data_varint_pools_1,
        'counts': [5, 5, 5, 0, 0]
    }, {
        'spec': spec_varint_pools,
        'data': data_varint_pools_2,
        'counts': [0, 5, 5, 1, 16]
    }])
def params_varint_pools(request):
    return conftest.make_testcase(request.param)

def test_varint_pools(params_varint_pools):
    params = params_varint_pools
    dtree, counts = params['dtree'], params['counts']

    for tcase, array in enumerate([dtree.v8_array, dtree.v16_array,
                                   dtree.v32_array, dtree.v64_array,
                                   dtree.vx_array]):
        assert len(array) == counts[tcase]
        for i, n in enumerate(array):
            assert n == i


spec_varint_in_blocks = """
type u8 byte: integer(signed=false);
type u16 byte[2]: integer(signed=false, endian=little);
type u32 byte[4]: integer(signed=false, endian=little);

type VarInt byte[]: varint();

file {
    Block[] blocks;
}

struct Block {
    VarInt length;
    byte[length] data;
};
"""

data_varint_in_blocks_1 = """
# block #0:
8a 00 # size of block #0
"weeeeeeeez"
# block #1:
8a 80 00 # size of block #1
"wiiiiiiiiz"
"""


@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_varint_in_blocks,
        'data': data_varint_in_blocks_1,
    }])
def params_varint_in_blocks(request):
    return conftest.make_testcase(request.param)

def test_varint_in_blocks(params_varint_in_blocks):
    params = params_varint_in_blocks
    dtree = params['dtree']

    assert len(dtree.blocks) == 2
    block0 = dtree.blocks[0]
    assert model.make_python_object(block0.data) == 'weeeeeeeez'
    block1 = dtree.blocks[1]
    assert model.make_python_object(block1.data) == 'wiiiiiiiiz'
