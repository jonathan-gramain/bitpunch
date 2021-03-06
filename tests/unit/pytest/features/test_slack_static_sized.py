#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

#
# Test layout of slack objects
#
# Slack objects are objects containing at least one slack array. The
# number of items in a slack array is settled with a greedy iteration,
# stopping when there is no more room for a full extra element without
# going over the maximum offset. The @minspan, @maxspan and @span
# attributes can affect how many items are allocated in slack arrays.
#

BLOCK_SIZES = [100, 30, 10, 3]

spec_const_sized = """

let Schema = struct {
    huge_blocks: [] HugeBlock;
    big_blocks: [] BigBlock;
    avg_blocks: [] AvgBlock;
    small_blocks: [] SmallBlock;
};

let HugeBlock = struct {
    // will be filled with 0x00 bytes
    contents: [100] byte;
};

let BigBlock = struct {
    // will be filled with 0x01 bytes
    contents: [30] byte;
};

let AvgBlock = struct {
    // will be filled with 0x02 bytes
    contents: [10] byte;
};

let SmallBlock = struct {
    // will be filled with 0x03 bytes
    contents: [3] byte;
};

"""

data_const_sized_1 = """
# no HugeBlock
01 01 01 01 01 01 01 01 01 01
01 01 01 01 01 01 01 01 01 01
01 01 01 01 01 01 01 01 01 01
# end of BigBlock #0
02 02 02 02 02 02 02 02 02 02
# end of AvgBlock #0
02 02 02 02 02 02 02 02 02 02
# end of AvgBlock #1
03 03 03 03 03 03 03 03 03    # 3x SmallBlock
"""

data_const_sized_2 = """
00 00 00 00 00 00 00 00 00 00
00 00 00 00 00 00 00 00 00 00
00 00 00 00 00 00 00 00 00 00
00 00 00 00 00 00 00 00 00 00
00 00 00 00 00 00 00 00 00 00
00 00 00 00 00 00 00 00 00 00
00 00 00 00 00 00 00 00 00 00
00 00 00 00 00 00 00 00 00 00
00 00 00 00 00 00 00 00 00 00
00 00 00 00 00 00 00 00 00 00
# end of HugeBlock #0
03 03 03 03 03 03             # 2x SmallBlock
"""


@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_const_sized,
        'data': data_const_sized_1,
        'block_counts': [0, 1, 2, 3]
    }, {
        'spec': spec_const_sized,
        'data': data_const_sized_2,
        'block_counts': [1, 0, 0, 2]
    },
    ])
def params_slack_const_sized(request):
    return conftest.make_testcase(request.param)


def test_slack_const_sized(params_slack_const_sized):
    params = params_slack_const_sized
    dtree, block_counts = params['dtree'], params['block_counts']

    cur_location = 0
    for (blocktype, block_array) in enumerate([dtree.huge_blocks,
                                               dtree.big_blocks,
                                               dtree.avg_blocks,
                                               dtree.small_blocks]):
        assert len(block_array) == block_counts[blocktype]
        for block in block_array:
            assert block.get_offset() == cur_location
            cur_location += BLOCK_SIZES[blocktype]
            contents = model.make_python_object(block.contents)
            assert contents == ''.join(
                [chr(blocktype)] * BLOCK_SIZES[blocktype])
