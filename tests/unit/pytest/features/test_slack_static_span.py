#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

spec_static_span_and_slack = """

let u8 = byte: integer { signed: false; };

file {
    blocks: [] VarBlock;
}

let VarBlock = struct {
    contents: [] byte;
    $span: 10;
};

"""

data_static_span_and_slack = """
00 00 00 00 00 00 00 00 00 00
01 01 01 01 01 01 01 01 01 01
02 02 02 02 02 02 02 02 02 02
03 03 03 03 03 03 03 03 03 03
"""


spec_static_span_and_slack_sized = """

let u8 = byte: integer { signed: false; };

file {
    blocks: [] VarBlock;
}

let VarBlock = struct {
    contents: [] byte: string { boundary: '\x42'; };
    padding:  [] byte;
    $span: 10;
};

"""

spec_static_span_and_slack_sized_subblock = """

let u8 = byte: integer { signed: false; };

file {
    blocks: [] VarBlock;
}

let VarBlock = struct {
    let BlockContents = struct {
        data: [] byte: string { boundary: '\x42'; };
    };
    contents: BlockContents;
    padding:  [] byte;
    $span: 10;
};

"""

data_static_span_and_slack_sized = """
00 00 00 00 42 ff ff ff ff ff
01 01 01 01 01 01 42 ff ff ff
02 02 02 42 ff ff ff ff ff ff
42 ff ff ff ff ff ff ff ff ff
"""


spec_static_minspan_conditional = """

let u8 = byte: integer { signed: false; };

file {
    blocks: [] VarBlock;
}

let VarBlock = struct {
    length:   u8;
    contents: [length] byte;
    if (sizeof(length) + sizeof(contents) < 10) {
        padding: [] byte;
        $span: 10;
    }
};

"""

data_static_minspan_conditional = """
04 00 00 00 00 ff ff ff ff ff
06 01 01 01 01 01 01 ff ff ff
09 02 02 02 02 02 02 02 02 02
0c 03 03 03 03 03 03 03 03 03 03 03 03
"""



spec_length_in_trailer = """

let u8 = byte: integer { signed: false; };

file {
    blocks: [] VarBlock;
}

let VarBlock = struct {
    contents: [length] byte;
    padding:  [] byte;
    length:   u8;
    $span: 10;
};
"""

spec_length_in_trailer_conditional = """

let u8 = byte: integer { signed: false; };

file {
    blocks: [] VarBlock;
}

let VarBlock = struct {
    if (length <= 9) {
        contents:         [length] byte;
    } else {
        contents_bounded: [9] byte;
    }
    padding: [] byte;
    length:  u8;
    $span: 10;
};
"""

data_length_in_trailer = """
00 00 00 00 ff ff ff ff ff 04
01 01 01 01 01 01 ff ff ff 06
02 02 02 02 02 02 02 02 02 09
03 03 03 03 03 ff ff ff ff 05
"""


spec_var_length_trailer = """

let u8 = byte: integer { signed: false; };

file {
    blocks: [] VarBlock;
}

let VarBlock = struct {
    contents:       [] byte;
    padding:        [padding_length] byte;
    padding_length: u8;
    $span: 10;
};
"""

data_var_length_trailer = """
00 00 00 00 ff ff ff ff ff 05
01 01 01 01 01 01 ff ff ff 03
02 02 02 02 02 02 02 02 02 00
03 03 03 03 03 ff ff ff ff 04
"""


spec_var_length_array_trailer = """

let u8 = byte: integer { signed: false; };
let u16 = [2] byte: integer { signed: false; };

file {
    blocks: [] VarBlock;
}

let VarBlock = struct {
    contents:        [] byte;
    padding:         [n_padding_items] u16;
    n_padding_items: u8;
    $span: 10;
};
"""

data_var_length_array_trailer = """
00 00 00 00 00 ff ff ff ff 02
01 01 01 01 01 01 01 ff ff 01
02 02 02 02 02 02 02 02 02 00
03 ff ff ff ff ff ff ff ff 04
"""


spec_var_length_block_trailer = """

let u8 = byte: integer { signed: false; };
let u16 = [2] byte: integer { signed: false; };

file {
    blocks: [] VarBlock;
}

let VarBlock = struct {
    contents:        [] byte;
    padding:         TrailerBlock;
    n_padding_items: u8;
    $span: 10;

    let TrailerBlock = struct {
        padding_array:              [n_padding_items] u16;
        another_padding_byte_array: [1] byte;
        yet_another_padding_item:   u16;
    };
};
"""

data_var_length_block_trailer = """
00 00 ff ff ff ff ff ff ff 02
01 01 01 01 ff ff ff ff ff 01
02 02 02 02 02 02 ff ff ff 00
ff ff ff ff ff ff ff ff ff 03
"""


spec_var_length_subblock_trailer = """

let u8 = byte: integer { signed: false; };
let u16 = [2] byte: integer { signed: false; };

file {
    blocks: [] VarBlock;
}

let VarBlock = struct {
    contents:        [] byte;
    padding:         TrailerBlock;
    n_padding_items: u8;
    $span: 10;

    let TrailerSubBlock = struct {
        padding_array:              [n_padding_items] u16;
        another_padding_byte_array: [1] byte;
    };
    let TrailerBlock = struct {
        sub_block:                TrailerSubBlock;
        yet_another_padding_item: u16;
    };
};
"""


@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_static_span_and_slack,
        'data': data_static_span_and_slack,
        'control_size': 0,
        'lengths': [10, 10, 10, 10]
    }, {
        'spec': spec_static_span_and_slack_sized,
        'data': data_static_span_and_slack_sized,
        'control_size': 1,
        'lengths': [4, 6, 3, 0],
        'data_lengths': [4, 6, 3, 0]
    }, {
        'spec': spec_static_span_and_slack_sized_subblock,
        'data': data_static_span_and_slack_sized,
        'control_size': 0,
        'lengths': [5, 7, 4, 1],
        'data_lengths': [4, 6, 3, 0]
    }, {
        'spec': spec_static_minspan_conditional,
        'data': data_static_minspan_conditional,
        'control_size': 1,
        'lengths': [4, 6, 9, 12]
    }, {
        'spec': spec_length_in_trailer,
        'data': data_length_in_trailer,
        'control_size': 1,
        'lengths': [4, 6, 9, 5]
    }, {
        'spec': spec_length_in_trailer_conditional,
        'data': data_length_in_trailer,
        'control_size': 1,
        'lengths': [4, 6, 9, 5]
    }, {
        'spec': spec_var_length_trailer,
        'data': data_var_length_trailer,
        'control_size': 1,
        'lengths': [4, 6, 9, 5]
    }, {
        'spec': spec_var_length_array_trailer,
        'data': data_var_length_array_trailer,
        'control_size': 1,
        'lengths': [5, 7, 9, 1]
    }, {
        'spec': spec_var_length_block_trailer,
        'data': data_var_length_block_trailer,
        'control_size': 1,
        'lengths': [2, 4, 6, 0]
    }, {
        'spec': spec_var_length_subblock_trailer,
        'data': data_var_length_block_trailer,
        'control_size': 1,
        'lengths': [2, 4, 6, 0]
    }])
def params_slack_span_simple(request):
    return conftest.make_testcase(request.param)


def test_slack_span_simple(params_slack_span_simple):
    params = params_slack_span_simple
    dtree, lengths, control_size = (params['dtree'], params['lengths'],
                                    params['control_size'])
    if 'data_lengths' in params:
        data_lengths = params['data_lengths']
    else:
        data_lengths = lengths

    assert len(dtree.blocks) == len(lengths)
    for i, block in enumerate(dtree.blocks):
        contents = memoryview(block.contents)
        assert len(contents) == lengths[i]
        assert contents[:data_lengths[i]] == ''.join(
            [chr(i)] * data_lengths[i])
        assert block.get_size() >= 10
        assert block.get_size() == dtree.blocks[i:i+1].get_size()
        padding_len = max(10 - (control_size + lengths[i]), 0)
        try:
            assert (memoryview(block.padding) == '\xff' * padding_len)
        except AttributeError:
            assert padding_len == 0


spec_static_span_template = """

let u8 = byte: integer {{ signed: false; }};

file {{
    huge_blocks:  [] HugeBlock;
    big_blocks:   [] BigBlock;
    avg_blocks:   [] AvgBlock;
    small_blocks: [] SmallBlock;
}}

let VarBlock = struct {{
    length:   u8;
    contents: [length] byte;
}};

let HugeBlock = struct {{
    // will be filled with 0x00 bytes
    sub_blocks: [] VarBlock;
    {span_huge}
}};

let BigBlock = struct {{
    // will be filled with 0x01 bytes
    sub_blocks: [] VarBlock;
    {span_big}
}};

let AvgBlock = struct {{
    // will be filled with 0x02 bytes
    sub_blocks: [] VarBlock;
    {span_avg}
}};

let SmallBlock = struct {{
    // will be filled with 0x03 bytes
    sub_blocks: [] VarBlock;
    {span_small}
}};
"""


data_static_span_and_subblocks = """
# no HugeBlock
0a   01 01 01 01 01 01 01 01 01 01
09   01 01 01 01 01 01 01 01 01
08   01 01 01 01 01 01 01 01
# end of BigBlock #0
02   02 02
03   02 02 02
02   02 02
# end of AvgBlock #0
01   02
01   02
04   02 02 02 02
00
# end of AvgBlock #1
02   03 03
# end of SmallBlock #0
00
00
00
# end of SmallBlock #1
00
01   03
# end of SmallBlock #2
"""

data_static_minspan_and_subblocks = """
# no HugeBlock
0a   01 01 01 01 01 01 01 01 01 01
09   01 01 01 01 01 01 01 01 01
08   01 01 01 01 01 01 01 01
02   01 01
03   01 01 01
02   01 01
01   01
01   01
04   01 01 01 01
00
02   01 01
00
00
00
00
01   01
# end of BigBlock #0
# no AvgBlock
# no SmallBlock
"""

data_static_maxspan_and_subblocks = """
0a   00 00 00 00 00 00 00 00 00 00
09   00 00 00 00 00 00 00 00 00
08   00 00 00 00 00 00 00 00
02   00 00
03   00 00 00
02   00 00
01   00
01   00
04   00 00 00 00
00
02   00 00
00
00
00
00
01   00
# end of HugeBlock #0
# no BigBlock
# no AvgBlock
# no SmallBlock
"""

data_static_minmaxspan_and_subblocks = """
# no HugeBlock
0a   01 01 01 01 01 01 01 01 01 01
09   01 01 01 01 01 01 01 01 01
06   01 01 01 01 01 01
# end of BigBlock #0
06   01 01 01 01 01 01
03   01 01 01
03   01 01 01
01   01
01   01
01   01
00
02   01 01
00
00
00
00
01   01
# end of BigBlock #1
04 02 02 02 02
03 02 02 02
# end of AvgBlock #0
02 03 03
# end of SmallBlock #0
"""


@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_static_span_template.format(
            span_huge='$span: 100;',
            span_big='$span: 30;',
            span_avg='$span: 10;',
            span_small='$span: 3;'),
        'data': data_static_span_and_subblocks,
        'block_counts': [0, 1, 2, 3]
    }, {
        'spec': spec_static_span_template.format(
            span_huge='$minspan: 100; $maxspan: 100;',
            span_big='$minspan: 30; $maxspan: 30;',
            span_avg='$minspan: 10; $maxspan: 10;',
            span_small='$minspan: 3; $maxspan: 3;'),
        'data': data_static_span_and_subblocks,
        'block_counts': [0, 1, 2, 3]
    }, {
        'spec': spec_static_span_template.format(
            span_huge='$minspan: 100;',
            span_big='$minspan: 30;',
            span_avg='$minspan: 10;',
            span_small='$minspan: 3;'),
        'data': data_static_minspan_and_subblocks,
        'block_counts': [0, 1, 0, 0]
    }, { 
        # set $minspan to 1 in blocks, because they can potentially be
        # 0 bytes otherwise, leaving an ambiguous situation
        'spec': spec_static_span_template.format(
            span_huge='$minspan: 1; $maxspan: 100;',
            span_big='$minspan: 1; $maxspan: 30;',
            span_avg='$minspan: 1; $maxspan: 10;',
            span_small='$minspan: 1; $maxspan: 3;'),
        'data': data_static_maxspan_and_subblocks,
        'block_counts': [1, 0, 0, 0]
    }, {
        'spec': spec_static_span_template.format(
            span_huge='$minspan: 90; $maxspan: 110;',
            span_big='$minspan: 26; $maxspan: 34;',
            span_avg='$minspan: 9; $maxspan: 11;',
            span_small='$minspan: 3; $maxspan: 3;'),
        'data': data_static_minmaxspan_and_subblocks,
        'block_counts': [0, 2, 1, 1]
    }
    ])
def params_subblocks(request):
    return conftest.make_testcase(request.param)



def test_slack_static_span(params_subblocks):
    import sys
    #model.enable_debug_mode()
    params = params_subblocks
    dtree, block_counts = params['dtree'], params['block_counts']

    cur_offset = 0
    for (blocktype, block_array) in enumerate([dtree.huge_blocks,
                                               dtree.big_blocks,
                                               dtree.avg_blocks,
                                               dtree.small_blocks]):
        assert len(block_array) == block_counts[blocktype]
        for block in block_array:
            for sub_block in block.sub_blocks:
                assert sub_block.get_offset() == cur_offset
                contents = model.make_python_object(sub_block.contents)
                assert contents == ''.join(
                    [chr(blocktype)] * len(contents))
                cur_offset += sub_block.get_size()
