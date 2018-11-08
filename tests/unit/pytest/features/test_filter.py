#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

spec_file_simple_filter_as_type = """

let u32 = [4] byte <> integer { @signed = false; @endian = 'little'; };

let AsContents = struct {
    n: u32;
    data: [n] byte;
};

env("IN") <> struct {
    contents: [] byte <> AsContents;
};

"""

spec_file_simple_filter_as_type_twice = """

let u32 = [4] byte <> integer { @signed = false; @endian = 'little'; };

let DummyStruct = struct {
    n: u32;
    dummy: [n] byte;
};

let AsContents = struct {
    n: u32;
    data: [n] byte;
};

env("IN") <> struct {
    contents: [] byte <> DummyStruct <> AsContents;
};

"""

spec_file_simple_filter_as_type_anon_hop = """

let u32 = [4] byte <> integer { @signed = false; @endian = 'little'; };

let HopStruct = struct {
    n: u32;
    [n] byte <> AsContents;
};

let AsContents = struct {
    data: [] byte;
};

env("IN") <> struct {
    contents: [] byte <> HopStruct;
};

"""

data_file_simple_filter_as_type = """
10 00 00 00
"as contents data"
"""

spec_file_simple_filter_as_type_constrained = """

let u32 = [4] byte <> integer { @signed = false; @endian = 'little'; };

let AsContents = struct {
    n: u32;
    data: [n] byte;
};

env("IN") <> struct {
    header: [30] byte;
    contents: [20] byte <> AsContents;
    footer: [30] byte;
};

"""

data_file_simple_filter_as_type_constrained = """
"loooooooooooooooooooong header"
10 00 00 00
"as contents data"
"loooooooooooooooooooong footer"
"""

data_file_simple_filter_as_type_constrained_bad = """
"loooooooooooooooooooong header"
19 00 00 00
"as contents data too long"
"loooooooooooooooooooong footer"
"""

spec_file_simple_filter_as_type_base64 = """

let u32 = [4] byte <> integer { @signed = false; @endian = 'little'; };

let Base64Block = struct {
    n: u32;
    [n] byte <> base64 <> AsContents;
};

let AsContents = struct {
    data: [] byte;
};

env("IN") <> struct {
    contents: [] byte <> Base64Block;
};

"""

spec_file_simple_filter_as_type_base64_template = """

let u32 = [4] byte <> integer { @signed = false; @endian = 'little'; };
let Base64 = base64 {};

let Base64Block = struct {
    n: u32;
    [n] byte <> Base64 <> AsContents;
};

let AsContents = struct {
    data: [] byte;
};

env("IN") <> struct {
    contents: [] byte <> Base64Block;
};

"""

data_file_simple_filter_as_type_base64 = """
18 00 00 00
"YXMgY29udGVudHMgZGF0YQ=="
"""


@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_simple_filter_as_type,
        'data': data_file_simple_filter_as_type,
        'stored_content_length': 16,
    }, {
        'spec': spec_file_simple_filter_as_type_twice,
        'data': data_file_simple_filter_as_type,
        'stored_content_length': 16,
    }, {
        'spec': spec_file_simple_filter_as_type_anon_hop,
        'data': data_file_simple_filter_as_type,
        'stored_content_length': 16,
    }, {
        'spec': spec_file_simple_filter_as_type_constrained,
        'data': data_file_simple_filter_as_type_constrained,
        'stored_content_length': 16,
    }, {
        'spec': spec_file_simple_filter_as_type_base64,
        'data': data_file_simple_filter_as_type_base64,
        'stored_content_length': 24,
    }, {
        'spec': spec_file_simple_filter_as_type_base64_template,
        'data': data_file_simple_filter_as_type_base64,
        'stored_content_length': 24,
    }])
def params_filter_1(request):
    return conftest.make_testcase(request.param)


def test_filter_1(params_filter_1):
    params = params_filter_1
    dtree, stored_content_length = (params['dtree'],
                                    params['stored_content_length'])

    assert dtree.contents.n == stored_content_length
    assert len(dtree.contents.data) == 16
    assert memoryview(dtree.contents.data) == 'as contents data'
    assert model.make_python_object(dtree.contents) == {
        'n': stored_content_length,
        'data': 'as contents data'
    }
    assert dtree.contents.n == stored_content_length
    assert dtree.eval_expr('contents.n') == stored_content_length
    assert model.make_python_object(dtree.eval_expr('^contents.n')) == \
        chr(stored_content_length) + '\x00\x00\x00'

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_simple_filter_as_type_constrained,
        'data': data_file_simple_filter_as_type_constrained_bad,
    }])
def params_filter_bad(request):
    return conftest.make_testcase(request.param)


def test_filter_bad(params_filter_bad):
    params = params_filter_bad
    dtree = params['dtree']

    with pytest.raises(model.OutOfBoundsError):
        print(model.make_python_object(dtree.contents))


spec_file_simple_filter_line_separated_base64 = """

let u32 = [4] byte <> integer { @signed = false; @endian = 'little'; };

let RawBlock = struct {
    n: u32;
    [n] byte <> AsContents;
};

let Base64Block = string { @boundary = '\\n'; }
    <> base64
    <> RawBlock;

let AsContents = struct {
    data: [] byte;

    let ?data_preview = data[..10];
    let ?data_glimpse = data[10..][4..7];
};

env("IN") <> struct {
    blocks: [] Base64Block;
};

"""

spec_file_simple_filter_line_separated_base64_2 = """

let u32 = [4] byte <> integer { @signed = false; @endian = 'little'; };

let Base64Block = string { @boundary = '\\n'; }
   <> base64
   <> struct {
    n: u32;
    [n] byte <> AsContents;
};

let AsContents = struct {
    data: [] byte;

    let ?data_preview = data[..10];
    let ?data_glimpse = data[10..][4..7];
};

env("IN") <> struct {
    blocks: [] Base64Block;
};

"""

spec_file_simple_filter_line_separated_base64_template = """

let u32 = [4] byte <> integer { @signed = false; @endian = 'little'; };
let CustomBase64Filter = base64 {};

let Base64Block = string { @boundary = '\\n'; }
   <> CustomBase64Filter
   <> struct {
      n: u32;
      [n] byte <> AsContents;
  };

let AsContents = struct {
    data: [] byte;

    let ?data_preview = data[..10];
    let ?data_glimpse = data[10..][4..7];
};

env("IN") <> struct {
    blocks: [] Base64Block;
};

"""

data_file_simple_filter_line_separated_base64 = """
"EAAAAGFzIGNvbnRlbnRzIGRhdGE=\n"
"EgAAAG1vcmUgY29udGVudHMgZGF0YQ==\n"
"FwAAAGV2ZW4gbW9yZSBjb250ZW50cyBkYXRh\n"
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_simple_filter_line_separated_base64,
        'data': data_file_simple_filter_line_separated_base64,
    }, {
        'spec': spec_file_simple_filter_line_separated_base64_2,
        'data': data_file_simple_filter_line_separated_base64,
    }, {
        'spec': spec_file_simple_filter_line_separated_base64_template,
        'data': data_file_simple_filter_line_separated_base64,
    }])
def params_filter_2(request):
    return conftest.make_testcase(request.param)


def test_filter_2(params_filter_2):
    params = params_filter_2
    dtree = params['dtree']

    assert dtree.blocks[1].get_offset() == 0
    assert dtree.blocks[1].get_size() == 22
    assert dtree.eval_expr('blocks[1]').get_offset() == 0
    assert dtree.eval_expr('blocks[1]').get_size() == 22
    assert dtree.blocks[1].n == 18
    assert str(dtree.blocks[1].data) == 'more contents data'
    assert str(dtree.blocks[1]['?data_preview']) == 'more conte'
    assert str(dtree.blocks[1]['?data_glimpse']) == 'dat'
    assert str(dtree.eval_expr('blocks[1].?data_preview')) == 'more conte'
    assert str(dtree.eval_expr('blocks[1].?data_glimpse')) == 'dat'
    assert dtree.blocks[0].n == 16
    assert str(dtree.blocks[0].data) == 'as contents data'
    assert model.make_python_object(dtree.blocks[2]) == {
        'n': 23,
        'data': 'even more contents data'
    }
    assert model.make_python_object(dtree.eval_expr('blocks[2]')) == {
        'n': 23,
        'data': 'even more contents data'
    }
    assert dtree.eval_expr('blocks[1].?data_preview').get_size() == 10
    assert dtree.eval_expr('blocks[1].?data_glimpse').get_size() == 3

    # up one ancestor => input for base64 filter
    daddy = dtree.eval_expr('^blocks[1]')
    assert daddy.get_offset() == 0
    assert daddy.get_size() == 22
    # filtered output
    assert daddy == '\x12\00\00\00more contents data'

    # up two ancestors => input for newline-terminated string

    # there are only two ancestors, so up to three ancestors shall
    # return identical results than up two ancestors
    for grandpa in [dtree.eval_expr('^^blocks[1]'),
                    dtree.eval_expr('^^^blocks[1]')]:
        assert grandpa.get_offset() == 29
        assert grandpa.get_size() == 32
        # filtered output (i.e. without final newline)
        assert str(grandpa) == 'EgAAAG1vcmUgY29udGVudHMgZGF0YQ=='

    assert model.make_python_object(dtree.eval_expr('^blocks[2]')) == \
       '\x17\x00\x00\x00even more contents data'
    assert model.make_python_object(dtree.eval_expr('^(^blocks)[2]')) == \
       '\x17\x00\x00\x00even more contents data'
    assert model.make_python_object(dtree.eval_expr('^^blocks[2]')) == \
       'FwAAAGV2ZW4gbW9yZSBjb250ZW50cyBkYXRh'
    assert model.make_python_object(dtree.eval_expr('^^blocks[1..][1]')) == \
       'FwAAAGV2ZW4gbW9yZSBjb250ZW50cyBkYXRh'
    assert model.make_python_object(dtree.eval_expr('^^^blocks[2]')) == \
        'FwAAAGV2ZW4gbW9yZSBjb250ZW50cyBkYXRh'
    assert model.make_python_object(dtree.eval_expr('^^^^blocks[2]')) == \
       'FwAAAGV2ZW4gbW9yZSBjb250ZW50cyBkYXRh'


spec_file_filter_in_field_expression = """

let Int = integer { @signed = false; @endian = 'big'; };

let AsStruct = struct {
    value: [1] byte <> Int;
};

let UnusedByteStruct = struct {
    by: [1] byte;
};

env("IN") <> struct {
    a: [1] byte;
    b: [2] byte;

    let a_as_int = (a <> Int);
    let b_as_int = (b <> Int);

    let ?a_as_struct = a <> AsStruct;
    let ?b_as_struct = (b <> bytes)[..] <> [] byte <> AsStruct;

    if (a_as_int == 1) {
        let ?nb_as_struct = ?a_as_struct;
    } else {
        let ?nb_as_struct = ?b_as_struct;
    }
};

"""

data_file_filter_in_field_expression = """
01   00 02
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_filter_in_field_expression,
        'data': data_file_filter_in_field_expression,
    }
])
def params_filter_in_field_expression(request):
    return conftest.make_testcase(request.param)


def test_filter_in_field_expression(params_filter_in_field_expression):
    params = params_filter_in_field_expression
    dtree = params['dtree']

    assert dtree.a_as_int == 1
    assert dtree.b_as_int == 2
    assert dtree['?nb_as_struct'].value == 1
    with pytest.raises(AttributeError):
        dtree.eval_expr('^?nb_as_struct').value
    assert dtree.eval_expr('a_as_int') == 1
    assert dtree.eval_expr('a <> Int') == 1
    assert model.make_python_object(
        dtree.eval_expr('a <> UnusedByteStruct').by) == '\x01'
    assert model.make_python_object(dtree.eval_expr('^a_as_int')) == '\x01'
    with pytest.raises(AttributeError):
        dtree.eval_expr('^?a_as_struct').value



spec_file_filter_encoded_integer_field = """

let Int = integer { @signed = false; @endian = 'big'; };

let Base64Line = string { @boundary = '\\n'; } <> base64 {};

let Header = struct {
    nb_messages: [4] byte <> Int;

    let ?nb_messages = nb_messages;
};

let B64Header = Base64Line <> Header;

let B64Message = Base64Line <> struct {
    data: string;

    let ?data = data;
    let ?data_as_split_strings = data <> struct {
        first_two: [2] byte;
        remain:    [] byte;
    };
};

env("IN") <> struct {
    hdr:      B64Header;
    messages: [hdr.?nb_messages] B64Message;
    garbage:  [] byte;

    let ?first_message_data_3_chars = messages[0].data <> [3] byte <> string;
};

"""

data_file_filter_encoded_integer_field = """
"AAAAAw==\n"     # nb_messages=3
"aGVsbG8=\n"     # hello
"YmVhdXRpZnVs\n" # beautiful
"d29ybGQ=\n"     # world
"some random garbage that should not be read\n"
"""


spec_file_filter_nested_base64 = """

let Int = integer { @signed = false; @endian = 'big'; };

let Base64Line = string { @boundary = '\\n'; } <> base64 {};

let Header = struct {
    nb_messages: [4] byte <> Int;

    let ?nb_messages = nb_messages;
};

let B64Header = Base64Line <> Header;

let B64Message = Base64Line <> struct {
    data: string;

    let ?data = data;
    let ?data_as_split_strings = data <> struct {
        first_two: [2] byte;
        remain:    [] byte;
    };
};

env("IN") <> struct {
    base64 <> DecodedFile;

    let ?first_message_data_3_chars = messages[0].data <> [3] byte <> string;
};

let DecodedFile = struct {
    hdr:      B64Header;
    messages: [hdr.?nb_messages] B64Message;
    garbage:  [] byte;
};

"""

data_file_filter_nested_base64 = """
"QUFBQUF3PT0KYUdWc2JHOD0KWW1WaGRYUnBablZzCmQyOXliR1E9CnNvbWUgcm"
"FuZG9tIGdhcmJhZ2UgdGhhdCBzaG91bGQgbm90IGJlIHJlYWQK"
"""


spec_file_filter_base64_selector = """

let Int = integer { @signed = false; @endian = 'big'; };

let PlainLine = string { @boundary = '\\n'; };
let Base64Line = PlainLine <> base64 {};

let Header = struct {
    nb_messages: [4] byte <> Int;

    let ?nb_messages = nb_messages;
};

let B64Header = Base64Line <> Header;

let MessagePayload = struct {
    data: string;

    let ?data = data;
};

let Message = struct {
    is_base64: byte <> Int;
    raw_data: PlainLine;
    if (is_base64 == 1) {
        let data = raw_data <> base64 <> string;
    } else {
        let data = raw_data;
    }
    let ?data = data;
    let ?data_as_split_strings = data <> struct {
        first_two: [2] byte;
        remain:    [] byte;
    };
};

env("IN") <> struct {
    hdr:      B64Header;
    messages: [hdr.?nb_messages] Message;
    garbage:  [] byte;

    let ?first_message_data_3_chars = messages[0].data <> [3] byte <> string;
};

"""

data_file_filter_base64_selector = """
"AAAAAw==\n"     # nb_messages=3
01 "aGVsbG8=\n"     # hello
00 "beautiful\n"
01 "d29ybGQ=\n"     # world
"some random garbage that should not be read\n"
"""



@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_filter_encoded_integer_field,
        'data': data_file_filter_encoded_integer_field,
    }, {
        'spec': spec_file_filter_nested_base64,
        'data': data_file_filter_nested_base64,
    }, {
        'spec': spec_file_filter_base64_selector,
        'data': data_file_filter_base64_selector,
    }])
def params_filter_messages(request):
    return conftest.make_testcase(request.param)


def test_filter_messages(params_filter_messages):
    params = params_filter_messages
    dtree = params['dtree']

    assert len(dtree.messages) == 3
    assert dtree.eval_expr('file.hdr.nb_messages') == 3
    assert len(dtree.messages[0].data) == 5
    assert str(dtree.messages[0].data) == 'hello'
    assert len(dtree.messages[1].data) == 9
    assert str(dtree.messages[1].data) == 'beautiful'
    assert len(dtree.messages[2].data) == 5
    assert str(dtree.messages[2].data) == 'world'
    assert str(dtree.eval_expr('messages[2].?data')) == 'world'
    assert model.make_python_object(
        dtree.eval_expr('messages[2].?data_as_split_strings')) == {
            'first_two': 'wo', 'remain': 'rld' };
    assert str(dtree['?first_message_data_3_chars']) == 'hel'
    with pytest.raises(IndexError):
        print str(dtree.messages[3].data)



spec_file_filter_file_as_single_integer_1 = """

let UnsignedTemplate = integer { @signed = false; @endian = 'little'; };

env("IN") <> struct {
    value: UnsignedTemplate;
};

"""

spec_file_filter_file_as_single_integer_2 = """

let UnsignedTemplate = integer { @signed = false; @endian = 'little'; };

env("IN") <> struct {
    value: ?ref;

    let ?ref = UnsignedTemplate;
};

"""

data_file_filter_file_as_single_integer = """
2A 00 00 00
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_filter_file_as_single_integer_1,
        'data': data_file_filter_file_as_single_integer,
    }, {
        'spec': spec_file_filter_file_as_single_integer_2,
        'data': data_file_filter_file_as_single_integer,
    }])
def params_filter_file_as_single_integer(request):
    return conftest.make_testcase(request.param)


def test_filter_file_as_single_integer(params_filter_file_as_single_integer):
    params = params_filter_file_as_single_integer
    dtree = params['dtree']
    assert dtree.value == 42

spec_file_filter_invalid_bad_filter_type = """

env("IN") <> struct {
    value: 42;
};

"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_filter_invalid_bad_filter_type,
    }])
def params_filter_invalid(request):
    return request.param


def test_filter_invalid(params_filter_invalid):
    params = params_filter_invalid
    spec = params['spec']
    with pytest.raises(OSError):
        dtree = model.DataTree('', spec)


@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_simple_filter_as_type,
        'data': data_file_simple_filter_as_type,
    }])
def params_ancestor_operator__as_type(request):
    return conftest.make_testcase(request.param)


def test_ancestor_operator__as_type(params_ancestor_operator__as_type):
    params = params_ancestor_operator__as_type
    dtree = params['dtree']

    assert model.make_python_object(dtree.eval_expr('^contents')) == \
        '\x10\x00\x00\x00as contents data'
    assert model.make_python_object(dtree.eval_expr('^^contents')) == \
        '\x10\x00\x00\x00as contents data'


@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_simple_filter_as_type_twice,
        'data': data_file_simple_filter_as_type,
    }])
def params_ancestor_operator__as_type_twice(request):
    return conftest.make_testcase(request.param)


def test_ancestor_operator__as_type_twice(params_ancestor_operator__as_type_twice):
    params = params_ancestor_operator__as_type_twice
    dtree = params['dtree']

    assert model.make_python_object(dtree.eval_expr('^contents')) == {
        'n': 16,
        'dummy': 'as contents data'
    }
    assert model.make_python_object(dtree.eval_expr('^^contents')) == \
        '\x10\x00\x00\x00as contents data'
    assert model.make_python_object(dtree.eval_expr('^^^contents')) == \
        '\x10\x00\x00\x00as contents data'

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_simple_filter_as_type_anon_hop,
        'data': data_file_simple_filter_as_type,
    }])
def params_ancestor_operator__as_type_anon_hop(request):
    return conftest.make_testcase(request.param)


def test_ancestor_operator__as_type_anon_hop(params_ancestor_operator__as_type_anon_hop):
    params = params_ancestor_operator__as_type_anon_hop
    dtree = params['dtree']

    assert model.make_python_object(dtree.eval_expr('^contents')) == \
        '\x10\x00\x00\x00as contents data'

    assert model.make_python_object(dtree.eval_expr('^^contents')) == \
        '\x10\x00\x00\x00as contents data'



spec_file_ancestor_of_u8 = """

let u8 = byte <> integer { @signed = false; };

env("IN") <> struct {
    contents: [5] u8;
};

"""

data_file_ancestor_of_u8 = """
01 02 03 04 05
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_ancestor_of_u8,
        'data': data_file_ancestor_of_u8,
    }])
def params_ancestor_of_u8(request):
    return conftest.make_testcase(request.param)


def test_ancestor_of_u8(params_ancestor_of_u8):
    params = params_ancestor_of_u8
    dtree = params['dtree']

    assert model.make_python_object(dtree.eval_expr('contents')) == \
        [1, 2, 3, 4, 5]
    # contents itself has no attached filter, so result is unchanged
    assert model.make_python_object(dtree.eval_expr('^contents')) == \
        [1, 2, 3, 4, 5]

    assert model.make_python_object(dtree.eval_expr('contents[3]')) == 4
    assert model.make_python_object(dtree.eval_expr('^contents[3]')) == '\x04'

spec_file_non_slack_array_filtered = """

let NullTermFixedString = [8] byte <> string { @boundary = '\\0'; };

env("IN") <> struct {
    contents: NullTermFixedString;
};

"""

data_file_non_slack_array_filtered = """
"hello" 00 00 00
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_non_slack_array_filtered,
        'data': data_file_non_slack_array_filtered,
    }])
def params_non_slack_array_filtered(request):
    return conftest.make_testcase(request.param)


def test_non_slack_array_filtered(params_non_slack_array_filtered):
    params = params_non_slack_array_filtered
    dtree = params['dtree']

    assert str(dtree) == 'hello\0\0\0'
    assert str(dtree.contents) == 'hello'
    assert str(dtree.contents[1:]) == 'ello'
    assert model.make_python_object(
        dtree.eval_expr('contents')) == 'hello'
    assert model.make_python_object(
        dtree.eval_expr('contents[1..]')) == 'ello'
    assert dtree.eval_expr('len(contents)') == 5
    assert len(dtree.eval_expr('contents')) == 5
    assert dtree.eval_expr('len(^contents)') == 8
    assert len(dtree.eval_expr('^contents')) == 8
    assert dtree.eval_expr('len(contents[1..])') == 4
    assert len(dtree.eval_expr('contents[1..]')) == 4
    assert dtree.eval_expr('sizeof(contents)') == 8
    assert dtree.eval_expr('sizeof(^contents)') == 8
    assert dtree.eval_expr('sizeof((^contents)[1..])') == 7



spec_file_nested_filter_defining_size = """

let Base64 = base64 {};
let Base64Line = string { @boundary = '\\n'; } <> Base64;

let Item = Base64Line <> string { @boundary = ' '; };

env("IN") <> struct {
    contents: [] Item;
};

"""

data_file_nested_filter_defining_size = """
"aG9wIFhYWFhY\n"
"eW9sbyBYWFhYWFhYWFhY\n"
"enVsdSBYWFg="
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_nested_filter_defining_size,
        'data': data_file_nested_filter_defining_size,
    }])
def params_nested_filter_defining_size(request):
    return conftest.make_testcase(request.param)


def test_nested_filter_defining_size(params_nested_filter_defining_size):
    params = params_nested_filter_defining_size
    dtree = params['dtree']

    assert model.make_python_object(dtree.contents) == \
        ['hop', 'yolo', 'zulu']



spec_file_dynamic_filter_param_integer = """

let u8 = byte <> integer { @signed = false; };

env("IN") <> struct {
    little_endian: u8;
    if (little_endian == 1) {
        let ?endian = 'little';
    } else if (little_endian == 0) {
        let ?endian = 'big';
    }

    let Integer = integer { @endian = ?endian; @signed = false; };
    let u16 = [2] byte <> Integer;

    values: [5] u16;
};

"""

data_file_dynamic_filter_param_integer_1 = """
# little endian
00
# values
00 01 00 02 00 03 00 04 00 05
"""

data_file_dynamic_filter_param_integer_2 = """
# big endian
01
# values
01 00 02 00 03 00 04 00 05 00
"""

data_file_dynamic_filter_param_integer_error_1 = """
# unknown endian
02
# values
01 00 02 00 03 00 04 00 05 00
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_dynamic_filter_param_integer,
        'data': data_file_dynamic_filter_param_integer_1,
        'error': False,
    }, {
        'spec': spec_file_dynamic_filter_param_integer,
        'data': data_file_dynamic_filter_param_integer_2,
        'error': False,
    }, {
        'spec': spec_file_dynamic_filter_param_integer,
        'data': data_file_dynamic_filter_param_integer_error_1,
        'error': True,
    }])
def params_dynamic_filter_param_integer(request):
    return conftest.make_testcase(request.param)


def test_dynamic_filter_param_integer(params_dynamic_filter_param_integer):
    params = params_dynamic_filter_param_integer
    dtree, error = params['dtree'], params['error']

    if error:
        with pytest.raises(model.DataError):
            print dtree.values[0]
    else:
        assert model.make_python_object(dtree.values) == [1, 2, 3, 4, 5]
