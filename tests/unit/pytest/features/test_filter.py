#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

spec_file_simple_filter_as_type = """

let u32 = byte[4]: integer { signed: false; endian: 'little'; };

let AsContents = struct {
    n: u32;
    data: byte[n];
};

file {
    contents: byte[]: AsContents;
}

"""

spec_file_simple_filter_as_type_twice = """

let u32 = byte[4]: integer { signed: false; endian: 'little'; };

let DummyStruct = struct {
    n: u32;
    dummy: byte[n];
};

let AsContents = struct {
    n: u32;
    data: byte[n];
};

file {
    contents: byte[]: DummyStruct: AsContents;
}

"""

spec_file_simple_filter_as_type_anon_hop = """

let u32 = byte[4]: integer { signed: false; endian: 'little'; };

let HopStruct = struct {
    n: u32;
    : byte[n]: AsContents;
};

let AsContents = struct {
    data: byte[];
};

file {
    contents: byte[]: HopStruct;
}

"""

data_file_simple_filter_as_type = """
10 00 00 00
"as contents data"
"""

spec_file_simple_filter_as_type_constrained = """

let u32 = byte[4]: integer { signed: false; endian: 'little'; };

let AsContents = struct {
    n: u32;
    data: byte[n];
};

file {
    header: byte[30];
    contents: byte[20]: AsContents;
    footer: byte[30];
}

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

let u32 = byte[4]: integer { signed: false; endian: 'little'; };

let Base64Block = struct {
    n: u32;
    : byte[n]: base64: AsContents;
};

let AsContents = struct {
    data: byte[];
};

file {
    contents: byte[]: Base64Block;
}

"""

spec_file_simple_filter_as_type_base64_template = """

let u32 = byte[4]: integer { signed: false; endian: 'little'; };
let Base64 = base64 {};

let Base64Block = struct {
    n: u32;
    : byte[n]: Base64: AsContents;
};

let AsContents = struct {
    data: byte[];
};

file {
    contents: byte[]: Base64Block;
}

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
def params_filter(request):
    return conftest.make_testcase(request.param)


def test_filter(params_filter):
    params = params_filter
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

let u32 = byte[4]: integer { signed: false; endian: 'little'; };

let RawBlock = struct {
    n: u32;
    : byte[n]: AsContents;
};

let Base64Block = byte[]: string { boundary: '\\n'; }: base64: RawBlock;

let AsContents = struct {
    data: byte[];
};

file {
    blocks: Base64Block[];
}

"""

spec_file_simple_filter_line_separated_base64_2 = """

let u32 = byte[4]: integer { signed: false; endian: 'little'; };

let Base64Block = byte[]: string { boundary: '\\n'; }: base64: struct {
    n: u32;
    : byte[n]: AsContents;
};

let AsContents = struct {
    data: byte[];
};

file {
    blocks: Base64Block[];
}

"""

spec_file_simple_filter_line_separated_base64_template = """

let u32 = byte[4]: integer { signed: false; endian: 'little'; };
let CustomBase64Filter = base64 {};

let Base64Block = byte[]
  : string { boundary: '\\n'; }
  : CustomBase64Filter: struct {
      n: u32;
      : byte[n]: AsContents;
  };

let AsContents = struct {
    data: byte[];
};

file {
    blocks: Base64Block[];
}

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
    #assert dtree.eval_expr('^blocks[1]').get_offset() == 29
    assert dtree.blocks[1].get_size() == 22
    assert dtree.blocks[1].n == 18
    assert str(dtree.blocks[1].data) == 'more contents data'
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
    #assert model.make_python_object(dtree.eval_expr('^blocks[2]')) == \
    #    '\x17\x00\x00\x00even more contents data'
    #assert model.make_python_object(dtree.eval_expr('^(^blocks)[2]')) == \
    #    '\x17\x00\x00\x00even more contents data'
    #assert model.make_python_object(dtree.eval_expr('^^blocks[2]')) == \
    #    'FwAAAGV2ZW4gbW9yZSBjb250ZW50cyBkYXRh'
    #assert model.make_python_object(dtree.eval_expr('^^blocks[1..][1]')) == \
    #    'FwAAAGV2ZW4gbW9yZSBjb250ZW50cyBkYXRh'
    #assert model.make_python_object(dtree.eval_expr('^^^blocks[2]')) == \
    #'FwAAAGV2ZW4gbW9yZSBjb250ZW50cyBkYXRh\n'
    #assert model.make_python_object(dtree.eval_expr('^^^^blocks[2]')) == \
    #    'FwAAAGV2ZW4gbW9yZSBjb250ZW50cyBkYXRh\n'


spec_file_filter_in_field_expression = """

let Int = integer { signed: false; endian: 'big'; };

let AsStruct = struct {
    value: byte[1]: Int;
};

file {
    a: byte[1];
    b: byte[2];

    let a_as_int = (a: Int);
    let b_as_int = (b: Int);

    let ?a_as_struct = a: AsStruct;
    let ?b_as_struct = bytes(b)[..]: byte[]: AsStruct;

    if (a_as_int == 1) {
        let ?nb_as_struct = ?a_as_struct;
    } else {
        let ?nb_as_struct = ?b_as_struct;
    }
}

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
def params_filter_3(request):
    return conftest.make_testcase(request.param)


def test_filter_3(params_filter_3):
    params = params_filter_3
    dtree = params['dtree']

    assert dtree.a_as_int == 1
    assert dtree.b_as_int == 2
    assert dtree['?nb_as_struct'].value == 1
    #with pytest.raises(AttributeError):
    #    dtree.eval_expr('^?nb_as_struct').value
    assert dtree.eval_expr('a_as_int') == 1
    #assert model.make_python_object(dtree.eval_expr('^a_as_int')) == '\x01'
    #with pytest.raises(AttributeError):
    #    dtree.eval_expr('^?a_as_struct').value


spec_file_filter_invalid_no_data_source_1 = """

let UnsignedTemplate = integer { signed: false; endian: 'little'; };

file {
    value: UnsignedTemplate;
}

"""


spec_file_filter_encoded_integer_field = """

let Int = integer { signed: false; endian: 'big'; };

let Base64Line = byte[]: string { boundary: '\\n'; }: base64 {};

let Header = struct {
    nb_messages: byte[4]: Int;

    let ?nb_messages = nb_messages;
};

let B64Header = Base64Line: Header;

let B64Message = Base64Line: struct {
    data: byte[]: string;

    let ?data = data;
    let ?data_as_split_strings = data: struct {
        first_two: byte[2];
        remain:    byte[];
    };
};

file {
    hdr:      byte[]: B64Header;
    messages: B64Message[hdr.?nb_messages];
    garbage:  byte[];

    let ?first_message_data_3_chars = messages[0].data: byte[3]: string;
}

"""

data_file_filter_encoded_integer_field = """
"AAAAAw==\n"     # nb_messages=3
"aGVsbG8=\n"     # hello
"YmVhdXRpZnVs\n" # beautiful
"d29ybGQ=\n"     " world
"some random garbage that should not be read\n"
"""


spec_file_filter_nested_base64 = """

let Int = integer { signed: false; endian: 'big'; };

let Base64Line = byte[]: string { boundary: '\\n'; }: base64 {};

let Header = struct {
    nb_messages: byte[4]: Int;

    let ?nb_messages = nb_messages;
};

let B64Header = Base64Line: Header;

let B64Message = Base64Line: struct {
    data: byte[]: string;

    let ?data = data;
    let ?data_as_split_strings = data: struct {
        first_two: byte[2];
        remain:    byte[];
    };
};

file {
    : byte[]: base64: DecodedFile;

    let ?first_message_data_3_chars = messages[0].data: byte[3]: string;
}

let DecodedFile = struct {
    hdr:      byte[]: B64Header;
    messages: B64Message[hdr.?nb_messages];
    garbage:  byte[];
};

"""

data_file_filter_nested_base64 = """
"QUFBQUF3PT0KYUdWc2JHOD0KWW1WaGRYUnBablZzCmQyOXliR1E9CnNvbWUgcm"
"FuZG9tIGdhcmJhZ2UgdGhhdCBzaG91bGQgbm90IGJlIHJlYWQK"
"""


spec_file_filter_base64_selector = """

let Int = integer { signed: false; endian: 'big'; };

let PlainLine = byte[]: string { boundary: '\\n'; };
let Base64Line = PlainLine: base64 {};

let Header = struct {
    nb_messages: byte[4]: Int;

    let ?nb_messages = nb_messages;
};

let B64Header = Base64Line: Header;

let MessagePayload = struct {
    data: byte[]: string;

    let ?data = data;
};

let Message = struct {
    is_base64: byte: Int;
    raw_data: PlainLine;
    if (is_base64 == 1) {
        let data = raw_data: base64;
    } else {
        let data = raw_data;
    }
    let ?data = data;
    let ?data_as_split_strings = data: struct {
        first_two: byte[2];
        remain:    byte[];
    };
};

file {
    hdr:      byte[]: B64Header;
    messages: Message[hdr.?nb_messages];
    garbage:  byte[];

    let ?first_message_data_3_chars = messages[0].data: byte[3]: string;
}

"""

data_file_filter_base64_selector = """
"AAAAAw==\n"     # nb_messages=3
01 "aGVsbG8=\n"     # hello
00 "beautiful\n"
01 "d29ybGQ=\n"     " world
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
    }
])
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
    assert dtree.eval_expr('messages[2].?data') == 'world'
    assert model.make_python_object(
        dtree.eval_expr('messages[2].?data_as_split_strings')) == {
            'first_two': 'wo', 'remain': 'rld' };
    assert dtree['?first_message_data_3_chars'] == 'hel'
    with pytest.raises(IndexError):
        print str(dtree.messages[3].data)



spec_file_filter_invalid_no_data_source_2 = """

let UnsignedTemplate = integer { signed: false; endian: 'little'; };

file {
    value: ?ref;

    let ?ref = UnsignedTemplate;
}

"""

spec_file_filter_invalid_bad_filter_type = """

file {
    value: 42;
}

"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_filter_invalid_no_data_source_1,
    }, {
        'spec': spec_file_filter_invalid_no_data_source_2,
    }, {
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

    #assert model.make_python_object(dtree.eval_expr('^contents')) == \
    #    '\x10\x00\x00\x00as contents data'
    #assert model.make_python_object(dtree.eval_expr('^^contents')) == \
    #    '\x10\x00\x00\x00as contents data'


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

    #assert model.make_python_object(dtree.eval_expr('^contents')) == {
    #    'n': 16,
    #    'dummy': 'as contents data'
    #}
    #assert model.make_python_object(dtree.eval_expr('^^contents')) == \
    #    '\x10\x00\x00\x00as contents data'
    #assert model.make_python_object(dtree.eval_expr('^^^contents')) == \
    #    '\x10\x00\x00\x00as contents data'

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

    #assert model.make_python_object(dtree.eval_expr('^contents')) == \
    #    '\x10\x00\x00\x00as contents data'

    #assert model.make_python_object(dtree.eval_expr('^^contents')) == \
    #    '\x10\x00\x00\x00as contents data'



spec_file_ancestor_of_u8 = """

let u8 = byte: integer { signed: false; };

file {
    contents: u8[5];
}

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

let u32 = byte[4]: integer { signed: false; };
let NullTermFixedString = byte[8]: string { boundary: '\\0'; };

file {
    contents: NullTermFixedString;
}

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

    assert dtree.contents == 'hello'
    assert dtree.contents[1:] == 'ello'
    assert model.make_python_object(
        dtree.eval_expr('contents')) == 'hello'
    assert model.make_python_object(
        dtree.eval_expr('contents[1..]')) == 'ello'
    # FIXME when len != span_size, len should be 5
    assert dtree.eval_expr('len(contents)') == 5
    assert dtree.eval_expr('len(contents[1..])') == 4
