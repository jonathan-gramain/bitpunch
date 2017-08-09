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
        'spec': spec_file_simple_filter_as_type_constrained,
        'data': data_file_simple_filter_as_type_constrained_bad,
        'stored_content_length': 16,
        'is_bad': True,
    }, {
        'spec': spec_file_simple_filter_as_type_base64,
        'data': data_file_simple_filter_as_type_base64,
        'stored_content_length': 24,
    }])
def params_filter(request):
    return conftest.make_testcase(request.param)


def test_filter(params_filter):
    params = params_filter
    dtree, stored_content_length = (params['dtree'],
                                    params['stored_content_length'])
    if 'is_bad' in params:
        with pytest.raises(model.OutOfBoundsError):
            print(model.make_python_object(dtree.contents))
    else:
        assert dtree.contents.n == stored_content_length
        assert len(dtree.contents.data) == 16
        assert memoryview(dtree.contents.data) == 'as contents data'
        assert model.make_python_object(dtree.contents) == {
            'n': stored_content_length,
            'data': 'as contents data'
        }


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
    }])
def params_filter_2(request):
    return conftest.make_testcase(request.param)


def test_filter_2(params_filter_2):
    params = params_filter_2
    dtree = params['dtree']

    assert dtree.blocks[1].get_offset() == 29
    assert dtree.blocks[1].get_size() == 33
    assert dtree.blocks[1].n == 18
    assert str(dtree.blocks[1].data) == 'more contents data'
    assert dtree.blocks[0].n == 16
    assert str(dtree.blocks[0].data) == 'as contents data'
    assert model.make_python_object(dtree.blocks[2]) == {
        'n': 23,
        'data': 'even more contents data'
    }


spec_file_filter_in_field_expression = """

let Int = integer { signed: false; endian: 'big'; };

file {
    a: byte;
    b: byte[2];

    let a_as_int = (a: Int);
    let b_as_int = (b: Int);
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


spec_file_filter_invalid_no_data_source_1 = """

let UnsignedTemplate = integer { signed: false; endian: 'little'; };

file {
    value: UnsignedTemplate;
}

"""

spec_file_filter_invalid_no_data_source_2 = """

let UnsignedTemplate = integer { signed: false; endian: 'little'; };

file {
    value: ?ref;

    let ?ref = UnsignedTemplate;
}

"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_filter_invalid_no_data_source_1,
    }, {
        'spec': spec_file_filter_invalid_no_data_source_2,
    }])
def params_filter_invalid(request):
    return request.param


def test_filter_invalid(params_filter_invalid):
    params = params_filter_invalid
    spec = params['spec']
    with pytest.raises(OSError):
        dtree = model.DataTree('', spec)
