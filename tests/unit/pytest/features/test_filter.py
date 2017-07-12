#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

spec_file_simple_filter_as_type = """

type u32 byte[4]: integer(signed=false, endian=little);

struct AsContents {
    u32 n;
    byte[n] data;
};

file {
    byte[] contents: AsContents;
}

"""

spec_file_simple_filter_as_type_twice = """

type u32 byte[4]: integer(signed=false, endian=little);

struct DummyStruct {
    u32 n;
    byte[n] dummy;
};

struct AsContents {
    u32 n;
    byte[n] data;
};

file {
    byte[] contents: DummyStruct: AsContents;
}

"""

spec_file_simple_filter_as_type_anon_hop = """

type u32 byte[4]: integer(signed=false, endian=little);

struct HopStruct {
    u32 n;
    byte[n]: AsContents;
};

struct AsContents {
    byte[] data;
};

file {
    byte[] contents: HopStruct;
}

"""

data_file_simple_filter_as_type = """
10 00 00 00
"as contents data"
"""

spec_file_simple_filter_as_type_constrained = """

type u32 byte[4]: integer(signed=false, endian=little);

struct AsContents {
    u32 n;
    byte[n] data;
};

file {
    byte[30] header;
    byte[20] contents: AsContents;
    byte[30] footer;
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

type u32 byte[4]: integer(signed=false, endian=little);

struct Base64Block {
    u32 n;
    byte[n]: base64: AsContents;
};

struct AsContents {
    byte[] data;
};

file {
    byte[] contents: Base64Block;
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
