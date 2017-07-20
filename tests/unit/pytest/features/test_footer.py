#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

spec_file_footer_static_sized = """

file {
    byte[] contents;
    byte[6] footer;
}

"""

data_file_footer_static_sized = """
"some random data"
"footer"
"""


spec_file_footer_dynamic_sized_len_at_start = """

type u8 = byte: integer(signed=false);

file {
    u8 footer_size;
    byte[] contents;
    byte[footer_size] footer;
}

"""

data_file_footer_dynamic_sized_len_at_start = """
06
"some random data"
"footer"
"""


spec_file_footer_dynamic_sized_len_in_footer = """

type u8 = byte: integer(signed=false);

file {
    byte[] contents;
    byte[footer_size] footer;
    u8 footer_size;
}

"""

data_file_footer_dynamic_sized_len_in_footer = """
"some random data"
"footer"
06
"""


@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_footer_static_sized,
        'data': data_file_footer_static_sized,
    }, {
        'spec': spec_file_footer_dynamic_sized_len_at_start,
        'data': data_file_footer_dynamic_sized_len_at_start,
    }, {
        'spec': spec_file_footer_dynamic_sized_len_in_footer,
        'data': data_file_footer_dynamic_sized_len_in_footer,
    }])
def params_footer(request):
    return conftest.make_testcase(request.param)


def test_footer(params_footer):
    params = params_footer
    dtree = params['dtree']
    assert len(dtree.footer) == 6

    assert memoryview(dtree.footer) == 'footer'
    assert memoryview(dtree.contents) == 'some random data'



spec_file_footer_dynamic_sized_complex_footer = """

type u8 = byte: integer(signed=false);
type u16 = byte[2]: integer(signed=false, endian=big);

struct Footer {
    byte[footer_hdr_size] hdr;
    byte[] footer_contents;
    u16 footer_size;
    u8 footer_hdr_size;
    span footer_size;
};

struct Contents {
    byte[] contents;
    Footer footer;
};

file {
    Contents root;
}

"""

data_file_footer_dynamic_sized_complex_footer = """
"some random data"
"fthdr"
"footer"
00 0e
05
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_footer_dynamic_sized_complex_footer,
        'data': data_file_footer_dynamic_sized_complex_footer,
    }])
def params_footer_complex(request):
    return conftest.make_testcase(request.param)


def test_footer_complex(params_footer_complex):
    params = params_footer_complex
    dtree = params['dtree']
    assert dtree.root.footer.get_size() == 14

    assert dtree.root.footer.get_size() == dtree.root.footer.footer_size
    assert dtree.root.footer.footer_contents.get_size() == 6
    assert memoryview(dtree.root.footer.footer_contents) == 'footer'
    assert memoryview(dtree.root.contents) == 'some random data'

    assert dtree.root.get_size() == 30



spec_file_footer_with_implicit_padding = """

type u8 = byte: integer(signed=false);
type u16 = byte[2]: integer(signed=false, endian=big);
type u32 = byte[4]: integer(signed=false, endian=big);

struct Footer {
    byte[footer_hdr_size] hdr;
    byte[] footer_contents;
    u16 footer_size;
    u8 footer_hdr_size;
    span footer_size;
};

struct Item {
    u32[3] values;
};

struct Contents {
    Item[] items;
    Footer footer;
};

file {
    Contents root;
}

"""

data_file_footer_with_implicit_padding = """
00 00 00 01 00 00 00 02 00 00 00 03 # root.items[0].values
"padding"
"fthdr"
"footer"
00 0e
05
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_footer_with_implicit_padding,
        'data': data_file_footer_with_implicit_padding,
    }])
def params_footer_with_implicit_padding(request):
    return conftest.make_testcase(request.param)


def test_footer_with_implicit_padding(params_footer_with_implicit_padding):
    params = params_footer_with_implicit_padding
    dtree = params['dtree']
    assert dtree.root.footer.get_size() == 14

    assert dtree.root.footer.get_size() == dtree.root.footer.footer_size
    assert dtree.root['items'].get_size() == 12
    assert dtree.root.get_size() == 33
