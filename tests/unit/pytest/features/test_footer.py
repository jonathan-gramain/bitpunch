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

type u8 byte: integer(signed=false);

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

type u8 byte: integer(signed=false);

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

type u8 byte: integer(signed=false);
type u16 byte[2]: integer(signed=false, endian=big);

struct Footer {
    byte[footer_hdr_size] hdr;
    byte[] footer_contents;
    u16 footer_size;
    u8 footer_hdr_size;
    span footer_size;
};

file {
    byte[] contents;
    Footer footer;
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
    assert model.get_size(dtree.footer) == 14

    assert model.get_size(dtree.footer) == dtree.footer.footer_size
    assert model.get_size(dtree.footer.footer_contents) == 6
    assert memoryview(dtree.footer.footer_contents) == 'footer'
    assert memoryview(dtree.contents) == 'some random data'
