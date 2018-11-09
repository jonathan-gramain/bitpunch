#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

spec_file_footer_const_sized_byte_array = """

env("FILE") <> struct {
    contents: [] byte;
    footer: [6] byte;
};

"""

spec_file_footer_const_sized_struct = """

let Footer = struct {
    data1: [4] byte;
    data2: [2] byte;
};

env("FILE") <> struct {
    contents: [] byte;
    footer: Footer;
};

"""

spec_file_footer_const_sized_span = """

let Footer = struct {
    data: [] byte;
    @span = 6;
};

env("FILE") <> struct {
    contents: [] byte;
    footer: Footer;
};

"""

data_file_footer_const_sized = """
"some random data"
"footer"
"""

spec_file_footer_var_sized_len_at_start = """

let u8 = byte <> integer { @signed = false; };

env("FILE") <> struct {
    footer_size: u8;
    contents: [] byte;
    footer: [footer_size] byte;
};

"""

data_file_footer_var_sized_len_at_start = """
06
"some random data"
"footer"
"""


spec_file_footer_var_sized_len_in_footer = """

let u8 = byte <> integer { @signed = false; };

env("FILE") <> struct {
    contents: [] byte;
    footer: [footer_size] byte;
    footer_size: u8;
};

"""

data_file_footer_var_sized_len_in_footer = """
"some random data"
"footer"
06
"""


@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_footer_const_sized_byte_array,
        'data': data_file_footer_const_sized,
    }, {
        'spec': spec_file_footer_const_sized_struct,
        'data': data_file_footer_const_sized,
    }, {
        'spec': spec_file_footer_const_sized_span,
        'data': data_file_footer_const_sized,
    }, {
        'spec': spec_file_footer_var_sized_len_at_start,
        'data': data_file_footer_var_sized_len_at_start,
    }, {
        'spec': spec_file_footer_var_sized_len_in_footer,
        'data': data_file_footer_var_sized_len_in_footer,
    }])
def params_footer_simple(request):
    return conftest.make_testcase(request.param)


def test_footer_simple(params_footer_simple):
    params = params_footer_simple
    dtree = params['dtree']
    assert dtree.eval_expr('sizeof(footer)') == 6

    assert memoryview(dtree.footer) == 'footer'
    assert memoryview(dtree.contents) == 'some random data'



spec_file_footer_var_sized_complex_footer_var_span = """

let u8 = byte <> integer { @signed = false; };
let u16 = [2] byte <> integer { @signed = false; @endian = 'big'; };

let Footer = struct {
    hdr: [footer_hdr_size] byte;
    footer_contents: [] byte;
    footer_size: u16;
    footer_hdr_size: u8;
    @span = footer_size;
};

let Contents = struct {
    contents: [] byte;
    footer: Footer;
};

env("FILE") <> struct {
    root: Contents;
};

"""

spec_file_footer_var_sized_complex_footer_static_span = """

let u8 = byte <> integer { @signed = false; };
let u16 = [2] byte <> integer { @signed = false; @endian = 'big'; };

let Footer = struct {
    hdr: [footer_hdr_size] byte;
    footer_contents: [] byte;
    footer_size: u16;
    footer_hdr_size: u8;
    @span = 14;
};

let Contents = struct {
    contents: [] byte;
    footer: Footer;
};

env("FILE") <> struct {
    root: Contents;
};

"""

spec_file_footer_var_sized_complex_footer_packed = """

let u8 = byte <> integer { @signed = false; };
let u16 = [2] byte <> integer { @signed = false; @endian = 'big'; };

let Footer = struct {
    hdr: [footer_hdr_size] byte;
    footer_contents: [6] byte;
    footer_size: u16;
    footer_hdr_size: u8;
};

let Contents = struct {
    contents: [] byte;
    footer: Footer;
};

env("FILE") <> struct {
    root: Contents;
};

"""

data_file_footer_var_sized_complex_footer = """
"some random data"
"fthdr"
"footer"
00 0e
05
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_footer_var_sized_complex_footer_var_span,
        'data': data_file_footer_var_sized_complex_footer,
    }, {
        'spec': spec_file_footer_var_sized_complex_footer_static_span,
        'data': data_file_footer_var_sized_complex_footer,
    }, {
        'spec': spec_file_footer_var_sized_complex_footer_packed,
        'data': data_file_footer_var_sized_complex_footer,
    }])
def params_footer_complex(request):
    return conftest.make_testcase(request.param)


def test_footer_complex(params_footer_complex):
    params = params_footer_complex
    dtree = params['dtree']
    assert dtree.root.footer.get_size() == 14

    assert dtree.root.footer.get_size() == dtree.root.footer.footer_size
    assert dtree.root.footer.footer_contents.get_size() == 6
    assert memoryview(dtree.root.footer.hdr) == 'fthdr'
    assert memoryview(dtree.root.footer.footer_contents) == 'footer'
    assert memoryview(dtree.root.contents) == 'some random data'

    assert dtree.root.get_size() == 30


spec_file_footer_var_sized_array_const_item_size_1 = """

let u8 = byte <> integer { @signed = false; };
let u16 = [2] byte <> integer { @signed = false; @endian = 'big'; };

let Footer = struct {
    footer_items: [nb_footer_items] u16;
    nb_footer_items: u8;
};

let Contents = struct {
    contents: [] byte;
    footer: Footer;
};

env("FILE") <> struct {
    root: Contents;
};

"""

spec_file_footer_var_sized_array_const_item_size_2 = """

let u8 = byte <> integer { @signed = false; };
let u16 = [2] byte <> integer { @signed = false; @endian = 'big'; };

let Footer = struct {
    footer_items: [] u16;
    nb_footer_items: u8;

    @span = nb_footer_items * 2 + 1;
};

let Contents = struct {
    contents: [] byte;
    footer: Footer;
};

env("FILE") <> struct {
    root: Contents;
};

"""

data_file_footer_var_sized_array_const_item_size = """
"some random data"
00 01 00 02 00 03 00 04 00 05
05
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_footer_var_sized_array_const_item_size_1,
        'data': data_file_footer_var_sized_array_const_item_size,
    }, {
        'spec': spec_file_footer_var_sized_array_const_item_size_2,
        'data': data_file_footer_var_sized_array_const_item_size,
    }])
def params_footer_var_sized_array_const_item_size(request):
    return conftest.make_testcase(request.param)


def test_footer_var_sized_array_const_item_size(
        params_footer_var_sized_array_const_item_size):
    params = params_footer_var_sized_array_const_item_size
    dtree = params['dtree']
    assert dtree.root.footer.get_size() == 11
    assert dtree.root.footer.footer_items.get_size() == 10
    assert dtree.root.footer.nb_footer_items == 5
    assert memoryview(dtree.root.contents) == 'some random data'
    assert model.make_python_object(dtree.root.footer.footer_items) == \
        [1, 2, 3, 4, 5]

    assert dtree.root.get_size() == 27
