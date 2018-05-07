#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

spec_file_footer_static_sized_byte_array = """

file {
    contents: [] byte;
    footer: [6] byte;
}

"""

spec_file_footer_static_sized_struct = """

let Footer = struct {
    data1: [4] byte;
    data2: [2] byte;
};

file {
    contents: [] byte;
    footer: Footer;
}

"""

spec_file_footer_static_sized_span = """

let Footer = struct {
    data: [] byte;
    span 6;
};

file {
    contents: [] byte;
    footer: Footer;
}

"""

data_file_footer_static_sized = """
"some random data"
"footer"
"""

spec_file_footer_dynamic_sized_len_at_start = """

let u8 = byte: integer { signed: false; };

file {
    footer_size: u8;
    contents: [] byte;
    footer: [footer_size] byte;
}

"""

data_file_footer_dynamic_sized_len_at_start = """
06
"some random data"
"footer"
"""


spec_file_footer_dynamic_sized_len_in_footer = """

let u8 = byte: integer { signed: false; };

file {
    contents: [] byte;
    footer: [footer_size] byte;
    footer_size: u8;
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
        'spec': spec_file_footer_static_sized_byte_array,
        'data': data_file_footer_static_sized,
    }, {
        'spec': spec_file_footer_static_sized_struct,
        'data': data_file_footer_static_sized,
    }, {
        'spec': spec_file_footer_static_sized_span,
        'data': data_file_footer_static_sized,
    }, {
        'spec': spec_file_footer_dynamic_sized_len_at_start,
        'data': data_file_footer_dynamic_sized_len_at_start,
    }, {
        'spec': spec_file_footer_dynamic_sized_len_in_footer,
        'data': data_file_footer_dynamic_sized_len_in_footer,
    }])
def params_footer_simple(request):
    return conftest.make_testcase(request.param)


def test_footer_simple(params_footer_simple):
    params = params_footer_simple
    dtree = params['dtree']
    assert dtree.eval_expr('sizeof(footer)') == 6

    assert memoryview(dtree.footer) == 'footer'
    assert memoryview(dtree.contents) == 'some random data'



spec_file_footer_dynamic_sized_complex_footer_dynamic_span = """

let u8 = byte: integer { signed: false; };
let u16 = [2] byte: integer { signed: false; endian: 'big'; };

let Footer = struct {
    hdr: [footer_hdr_size] byte;
    footer_contents: [] byte;
    footer_size: u16;
    footer_hdr_size: u8;
    span footer_size;
};

let Contents = struct {
    contents: [] byte;
    footer: Footer;
};

file {
    root: Contents;
}

"""

spec_file_footer_dynamic_sized_complex_footer_static_span = """

let u8 = byte: integer { signed: false; };
let u16 = [2] byte: integer { signed: false; endian: 'big'; };

let Footer = struct {
    hdr: [footer_hdr_size] byte;
    footer_contents: [] byte;
    footer_size: u16;
    footer_hdr_size: u8;
    span 14;
};

let Contents = struct {
    contents: [] byte;
    footer: Footer;
};

file {
    root: Contents;
}

"""

spec_file_footer_dynamic_sized_complex_footer_packed = """

let u8 = byte: integer { signed: false; };
let u16 = [2] byte: integer { signed: false; endian: 'big'; };

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

file {
    root: Contents;
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
        'spec': spec_file_footer_dynamic_sized_complex_footer_dynamic_span,
        'data': data_file_footer_dynamic_sized_complex_footer,
    }, {
        'spec': spec_file_footer_dynamic_sized_complex_footer_static_span,
        'data': data_file_footer_dynamic_sized_complex_footer,
    }, {
        'spec': spec_file_footer_dynamic_sized_complex_footer_packed,
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
    assert memoryview(dtree.root.footer.hdr) == 'fthdr'
    assert memoryview(dtree.root.footer.footer_contents) == 'footer'
    assert memoryview(dtree.root.contents) == 'some random data'

    assert dtree.root.get_size() == 30
