#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

spec_file_anonymous_struct_embedded = """

type u32 byte[4]: integer(endian=little, signed=false);

file {
    struct {
        u32 a;
        struct {
            struct {
                u32 b;
                u32 c;
            };
            u32 d;
        };
    };
}

"""

spec_file_anonymous_field = """

type u32 byte[4]: integer(endian=little, signed=false);

struct Foo {
    u32 a;
};

struct Bar {
    u32 b;
    u32 c;
};

file {
    Foo;
    Bar;
    u32 d;
}

"""

spec_file_hidden_field = """

type u8 byte: integer(signed=false);
type u32 byte[4]: integer(endian=little, signed=false);

struct Foo {
    u32 a;
};

struct Bar {
    u8 b;
    byte[3]; // hidden field
    u32 c;
};

file {
    Foo;
    Bar;
    u32 d;
}

"""

data_file_anonymous = """
01 00 00 00
02 00 00 00
03 00 00 00
04 00 00 00
"""


@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_anonymous_struct_embedded,
        'data': data_file_anonymous,
    }, {
        'spec': spec_file_anonymous_field,
        'data': data_file_anonymous,
    }, {
        'spec': spec_file_hidden_field,
        'data': data_file_anonymous,
    }])
def params_anonymous(request):
    return conftest.make_testcase(request.param)


def test_anonymous(params_anonymous):
    params = params_anonymous
    dtree = params['dtree']
    assert dtree.a == 1
    assert dtree.b == 2
    assert dtree.c == 3
    assert dtree.d == 4
