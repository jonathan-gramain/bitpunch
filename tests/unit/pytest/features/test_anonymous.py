#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

spec_file_anonymous_struct_embedded = """

let u32 = [4] byte <> integer { endian: 'little'; signed: false; };

file {
    : struct {
        a: u32;
        : struct {
            : struct {
                b: u32;
                c: u32;
            };
            d: u32;
        };
    };
}

"""

spec_file_anonymous_field = """

let u32 = [4] byte <> integer { endian: 'little'; signed: false; };

let Foo = struct {
    a: u32;
};

let Bar = struct {
    b: u32;
    c: u32;
};

file {
    :  Foo;
    :  Bar;
    d: u32;
}

"""

spec_file_hidden_field = """

let u8 = byte <> integer { signed: false; };
let u32 = [4] byte <> integer { endian: 'little'; signed: false; };

let Foo = struct {
    a: u32;
};

let Bar = struct {
    b: u8;
    :  [3] byte; // hidden field
    c: u32;
};

file {
    :  Foo;
    :  Bar;
    d: u32;
}

"""

data_file_anonymous = """
01 00 00 00
02 00 00 00
03 00 00 00
04 00 00 00
"""



spec_file_anonymous_in_trailer = """

let u8 = byte <> integer { signed: false; };
let u32 = [4] byte <> integer { endian: 'little'; signed: false; };

let Foo = struct {
    a: u32;
};

let Bar = struct {
    b: u8;
    :  [3] byte; // hidden field
};

let SlackBody = struct {
    : struct {
        :     [2] byte;
        data: [] byte;
    };
};

let Trailer = struct {
    : [4] byte;
    : struct {
        c: u32;
    };
    d: u32;
};

file {
    : Foo;
    : Bar;
    : SlackBody;
    : Trailer;
}

"""

data_file_anonymous_in_trailer = """
01 00 00 00
02 00 00 00
"some useless data"
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
    }, {
        'spec': spec_file_anonymous_in_trailer,
        'data': data_file_anonymous_in_trailer,
    }])
def params_anonymous(request):
    return conftest.make_testcase(request.param)


def test_anonymous(params_anonymous):
    params = params_anonymous
    dtree = params['dtree']
    assert dtree.d == 4
    assert dtree.c == 3
    assert dtree.a == 1
    assert dtree.b == 2
