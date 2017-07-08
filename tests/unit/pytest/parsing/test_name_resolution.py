#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

#
# Test that expressions are resolved properly in different cases
#

specs_resolve_types_and_expr = ["""
type u8 byte: integer(signed=false);

file {
    Foo foo;
}

struct Foo {
    u8 length;
    byte[length] contents;
};

""", """
type u8 byte: integer(signed=false);

file {
    Foo foo;
}

struct Foo {
    u8 length;
    Bar[length] bars;
};

struct Bar {
    byte[4] sub_bar;
};

""", """
type u8 byte: integer(signed=false);

file {
    byte[] data: Foo;
}

struct Foo {
    u8 length;
    byte[length] contents;
};

""", """
type u8 byte: integer(signed=false);

file {
    byte[] data: Foo[];
}

struct Foo {
    u8 length;
    byte[length] contents;
};

""", """
type u8 byte: integer(signed=false);

file {
}

struct Foo {
    u8 length;
    byte[length] contents;
};

""", """
type u8 byte: integer(signed=false);

file {
    byte[] data;
    ?data => data: Foo;
}

struct Foo {
    u8 length;
    byte[length] contents;
};

""", """
type u8 byte: integer(signed=false);

file {
    byte[] data;
    ?data1 => data;
    ?data2 => ?data1: Foo;
}

struct Foo {
    u8 length;
    byte[length] contents;
};

""", """
type u8 byte: integer(signed=false);

file {
    byte[] data;
    ?data1 => data;
    ?data2 => ?data1: Foo;
}

struct Foo {
    Bar bar;
};

struct Bar {
    u8 length;
    byte[length] contents;
};

""", """
type u8 byte: integer(signed=false);

file {
    byte[] data;
    ?data1 => data;
    ?data2 => ?data1: Foo;
}

struct Foo {
    u8 length;
    Bar[length] bars;
};

struct Bar {
    byte[4] sub_bar;
};

""", """
type u8 byte: integer(signed=false);

file {
    byte[] data;
    ?data1 => data;
    ?data2 => ?data1: Foo;
}

struct Foo {
    Bar bar;

    struct Bar {
        u8 length;
        byte[length] contents;
    };
};

""", """
type u8 byte: integer(signed=false);

file {
    Foo foo;
}

type Foo struct {
    u8 length;
    byte[length] contents;
};

""", """
type u8 byte: integer(signed=false);

file {
    Foo foo;
}

type Foo byte[]: Bar;

struct Bar {
    u8 length;
    byte[length] contents;
};

""", """
type u8 byte: integer(signed=false);

file {
    byte[] data;
    ?data_slice => data[10..20];
}

""", """
type u8 byte: integer(signed=false);

file {
    byte[] data;
    ?data => data;
    ?data_slice1 => ?data[10..20];
}

""", """
type u8 byte: integer(signed=false);

file {
    ?data => bytes(file);
    ?data_slice1 => ?data[10..20];
}

""", """
type u8 byte: integer(signed=false);

file {
    ?data => bytes(file)[..];
    ?data_slice1 => ?data[10..20];
}

""", """
type u8 byte: integer(signed=false);

file {
    ?data => bytes(file)[..]: Foo;
    ?data_slice1 => ?data;
}

struct Foo {
    u8[4] sub_foo;
};

""", """
type u8 byte: integer(signed=false);

file {
    u8 foo_offset;
    u8 nb_foo;
    u8 foo_size;
    ?data => bytes(file)[foo_offset..]: Foo[nb_foo];
    ?data_slice => ?data[2..3];
}

struct Foo {
    u8[foo_size] sub_foo;
};

""", """
type u8 byte: integer(signed=false);

file {
    byte foo_offset;
    byte nb_foo;
    byte foo_size;
    ?data => bytes(file)[foo_offset:u8..]: Foo[nb_foo:u8];
    ?data_slice => ?data[2..3];
}

struct Foo {
    u8[foo_size:u8] sub_foo;
};

"""
]

@pytest.mark.parametrize('spec', specs_resolve_types_and_expr)
def test_resolve_types_and_expr(spec):
    dtree = model.DataTree('', spec)
    assert dtree

#
# Test that unknown type names return a proper error
#

spec_unknown_type_name_1 = """
type Foo byte[42];

file {
    Bar bar;
}

"""

def test_unknown_typename():

    #FIXME use custom error for syntax errors
    with pytest.raises(OSError):
        dtree = model.DataTree('abcdefghij',
                               spec_unknown_type_name_1)
