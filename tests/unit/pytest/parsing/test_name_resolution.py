#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

#
# Test that expressions are resolved properly in different cases
#

specs_resolve_types_and_expr = ["""
let u8 = byte: integer { signed: false; };

file {
    foo: Foo;
}

let Foo = struct {
    length:   u8;
    contents: byte[length];
};

""", """
let u8 = byte: integer { signed: false; };

file {
    foo: Foo;
}

let Foo = struct {
    length:     u8;
    bars:       Bar[length];
};

let Bar = struct {
    sub_bar:    byte[4];
};

""", """
let u8 = byte: integer { signed: false; };

file {
    data: byte[]: Foo;
}

let Foo = struct {
    length:   u8;
    contents: byte[length];
};

""", """
let u8 = byte: integer { signed: false; };

file {
    data: byte[]: Foo[];
}

let Foo = struct {
    length:   u8;
    contents: byte[length];
};

""", """
let u8 = byte: integer { signed: false; };

file {
}

let Foo = struct {
    length:   u8;
    contents: byte[length];
};

""", """
let u8 = byte: integer { signed: false; };

file {
    data: byte[];
    let ?data = data: Foo;
}

let Foo = struct {
    length:   u8;
    contents: byte[length];
};

""", """
let u8 = byte: integer { signed: false; };

file {
    data: byte[];
    let ?data1 = data;
    let ?data2 = ?data1: Foo;
}

let Foo = struct {
    length:   u8;
    contents: byte[length];
};

""", """
let u8 = byte: integer { signed: false; };

file {
    data: byte[];
    let ?data1 = data;
    let ?data2 = ?data1: Foo;
}

let Foo = struct {
    bar: Bar;
};

let Bar = struct {
    length:   u8;
    contents: byte[length];
};

""", """
let u8 = byte: integer { signed: false; };

file {
    data: byte[];
    let ?data1 = data;
    let ?data2 = ?data1: Foo;
}

let Foo = struct {
    length: u8;
    bars:   Bar[length];
};

let Bar = struct {
    sub_bar: byte[4];
};

""", """
let u8 = byte: integer { signed: false; };

file {
    data: byte[];
    let ?data1 = data;
    let ?data2 = ?data1: Foo;
}

let Foo = struct {
    bar: Bar;

    let Bar = struct {
        length:   u8;
        contents: byte[length];
    };
};

""", """
let u8 = byte: integer { signed: false; };

file {
    foo: Foo;
}

let Foo = struct {
    length:   u8;
    contents: byte[length];
};

""", """
let u8 = byte: integer { signed: false; };

file {
    foo: Foo;
}

let Foo = byte[]: Bar;

let Bar = struct {
    length:   u8;
    contents: byte[length];
};

""", """
let u8 = byte: integer { signed: false; };

file {
    data: byte[];
    let ?data_slice = data[10..20];
}

""", """
let u8 = byte: integer { signed: false; };

file {
    data: byte[];
    let ?data = data;
    let ?data_slice1 = ?data[10..20];
}

""", """
let u8 = byte: integer { signed: false; };

file {
    let ?data = bytes(file);
    let ?data_slice1 = ?data[10..20];
}

""", """
let u8 = byte: integer { signed: false; };

file {
    let ?data = bytes(file)[..];
    let ?data_slice1 = ?data[10..20];
}

""", """
let u8 = byte: integer { signed: false; };

file {
    let ?data = bytes(file)[..]: Foo;
    let ?data_slice1 = ?data;
}

let Foo = struct {
    sub_foo: u8[4];
};

""", """
let u8 = byte: integer { signed: false; };

file {
    foo_offset: u8;
    nb_foo:     u8;
    foo_size:   u8;
    let ?data = bytes(file)[foo_offset..]: Foo[nb_foo];
    let ?data_slice = ?data[2..3];
}

let Foo = struct {
    sub_foo: u8[foo_size];
};

""", """
let u8 = byte: integer { signed: false; };

file {
    foo_offset: byte;
    nb_foo:     byte;
    foo_size:   byte;
    let ?data = bytes(file)[foo_offset:u8..]: Foo[nb_foo:u8];
    let ?data_slice = ?data[2..3];
}

let Foo = struct {
    sub_foo: u8[foo_size: u8];
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
let Foo = byte[42];

file {
    bar: Bar;
}

"""

def test_unknown_typename():

    #FIXME use custom error for syntax errors
    with pytest.raises(OSError):
        dtree = model.DataTree('abcdefghij',
                               spec_unknown_type_name_1)
