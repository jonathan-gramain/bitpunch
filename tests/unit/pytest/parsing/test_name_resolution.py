#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

#
# Test that expressions are resolved properly in different cases
#

specs_resolve_types_and_expr = ["""
let u8 = byte <> integer { @signed: false; };

env("DATASOURCE") <> struct {
    foo: Foo;
};

let Foo = struct {
    length:   u8;
    contents: [length] byte;
};

""", """
let u8 = byte <> integer { @signed: false; };

env("DATASOURCE") <> struct {
    foo: Foo;
};

let Foo = struct {
    length:     u8;
    bars:       [length] Bar;
};

let Bar = struct {
    sub_bar:    [4] byte;
};

""", """
let u8 = byte <> integer { @signed: false; };

env("DATASOURCE") <> struct {
    data: [] byte <> Foo;
};

let Foo = struct {
    length:   u8;
    contents: [length] byte;
};

""", """
let u8 = byte <> integer { @signed: false; };

env("DATASOURCE") <> struct {
    data: [] byte <> [] Foo;
};

let Foo = struct {
    length:   u8;
    contents: [length] byte;
};

""", """
let u8 = byte <> integer { @signed: false; };

env("DATASOURCE") <> struct {
};

let Foo = struct {
    length:   u8;
    contents: [length] byte;
};

""", """
let u8 = byte <> integer { @signed: false; };

env("DATASOURCE") <> struct {
    data: [] byte;
    let ?data = data <> Foo;
};

let Foo = struct {
    length:   u8;
    contents: [length] byte;
};

""", """
let u8 = byte <> integer { @signed: false; };

env("DATASOURCE") <> struct {
    data: [] byte;
    let ?data1 = data;
    let ?data2 = ?data1 <> Foo;
};

let Foo = struct {
    length:   u8;
    contents: [length] byte;
};

""", """
let u8 = byte <> integer { @signed: false; };

env("DATASOURCE") <> struct {
    data: [] byte;
    let ?data1 = data;
    let ?data2 = ?data1 <> Foo;
};

let Foo = struct {
    bar: Bar;
};

let Bar = struct {
    length:   u8;
    contents: [length] byte;
};

""", """
let u8 = byte <> integer { @signed: false; };

env("DATASOURCE") <> struct {
    data: [] byte;
    let ?data1 = data;
    let ?data2 = ?data1 <> Foo;
};

let Foo = struct {
    length: u8;
    bars:   [length] Bar;
};

let Bar = struct {
    sub_bar: [4] byte;
};

""", """
let u8 = byte <> integer { @signed: false; };

env("DATASOURCE") <> struct {
    data: [] byte;
    let ?data1 = data;
    let ?data2 = ?data1 <> Foo;
};

let Foo = struct {
    bar: Bar;

    let Bar = struct {
        length:   u8;
        contents: [length] byte;
    };
};

""", """
let u8 = byte <> integer { @signed: false; };

env("DATASOURCE") <> struct {
    foo: Foo;
};

let Foo = struct {
    length:   u8;
    contents: [length] byte;
};

""", """
let u8 = byte <> integer { @signed: false; };

env("DATASOURCE") <> struct {
    foo: Foo;
};

let Foo = [] byte <> Bar;

let Bar = struct {
    length:   u8;
    contents: [length] byte;
};

""", """
let u8 = byte <> integer { @signed: false; };

env("DATASOURCE") <> struct {
    data: [] byte;
    let ?data_slice = data[10..20];
};

""", """
let u8 = byte <> integer { @signed: false; };

env("DATASOURCE") <> struct {
    data: [] byte;
    let ?data = data;
    let ?data_slice1 = ?data[10..20];
};

""", """
let u8 = byte <> integer { @signed: false; };

env("DATASOURCE") <> struct {
    let ?data = (self <> bytes);
    let ?data_slice1 = ?data[10..20];
};

""", """
let u8 = byte <> integer { @signed: false; };

env("DATASOURCE") <> struct {
    let ?data = (self <> bytes)[..];
    let ?data_slice1 = ?data[10..20];
};

""", """
let u8 = byte <> integer { @signed: false; };

env("DATASOURCE") <> struct {
    let ?data = (self <> bytes)[..] <> Foo;
    let ?data_slice1 = ?data;
};

let Foo = struct {
    sub_foo: [4] u8;
};

""", """
let u8 = byte <> integer { @signed: false; };

env("DATASOURCE") <> struct {
    foo_offset: u8;
    nb_foo:     u8;
    foo_size:   u8;
    let ?data = (self <> bytes)[foo_offset..] <> [nb_foo] Foo;
    let ?data_slice = ?data[2..3];
};

let Foo = struct {
    sub_foo: [foo_size] u8;
};

""", """
let u8 = byte <> integer { @signed: false; };

env("DATASOURCE") <> struct {
    foo_offset: byte;
    nb_foo:     byte;
    foo_size:   byte;
    let ?data = env("DATASOURCE")[foo_offset <> u8..] <> [nb_foo <> u8] Foo;
    let ?data_slice = ?data[2..3];
};

let Foo = struct {
    sub_foo: [foo_size <> u8] u8;
};

""", """
let u8 = byte <> integer { @signed: false; };

env("DATASOURCE") <> Root;

let Root = struct {
    foo: Foo;
    data: [] byte;
};

let Foo = struct {
    bar: Bar;
};

let Bar = struct {
    start:    u8;
    end:      u8;

    let contents = data[start .. end];
};

""", """
let u8 = byte <> integer { @signed: false; };

env("DATASOURCE") <> Root;

let Root = struct {
    foo: Foo;
};

let Foo = struct {
    bar: Bar;
    data: [] byte;
};

let Bar = struct {
    start:    u8;
    end:      u8;

    let contents = Foo::data[start .. end];
};

"""
]

@pytest.mark.parametrize('spec', specs_resolve_types_and_expr)
def test_resolve_types_and_expr(spec):
    dtree = model.DataTree('          ', spec)
    assert dtree

#
# Test that unknown type names return a proper error
#

spec_unknown_type_name_1 = """
let Foo = [42] byte;

env("DATASOURCE") <> struct {
    bar: Bar;
};

"""

spec_unknown_scope_left_operand = """
let Foo = [42] byte;

env("DATASOURCE") <> struct {
    let bar = Bar::bar;
};

"""

spec_unknown_scoped_member = """
let Foo = struct {
    bar: [42] byte;
};

env("DATASOURCE") <> struct {
    let bar = Foo::qux;
};

"""

def test_syntax_errors():

    #FIXME use custom error for syntax errors
    with pytest.raises(OSError):
        dtree = model.DataTree('abcdefghij',
                               spec_unknown_type_name_1)
    with pytest.raises(OSError):
        dtree = model.DataTree('abcdefghij',
                               spec_unknown_scope_left_operand)
    with pytest.raises(OSError):
        dtree = model.DataTree('abcdefghij',
                               spec_unknown_scoped_member)
