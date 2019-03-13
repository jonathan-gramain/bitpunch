#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

#
# Test span size computation shenanigans
#

spec_span_embedded = """
let u32 = [4] byte <> integer { @signed: false; @endian: 'little'; };

let Schema = struct {
    greeting_set: GreetingSet;
    garbage: [] byte;
};

let Greeting = struct {
    size: u32;
    contents: [] byte;
    @minspan: 5;
    @span: size;
};

let GreetingSet = struct {
    set_size: u32;
    greetings: [] Greeting;
    padding: [] byte;
    @span: set_size;
};

"""


data_span_embedded_no_padding = """
25 00 00 00
09 00 00 00 "Hello" 0b 00 00 00 "Bonjour" 0d 00 00 00 "Guten Tag"
"garbage at the end of file"
"""

data_span_embedded_padding = """
28 00 00 00
09 00 00 00 "Hello" 0b 00 00 00 "Bonjour" 0d 00 00 00 "Guten Tag"
"pad"
"garbage at the end of file"
"""

data_span_embedded_more_padding = """
29 00 00 00
09 00 00 00 "Hello" 0b 00 00 00 "Bonjour" 0d 00 00 00 "Guten Tag"
"padd" # 4 bytes of padding works because Greeting.@minspan == 5
"garbage at the end of file"
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_span_embedded,
        'data': data_span_embedded_no_padding,
        'pad_string': '',
    }, {
        'spec': spec_span_embedded,
        'data': data_span_embedded_padding,
        'pad_string': 'pad',
    }, {
        'spec': spec_span_embedded,
        'data': data_span_embedded_more_padding,
        'pad_string': 'padd',
    }])
def params_span_embedded(request):
    return conftest.make_testcase(request.param)

def test_span_embedded(params_span_embedded):
    dtree, pad_string = (params_span_embedded['dtree'],
                         params_span_embedded['pad_string'])

    assert len(dtree.greeting_set.greetings) == 3
    assert model.make_python_object(
        dtree.greeting_set.greetings[0].contents) == 'Hello'
    assert model.make_python_object(
        dtree.greeting_set.greetings[1].contents) == 'Bonjour'
    assert model.make_python_object(
        dtree.greeting_set.greetings[2].contents) == 'Guten Tag'
    assert model.make_python_object(
        dtree.garbage) == 'garbage at the end of file'

    with pytest.raises(IndexError):
        dtree.greeting_set.greetings[3]

    assert model.make_python_object(
        dtree.greeting_set.padding) == pad_string


spec_span_lazy_eval = """
let number = [3] byte <> formatted_integer {
    @base: 10;
    @signed: false;
};

let Header = struct {
    magic: [5] byte;
    size: number;
    @span: 10;
};

let Schema = struct {
    entries: [] Entry;
};

let Entry = struct {
    header: Header;
    greetings: [header.size] byte <> [] Greeting;
};

let Greeting = string { @boundary: ' '; };

"""


data_span_lazy_eval_ok = """
"magic"
"023"
00 00
"Hello Bonjour GutenTag "
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_span_lazy_eval,
        'data': data_span_lazy_eval_ok,
    }])
def params_span_lazy_eval_ok(request):
    return conftest.make_testcase(request.param)

def test_span_lazy_eval_ok(params_span_lazy_eval_ok):
    dtree = params_span_lazy_eval_ok['dtree']

    assert dtree.entries[0].header.magic == 'magic'
    assert dtree.eval_expr('entries[0].header.magic') == 'magic'
    assert dtree.entries[0].header.size == 23
    assert dtree.entries[0].greetings[0] == 'Hello'
    assert dtree.entries[0].greetings[1] == 'Bonjour'
    assert dtree.entries[0].greetings[2] == 'GutenTag'


data_span_lazy_eval_error = """
"magic"
"foo"
00 00
"Hello Bonjour GutenTag "
"""

def test_span_lazy_eval_error():
    data = conftest.to_bytes(data_span_lazy_eval_error)
    board = model.Board()
    board.add_data_source('data', data)
    board.add_spec('Spec', spec_span_lazy_eval)
    board.add_expr('dtree', 'data <> Spec.Schema')

    # lazy span size evaluation should allow accessing fields that do
    # not depend on the entry size, even if the entry size is
    # incorrect (formatted size is "foo")
    assert board.eval_expr('dtree.entries[0].header.magic') == 'magic'
    with pytest.raises(model.DataError):
        print board.eval_expr('dtree.entries[0].header.size')
    assert board.eval_expr('^dtree.entries[0].header.size') == 'foo'
