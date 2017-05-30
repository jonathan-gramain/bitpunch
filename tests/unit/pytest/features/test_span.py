#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

#
# Test the span keyword feature
#

spec_embedded = """
type u32 byte[4]: integer(signed=false, endian=little);

file {
    GreetingSet greeting_set;
    byte[] garbage;
}

struct Greeting {
    u32 size;
    byte[] contents;
    span size;
};

struct GreetingSet {
    u32 set_size;
    Greeting[] greetings;
    byte[] padding;
    span set_size;
};

"""


data_no_padding = """
25 00 00 00
09 00 00 00 "Hello" 0b 00 00 00 "Bonjour" 0d 00 00 00 "Guten Tag"
"garbage at the end of file"
"""

data_padding = """
28 00 00 00
09 00 00 00 "Hello" 0b 00 00 00 "Bonjour" 0d 00 00 00 "Guten Tag"
"pad"
"garbage at the end of file"
"""

data_more_padding = """
29 00 00 00
09 00 00 00 "Hello" 0b 00 00 00 "Bonjour" 0d 00 00 00 "Guten Tag"
"padd"
"garbage at the end of file"
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_embedded,
        'data': data_no_padding,
        'pad_string': '',
    }, {
        'spec': spec_embedded,
        'data': data_padding,
        'pad_string': 'pad',
    }, {
        'spec': spec_embedded,
        'data': data_more_padding,
        'pad_string': 'padd',
    }])
def params(request):
    return conftest.make_testcase(request.param)

def test_embedded(params):
    dtree, pad_string = params['dtree'], params['pad_string']

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
