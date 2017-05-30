#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

LOREM_IPSUM = """
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam nec
leo ac lacus sagittis condimentum. Maecenas diam lorem, imperdiet non
turpis sed, dignissim vulputate nibh. Aenean at malesuada
nulla. Suspendisse sagittis aliquet hendrerit. Nulla nec pharetra
odio. Mauris ut egestas lorem. Fusce id molestie nunc, a venenatis
augue.

Suspendisse eu ante aliquet tellus viverra hendrerit sit amet id
justo. Morbi congue vitae enim eget interdum. Suspendisse elementum
vitae sem sit amet laoreet. Phasellus placerat eleifend nisi nec
ultricies. Quisque dignissim rhoncus augue, vel sollicitudin nisi
ultrices eu. Phasellus lectus lectus, mattis in odio at, accumsan
euismod orci. Cras nec leo accumsan, vestibulum ipsum eu, convallis
massa. Vivamus et metus suscipit urna tristique fringilla ultrices non
dolor. Nunc tempus finibus eros, ut ullamcorper erat imperdiet
id. Vestibulum vel sagittis dolor.

Fusce pulvinar sem tellus, vel posuere nulla dapibus vel. Nulla sit
amet finibus felis. Donec molestie nisl diam, id porta massa tempor
eu. Donec laoreet quam arcu, in consequat eros rhoncus
sed. Suspendisse blandit imperdiet nunc, imperdiet bibendum lorem
tristique nec. Etiam euismod dui ac interdum placerat. Quisque rutrum
nisi molestie dolor iaculis fringilla. Sed orci ex, ornare vel rhoncus
et, feugiat id nibh. Phasellus gravida ante sit amet diam pretium, sit
amet tincidunt odio commodo. Duis eu ex eu neque iaculis hendrerit a
eget sem. Suspendisse vitae cursus dui. Aenean mollis laoreet risus, a
tincidunt sem varius eu. Ut et congue purus, sit amet sodales
ex. Phasellus vehicula sem quis mi placerat, auctor sagittis metus
interdum. Nullam luctus placerat quam, nec interdum nisl ullamcorper
id.

In sodales blandit neque eget mattis. Ut purus lectus, porttitor ut
mollis ut, congue sit amet tortor. Curabitur aliquam molestie nunc ut
elementum. Maecenas ut est iaculis nunc fermentum porta. Donec
pharetra varius dui, in feugiat nisl bibendum ut. Donec ut nunc non
justo consectetur ornare. Nunc eleifend mauris non egestas
pellentesque. In vel nulla aliquet, sagittis quam a, dignissim
dui. Aenean id est bibendum, auctor nisl in, pharetra nisl. Aenean
ultricies tristique tempor. Curabitur laoreet vel massa in ultrices.

Vivamus a diam fermentum, lacinia massa non, sagittis lacus. Nullam
tempor justo dapibus ornare blandit. Mauris dapibus orci ipsum, a
ullamcorper leo iaculis vitae. Mauris id gravida sem. Curabitur ac
sapien id neque vestibulum dapibus. Phasellus eget justo at mauris
tincidunt ullamcorper quis sit amet turpis. Cras sodales nibh sed
ligula scelerisque tristique.
"""

#
# Test parsing of integers and string literals
#

def test_integer_literals():
    assert model.eval('0') == 0
    assert model.eval('1') == 1
    assert model.eval('123456789') == 123456789
    assert model.eval('42000000000000') == 42000000000000
    assert model.eval('07') == 7
    assert model.eval('01234567') == 01234567
    assert model.eval('0xdeadbeef') == 0xdeadbeef
    assert model.eval('0xdeadbeefbadf00d') == 0xdeadbeefbadf00d

    with pytest.raises(ValueError):
        model.eval('0abc')
    with pytest.raises(ValueError):
        model.eval('0xffeeg')
    with pytest.raises(ValueError):
        model.eval('42a')
    with pytest.raises(ValueError):
        model.eval('42a')
    with pytest.raises(ValueError):
        model.eval('08')


def test_string_literals():
    assert model.eval('"hi"') == 'hi'
    assert model.eval("'hello'") == 'hello'
    assert model.eval("'how\\nare\\nyou'") == 'how\nare\nyou'
    assert model.eval("'\\0\\x00\\377'") == '\0\x00\377'
    assert model.eval("'\\r\\t\\n'") == '\r\t\n'
    assert model.eval('"\\r\\t\\n"') == '\r\t\n'
    assert model.eval("'multi'' ' 'part' ' ''string'") == 'multi part string'
    # create a concatenation of lines as literals
    lorem_ipsum_quoted = "'" + LOREM_IPSUM.replace('\n', "\\n'\n    '") + "'"
    assert model.eval(lorem_ipsum_quoted) == LOREM_IPSUM

    with pytest.raises(ValueError):
        model.eval('hello')
