#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

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
        dtree = model.DataTree('', spec_unknown_type_name_1)
