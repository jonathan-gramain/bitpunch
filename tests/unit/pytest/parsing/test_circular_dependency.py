#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

#
# Test circular dependency detection and error reporting
#

spec_file_circular_dependency_1 = """
let CircularStruct = struct {
    circular: CircularStruct;
};

file {
    a: CircularStruct;
}
"""

spec_file_circular_dependency_2 = """
let OuterStruct = struct {
    middle: MiddleStruct;
};

let MiddleStruct = struct {
    inner: InnerStruct;
};

let InnerStruct = struct {
    outer: OuterStruct; // circular
};

file {
    a: OuterStruct;
}
"""

spec_file_circular_dependency_3 = """

let u32 = byte[4]: integer { signed: false; endian: 'little'; };

let OuterStruct = struct {
    middle: MiddleStruct;
};

let MiddleStruct = struct {
    inner: InnerStruct;
};

let InnerStruct = struct {
    a: OkStruct1;
    b: OkUnion1;
};

let OkStruct1 = struct {
    a: OkStruct2;
    b: OkUnion1;
};

let OkStruct2 = struct {
    a: u32;
    b: u32;
};

let OkUnion1 = union {
    a: u32;
    b: byte[5];
};

file {
    a: OkStruct1;
}
"""

spec_file_circular_dependency_3 = """

let u32 = byte[4]: integer { signed: false; endian: 'little'; };

let OuterStruct = struct {
    middle: MiddleStruct;
};

let MiddleStruct = struct {
    inner: InnerStruct;
};

let InnerStruct = struct {
    a: CircularStruct1;
    b: CircularUnion1;
};

let CircularStruct1 = struct {
    a: OkStruct2;
    b: CircularUnion1;
};

let OkStruct2 = struct {
    a: u32;
    b: u32;
};

let CircularUnion1 = union {
    a: u32;
    b: CircularStruct1;
    c: byte[5];
};

file {
    a: OuterStruct;
}
"""

spec_file_circular_dependency_4 = """
file {
    a: OuterStruct;
}

let OuterStruct = struct {
    circular: Item[42];
};

let Item = struct {
    s: OuterStruct;
};

"""

spec_file_no_circular_dependency_1 = """

let u32 = byte[4]: integer { signed: false; endian: 'little'; };

let OuterStruct = struct {
    middle: MiddleStruct;
};

let MiddleStruct = struct {
    inner: InnerStruct;
};

let InnerStruct = struct {
    a: OkStruct1;
    b: OkUnion1;
};

let OkStruct1 = struct {
    a: OkStruct2;
    b: OkUnion1;
};

let OkStruct2 = struct {
    a: u32;
    b: u32;
};

let OkUnion1 = union {
    a: u32;
    b: byte[5];
};

file {
    a: OuterStruct;
}
"""

spec_file_no_circular_dependency_2 = """

let u32 = byte[4]: integer { signed: false; endian: 'little'; };

file {
    a: OuterStruct;
}

let OuterStruct = struct {
    nb: u32;
    circular: Item[nb];
};

let Item = struct {
    s: OuterStruct;
};

"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_circular_dependency_1,
        'circular': True
    }, {
        'spec': spec_file_circular_dependency_2,
        'circular': True
    }, {
        'spec': spec_file_circular_dependency_3,
        'circular': True
    }, {
        'spec': spec_file_circular_dependency_4,
        'circular': True
    }, {
        'spec': spec_file_no_circular_dependency_1,
        'circular': False
    }, {
        'spec': spec_file_no_circular_dependency_2,
        'circular': False
    }])
def params_circular_dependency(request):
    return request.param

def test_circular_dependency(params_circular_dependency):
    params = params_circular_dependency
    spec = params['spec']
    circular = params['circular']

    data = 'foobarfoobarfoobarfoobarfoobar'
    if circular:
        with pytest.raises(OSError):
            dtree = model.DataTree(data, spec)
    else:
        dtree = model.DataTree(data, spec)
