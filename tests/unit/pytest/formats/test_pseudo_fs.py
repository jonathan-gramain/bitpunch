#!/usr/bin/env python

import pytest

from bitpunch import model
from bitpunch_cli import CLI
import conftest


# This one may look like the premises of a filesystem format, but it's
# purely fictitious :)

spec_file_pseudo_fs = """

let Int = integer { signed: false; endian: 'little'; };
let u8 = byte: Int;
let u16 = byte[2]: Int;

file {
    hdr:     Header;
    contents: byte[];

    let ?raw_catalog = bytes(file)[hdr.catalog_location..];
    let ?catalog = ?raw_catalog: Entry[hdr.nb_catalog_entries];
}

let Header = struct {
    catalog_location:    u16;
    nb_catalog_entries:  u16;
};

let Entry = struct {
    entry_location: u16;
    entry_size:     u16;
    entry_type:     byte[4]: string { boundary: ' '; };

    let ?data = bytes(file)[entry_location ..
                            entry_location + entry_size];
    if (entry_type == 'file') {
        let ?file = ?data: File;
        let ?props = ?file;
    }
    if (entry_type == 'dir') {
        let ?dir = ?data: Dir;
        let ?props = ?dir;
    }

    let File = struct {
        filesize:        u16;
        filedata_offset: u16;
        dir_entry_index: u16;
        filename:        byte[]: string;

        let ?dir = ?catalog[dir_entry_index].?data: Dir;
        let ?filedata = bytes(file)[filedata_offset ..
                                    filedata_offset + filesize];
    };

    let Dir = struct {
        dirname:         byte[]: string;
    };
};

"""

data_file_pseudo_fs = """
# header
  # catalog location
  64 00
  # nb catalog entries
  07 00

# contents
    "directory1"

    "directory2"

    0a 00 # file size
    9c 00 # data offset
    00 00 # dir entry index
    "foo"

    0b 00
    b2 00
    00 00
    "bar"

    0c 00
    a6 00
    00 00
    "baz"

    0d 00
    bd 00
    01 00
    "a somewhat long file name"

    0e 00
    ca 00
    01 00
    "shorter name"

# catalog
    # entry [0]
    04 00  # location
    0a 00  # size
    "dir " # type

    # entry [1]
    0e 00  # location
    0a 00  # size
    "dir " # type

    # entry [2]
    18 00  # location
    09 00  # size
    "file" # type

    # entry [3]
    21 00  # location
    09 00  # size
    "file" # type

    # entry [4]
    2a 00  # location
    09 00  # size
    "file" # type

    # entry [5]
    33 00  # location
    1f 00  # size
    "file" # type

    # entry [6]
    52 00  # location
    12 00  # size
    "file" # type

# raw data

    "1111111111"
    "333333333333"
    "22222222222"
    "4444444444444"
    "55555555555555"
"""


@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_pseudo_fs,
        'data': data_file_pseudo_fs,
    }])
def params_pseudo_fs(request):
    return conftest.make_testcase(request.param)


def test_pseudo_fs(params_pseudo_fs):
    params = params_pseudo_fs
    dtree = params['dtree']

    catalog = dtree['?catalog']
    assert len(catalog) == 7

    assert catalog[0]['?dir'].get_location() == (0x04, 0x0A)
    assert str(catalog[1].eval_expr('?dir.dirname')) == 'directory2'
    assert str(catalog[1].eval_expr('?props.dirname')) == 'directory2'

    assert catalog[2]['?file'].get_location() == (0x18, 0x09)
    assert catalog[2]['?props'].get_location() == (0x18, 0x09)

    assert model.make_python_object(
        catalog[2]['?file']['?filedata']) == '1111111111'
    assert model.make_python_object(
        dtree.eval_expr('?catalog[2].?file.?filedata')) == '1111111111'
    assert model.make_python_object(
        catalog[2]['?props']['?filedata']) == '1111111111'
    assert model.make_python_object(
        catalog[2].eval_expr('?props.?filedata')) == '1111111111'
    assert model.make_python_object(
        dtree.eval_expr('?catalog[2].?props.?filedata')) == '1111111111'

    assert model.make_python_object(
        catalog[3]['?file']['?filedata']) == '22222222222'

    assert model.make_python_object(
        catalog[4]['?file']['?filedata']) == '333333333333'

    assert model.make_python_object(
        catalog[5]['?file']['?filedata']) == '4444444444444'

    assert model.make_python_object(
        catalog[6]['?file']['?filedata']) == '55555555555555'

    assert str(catalog[2]['?file']['?dir'].dirname) == 'directory1'
    assert str(catalog[3]['?file']['?dir'].dirname) == 'directory1'
    assert str(catalog[4]['?file']['?dir'].dirname) == 'directory1'
    assert str(catalog[5]['?file']['?dir'].dirname) == 'directory2'
    assert str(catalog[6]['?file']['?dir'].dirname) == 'directory2'

    assert catalog.get_size() == 56
    assert catalog[3].get_size() == 8
    assert catalog[3]['?data'].get_size() == 9
    assert catalog[3]['?file'].get_size() == 9
    assert dtree.eval_expr('sizeof(?catalog[3].?file)') == 9
    assert dtree.eval_expr('sizeof(Entry)') == 8
    assert dtree.eval_expr('sizeof(Entry.entry_size)') == 2
    assert dtree.eval_expr('sizeof(Entry.File.filesize)') == 2
    assert dtree.eval_expr('sizeof(?catalog[3].File.filesize)') == 2

    # cannot do sizeof() on dynamic-sized type name
    with pytest.raises(ValueError):
        dtree.eval_expr('sizeof(Entry.Dir)')

    with pytest.raises(ValueError):
        dtree.eval_expr('sizeof(Entry.File.filename)')
    with pytest.raises(ValueError):
        dtree.eval_expr('sizeof(?catalog[3].File.filename)')

    # cannot do sizeof() on non-dpath if not type
    with pytest.raises(ValueError):
        dtree.eval_expr('sizeof(Entry.?data)')

    #cli = CLI()
    #cli.attach_data_tree(dtree)
    #cli.cmdloop()
