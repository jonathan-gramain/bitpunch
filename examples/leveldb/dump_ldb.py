#!/usr/bin/env python

import sys
import os
import argparse
import json
from bitpunch import model


def records_dumper(records_generator, output_file):
    first = True
    n_dumped_records = 0
    output_file.write('[')
    for record in records_generator:
        if first:
            first = False;
        else:
            output_file.write(',')

        # json is utf-8 based, since we want to dump binary data which
        # may be most easily read as ASCII most of the time
        # (especially for keys) we choose a single-byte-per-character
        # encoding which allows the full range of 256 values per
        # byte. The non-ASCII characters will then be encoded with
        # their unicode equivalent in latin-1.
        json.dump(record, output_file, encoding='latin-1')

        n_dumped_records += 1
    output_file.write(']')
    return n_dumped_records

def records_extractor(ldb_model):
    index = ldb_model['?index']
    prev_key = b''

    for entry in index['?stored_block'].entries:
        # recurse one more level into the LDB index to get the data,
        # because first-level values are block handles to other
        # sub-blocks in the file.
        sub_block = entry.eval_expr('(value <> BlockHandle).?stored_block')
        for sub_entry in sub_block.entries:
            entry_key = (prev_key[:sub_entry.key_shared_size] +
                         bytes(sub_entry.key_non_shared))
            yield {
                'key': entry_key,
                'value': bytes(sub_entry.value),
            }
            prev_key = entry_key


if __name__ == '__main__':
    ap = argparse.ArgumentParser(
        description='dump a LevelDB .LDB data file to JSON\n')
    ap.add_argument('input_file', type=argparse.FileType('r'), nargs=1,
                    help='LDB input file')
    ap.add_argument('output_file', type=argparse.FileType('wb'), nargs='?',
                    help='JSON output file')

    args = ap.parse_args()
    input_file = args.input_file[0]
    if args.output_file is not None:
        output_file = args.output_file
    else:
        output_file = sys.stdout
    sys.stderr.write('dumping LDB entries from {inf} to {outf}\n'
                     .format(inf=input_file.name,
                             outf=output_file.name))

    script_dir = os.path.dirname(sys.argv[0])
    leveldb_bp = model.find_handler(path='database.leveldb')
    board = model.Board()
    board.add_spec('LevelDB', leveldb_bp)
    board.add_data_source('db', input_file)
    ldb_model = board.eval_expr('db <> LevelDB.SSTFile')

    # here goes the magic :)
    n_dumped_records = records_dumper(
        records_extractor(ldb_model), output_file)

sys.stderr.write('dumped {0} records\n'.format(n_dumped_records))
