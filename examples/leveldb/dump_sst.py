#!/usr/bin/env python

import sys
import os
import argparse
import json
from bitpunch import model

ap = argparse.ArgumentParser(
    description='dump a LevelDB SST file entries in JSON format\n')
ap.add_argument('input_file', type=argparse.FileType('r'), nargs=1,
                help='SST input file')
ap.add_argument('output_file', type=argparse.FileType('wb'), nargs=1,
                help='JSON output file')

args = ap.parse_args()
input_file = args.input_file[0]
output_file = args.output_file[0]
print('dumping SST entries from {inf} to {outf}'.format(inf=input_file.name,
                                                        outf=output_file.name))

script_dir = os.path.dirname(sys.argv[0])
sst_bp = open('{dir}/leveldb_sst.bp'.format(dir=script_dir), 'r')
sst_data = model.DataTree(input_file, sst_bp)

output_file.write('[')
first = True
index = sst_data['?index']
prev_key = ''

#FIXME: replace loop by the commented lines when scoped eval is implemented
#for entry in index['?stored_block'].entries:
#    sub_block = entry.eval('(value: BlockHandle).?stored_block')
#    ...

for i, entry in enumerate(index['?stored_block'].entries):
    sub_block = model.eval('(value: BlockHandle).?stored_block', entry)
    for sub_entry in sub_block.entries:
        if first:
            first = False;
        else:
            output_file.write(',')
        entry_key = (prev_key[:sub_entry.key_shared_size] +
                     model.make_python_object(sub_entry.key_non_shared))
        entry = {
            'key': entry_key,
            'value': model.make_python_object(sub_entry.value)
        }
        prev_key = entry_key
        json.dump(entry, output_file, encoding='iso8859-1')
output_file.write(']')
