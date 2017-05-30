import pytest
import re
import os

from bitpunch import model

def to_bytes(bin_repr):
    res = bytearray()
    for token_groups in re.findall('([0-9a-fA-F]{2})'
                                   '|"((\\\\["\\\\]|[^"])*)"'
                                   '|\'((\\\\[\'\\\\]|[^\'])*)\''
                                   '|(#[^\n]*\n)',
                                   bin_repr):
        (tok_hex, tok_dquot, tok_squot, tok_comment) = (token_groups[0],
                                                        token_groups[1],
                                                        token_groups[3],
                                                        token_groups[5])
        tok_quot = tok_dquot if tok_dquot else tok_squot
        if tok_hex:
            value = int(tok_hex, base=16)
            if value < 0 or value > 255:
                raise ValueError('Value "{}" does not fit into a byte'
                                 .format(value))
            res.append(chr(value))
        elif tok_quot:
            tok_unescaped = tok_quot.replace('\\"', '"') \
                                    .replace("\\'", "'")
            res.extend(tok_unescaped)
        elif tok_comment:
            pass

    return res


def make_testcase(param):
    data = to_bytes(param['data'])
    param['dtree'] = model.DataTree(data, param['spec'])
    param['data'] = data
    return param

def load_test_dat(test_file, dat_file):
    dat_dir = os.path.dirname(os.path.realpath(test_file))
    return open(os.path.join(dat_dir, dat_file), 'rb').read()
