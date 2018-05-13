#!/usr/bin/env python
#
#    Copyright (c) 2017, Jonathan Gramain <jonathan.gramain@gmail.com>. All
#    rights reserved.
#
#    This file is part of the BitPunch Command Line tool
#
#    The BitPunch Command Line tool is free software: you can
#    redistribute it and/or modify it under the terms of the GNU
#    General Public License as published by the Free Software
#    Foundation, either version 3 of the License, or (at your option)
#    any later version.
#
#    The BitPunch Command Line tool is distributed in the hope that it
#    will be useful, but WITHOUT ANY WARRANTY; without even the
#    implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#    PURPOSE.  See the GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with the BitPunch Command Line tool.  If not, see
#    <http://www.gnu.org/licenses/>.
#

import logging
import os
import errno
import shlex
import json
import argparse
import pprint
import traceback
import hexdump
import sys

from rl import completion, completer

from nestedcmd import NestedCmd, CommandError, MissingSubCommandError
from bitpunch import model

class ArgListParser(argparse.ArgumentParser):

    def __init__(self, command, *args, **kwargs):
        self.command = command
        kwargs['prog'] = command
        super(ArgListParser, self).__init__(*args, **kwargs)
        self.remainder_arg = None

    def add_remainder_argument(self, argname):
        if self.remainder_arg:
            raise ValueError("Only one argument can get the remainder")
        self.remainder_arg = argname


    def parse_line(self, line):
        arglist = shlex.split(line, posix=True)
        if self.remainder_arg:
            args, remainder_args = self.parse_known_args(arglist)
            # redo the parsing but stop the lexer at the remainder this time
            lexer = shlex.shlex(line, posix=True)
            lexer.whitespace_split = True
            for pos in range(len(arglist) - len(remainder_args)):
                lexer.get_token() # throw items away, we have them already
            remainder = lexer.instream.read().strip()
            setattr(args, self.remainder_arg, remainder)
            return args
        else:
            return self.parse_args(arglist)

    def error(self, message):
        raise CommandError(self.command, message)


CONFIG_DIR = '.bitpunch'
HISTORY_FILE_NAME = 'history'

class NoCompletion(Exception): pass

class CLI(NestedCmd):

    LOGLEVELS = {
        'debug': logging.DEBUG,
        'info': logging.INFO,
        'warning': logging.WARNING,
        'error': logging.ERROR,
        'critical': logging.CRITICAL
    }

    def __init__(self):
        self._init_config()
        self.history_file = os.path.join(self.config_dir, HISTORY_FILE_NAME)

        super(CLI, self).__init__()
        self.intro = '*** BitPunch command-line interface ***'
        self.prompt = 'bitpunch> '
        self.expr_operators_delim = (' ', '(', '&')

        self.format_spec = None
        self.format_spec_path = None
        self.bin_file = None
        self.data_tree = None

    def _init_config(self):
        home_dir = os.path.expanduser('~')
        self.config_dir = os.path.join(home_dir, CONFIG_DIR)
        try:
            os.makedirs(self.config_dir)
        except OSError as e:
            if e.errno != errno.EEXIST:
                raise

    def preloop(self):
        super(CLI, self).preloop()
        completer.word_break_characters += '&'

    def open_data_tree(self, cmdname):
        if self.data_tree is not None:
            return
        if self.format_spec is None:
            raise CommandError(cmdname,
                               'missing format specification file '
                               '(see "file" command)')
        if self.bin_file is None:
            raise CommandError(cmdname,
                               'missing binary data file '
                               '(see "file" command)')
        self.data_tree = model.DataTree(self.bin_file, self.format_spec)

    def _complete_expression(self, text, begin, end,
                             ignore_primitive_types=False):
        try:
            for item in self._complete_expression_nocatch(text, begin, end,
                                                          ignore_primitive_types):
                yield item
        except Exception as e:
            exc_str = traceback.format_exc()
            logging.debug(exc_str)

    def _complete_expression_nocatch(self, text, begin, end,
                                     ignore_primitive_types=False):
        # do not append a final ' ' automatically if only one possible
        # completion
        completion.suppress_append = True

        def extract_trailing_dpath_expr(text):
            if text and text[-1] == ')':
                raise NoCompletion()

            min_boundary = -1
            boundaries = [text.rfind(boundary)
                          for boundary in self.expr_operators_delim]
            for boundary in boundaries:
                if (boundary != -1 and
                    (min_boundary == -1 or boundary > min_boundary)):
                    min_boundary = boundary
            if min_boundary != -1:
                expr = text[min_boundary+1:]
            else:
                expr = text
            return expr

        def extract_right_side_item(partial_expr):
            if not partial_expr:
                return ''

            for char_pos in range(len(partial_expr) - 1, -1, -1):
                item_char = partial_expr[char_pos]
                if not (item_char.isalnum() or item_char in "_?.[:]\"'{}-+/*"):
                    if item_char not in (tuple(self.expr_operators_delim)):
                        raise ValueError()
                    item_pos = char_pos + 1
                    return partial_expr[item_pos:]
            else:
                return partial_expr

        def filter_type(obj, key):
            if not ignore_primitive_types:
                return False
            if isinstance(obj, model.DataBlock):
                child_obj = getattr(obj, key)
            else:
                child_obj = obj[key]
            return (not isinstance(child_obj, model.DataBlock)
                    and not isinstance(child_obj, model.DataArray))

        def build_completion(candidate, obj, completion_base):
            if isinstance(obj, model.DataBlock):
                return candidate
            elif isinstance(candidate, int):
                return str(candidate)
            elif completion_base.startswith('"'):
                return candidate.str_double_quoted()
            else:
                return candidate.str_single_quoted()

        def rightmost_sep(s, seps):
            rightmost_idx = max(s.rfind(sep) for sep in seps)
            if rightmost_idx == -1:
                return None
            return s[rightmost_idx]

        try:
            expr = extract_trailing_dpath_expr(text[:end])
        except NoCompletion:
            return

        if self.format_spec and self.bin_file:
            self.open_data_tree('(complete expression)')

        #FIXME: not working with slices
        if expr.endswith(']'):
            item = extract_right_side_item(expr)
            item_complement = ''
            sep = '.'
            sep_end = ''
            completion_base = ''
        else:
            item_complement = ''
            sep = rightmost_sep(expr, ".[")
            sep_end = ''
            if not sep:
                sep = '.'
            left, sep, right = expr.rpartition(sep)
            if sep == '[':
                sep2 = rightmost_sep(right, ':')
                if sep2:
                    sep = ':'
                    item_complement, throw, right = right.rpartition(sep2)
                    item_complement = '[' + item_complement
            if sep in ('[', ':'):
                sep_end = ']'
            if (not left or left.endswith(' ')) and sep != '':
                return
            if all(c not in right
                   for c in (tuple(self.expr_operators_delim)
                             + (')',))):
                item = extract_right_side_item(left)
                completion_base = right
            else:
                item = ''
                sep = ''
                sep_end = ''
                completion_base = extract_right_side_item(right)

        logging.debug('complete_expression text=%s expr=%s begin=%d end=%d item=%s item_complement=%s sep=%s sep_end=%s completion_base=%s'
                      % (repr(text), repr(expr), begin, end,
                         repr(item), repr(item_complement),
                         repr(sep), repr(sep_end),
                         repr(completion_base)))
        if item:
            obj = self.data_tree.eval_expr(item)
            if not (isinstance(obj, model.DataBlock) and sep == '.' or
                    isinstance(obj, model.DataArray) and sep in ('[', ':')):
                return
            item += item_complement
        else:
            if 'file'.startswith(completion_base):
                yield 'file'
            obj = None
            if 'sizeof'.startswith(completion_base):
                yield 'sizeof('

        for builtin in model.get_builtin_names(prefix=completion_base,
                                                 object=obj):
            yield builtin + '('

        if obj is None:
            obj = self.data_tree
            if obj is None:
                return

        nb_found_keys = 0
        first_key = None
        for key in model.Tracker(obj, iter_mode=model.Tracker.ITER_MEMBER_NAMES):
            if not filter_type(obj, key):
                key_str = build_completion(key, obj, completion_base)
                if key_str.startswith(completion_base):
                    if nb_found_keys == 0:
                        first_key = key
                        first_key_str = key_str
                    else:
                        if nb_found_keys == 1:
                            yield item + sep + first_key_str + sep_end
                        yield item + sep + key_str + sep_end
                    nb_found_keys += 1


        if nb_found_keys == 1:

            try:
                if isinstance(obj, model.DataBlock):
                    child_obj = getattr(obj, first_key)
                else:
                    child_obj = obj[first_key]
                if (isinstance(child_obj, model.DataBlock) or
                    isinstance(child_obj, model.DataArray)):

                    if str(first_key) != completion_base or sep_end:
                        key_str = build_completion(first_key, obj,
                                                   completion_base)
                        yield item + sep + key_str + sep_end
                        return

                    for child_key in model.Tracker(
                            child_obj,
                            iter_mode=model.Tracker.ITER_MEMBER_NAMES):
                        if not filter_type(child_obj, child_key):
                            if isinstance(child_obj, model.DataBlock):
                                yield expr + sep_end + '.' + child_key
                            else:
                                key_str = build_completion(child_key,
                                                           child_obj,
                                                           completion_base)
                                yield expr + sep_end + '[' + key_str + ']'

                elif first_key_str != completion_base or sep_end:
                        yield item + sep + first_key_str + sep_end + ' '
                        return
            except Exception:
                yield item + sep + first_key_str + sep_end


    ### COMMANDS ###

    def parse_dump_args(self, cmd, args):
        parser = ArgListParser(cmd)
        parser.add_argument('output_file', type=str,
                            help='path to output file')
        parser.add_remainder_argument('expression')
        pargs = parser.parse_line(args)
        logging.debug('pargs=%s', repr(pargs))
        logging.debug('filepath=%s expression=%s',
                      repr(pargs.output_file), repr(pargs.expression))
        if not pargs.expression:
            parser.error('missing expression')
        return pargs

    def _complete_dump(self, text, begin, end):
        nargs = 0
        try:
            lexer = shlex.shlex(text[:end], posix=True)
            lexer.whitespace_split = True
            while lexer.instream.tell() < begin:
                lexer.get_token()
                nargs += 1
        except ValueError:
            pass
        logging.debug('_complete_dump text=%s begin=%d end=%d nargs=%d',
                      repr(text), begin, end, nargs)
        if nargs == 0:
            return self.complete_filename(text[begin:end])
        elif nargs == 1:
            return self._complete_expression(text[begin:], 0, end - begin)
        else:
            return []

    def do_dump_json(self, args):
        """Dump the value of an expression in JSON format

    Usage: dump json <output_file> <expression>
"""
        pargs = self.parse_dump_args('dump json', args)
        if self.format_spec and self.bin_file:
            self.open_data_tree('dump json')
        dump_obj = self.data_tree.eval_expr(pargs.expression)
        dump_obj = model.make_python_object(dump_obj) # required for JSON serialization
        with open(pargs.output_file, 'wxb') as output_file:
            json.dump(dump_obj, output_file, encoding='iso8859-1', indent=4)

    def complete_dump_json(self, text, begin, end):
        logging.debug('complete_dump_json text=%s begin=%d end=%d'
                      % (repr(text), begin, end))
        return self._complete_dump(text, begin, end)


    def do_dump_python(self, args):
        """Dump the value of an expression represented as a python object

    Usage: dump python <output_file> <expression>
"""
        pargs = self.parse_dump_args('dump python', args)
        if self.format_spec and self.bin_file:
            self.open_data_tree('dump python')
        dump_obj = self.data_tree.eval_expr(pargs.expression)
        # not strictly necessary, but enables pretty-printing
        dump_obj = model.make_python_object(dump_obj)
        with open(pargs.output_file, 'wxb') as output_file:
            pprint.pprint(dump_obj, output_file)

    def complete_dump_python(self, text, begin, end):
        logging.debug('complete_dump_python text=%s begin=%d end=%d'
                      % (repr(text), begin, end))
        return self._complete_dump(text, begin, end)


    def do_dump_raw(self, args):
        """Dump the raw contents of a dpath expression target

    Usage: dump raw <output_file> <dpath_expression>
"""
        pargs = self.parse_dump_args('dump raw', args)
        self.open_data_tree('dump raw')
        obj = self.data_tree.eval_expr(pargs.expression)
        logging.info("dump_raw dpath=%s", repr(pargs.expression))
        with open(pargs.output_file, 'wxb') as output_file:
            output_file.write(obj)

    def complete_dump_raw(self, text, begin, end):
        logging.debug('complete_dump_raw text=%s begin=%d end=%d'
                      % (repr(text), begin, end))
        return self._complete_dump(text, begin, end)


    def do_file(self, args):
        """Load a binary data file or show information about loaded files

    Usage:

    file <filename> [format_file_path]        
        Load a binary file along with its format file. If
        format_file_path is not given, try to find one matching the
        file extension.

    file
        Show information about current binary file and associated
        format file

        """
        parser = ArgListParser('file')
        parser.add_argument('filepath', type=str,
                            help='path to definition file',
                            nargs='?', default=None)
        parser.add_argument('bppath', type=str,
                            help='path to format file',
                            nargs='?', default=None)
        pargs = parser.parse_args(shlex.split(args, comments=True))

        self.load_file(pargs.filepath, pargs.bppath)


    def load_file(self, data_path=None, bp_path=None):
        if bp_path:
            self.format_spec_path = os.path.expanduser(bp_path)
            self.format_spec = model.FormatSpec(open(self.format_spec_path, 'r'))
        if data_path:
            filepath = os.path.expanduser(data_path)
            new_bin_file = open(filepath, 'r')
            if self.bin_file:
                self.bin_file.close()
            self.bin_file = new_bin_file
            self.data_tree = None
            try:
                extension = filepath[filepath.rindex('.') + 1:].lower()
                format_handler = model.find_format_as_stream(extension)
                self.format_spec = model.FormatSpec(format_handler)
                self.format_spec_path = format_handler.name;
            except Exception as e:
                pass
        else:
            print('file   -> {bin}\n'
                  'format -> {fmt}'
                  .format(bin=self.bin_file.name if self.bin_file
                          else '<not loaded>',
                          fmt=self.format_spec_path if self.format_spec_path
                          else '<not loaded>'));

    def attach_data_tree(self, data_tree):
        self.data_tree = data_tree

    def complete_file(self, text, begin, end):
        logging.debug('complete_file text=%s begin=%d end=%d'
                      % (repr(text), begin, end))
        return self.complete_filename(text[begin:end])


    def do_keys(self, args):
        """List keys of an object

    Usage: keys [<expression>]
"""
        expr = args
        if self.format_spec and self.bin_file:
            self.open_data_tree('keys')
        obj = self.data_tree.eval_expr(expr) if expr else self.data_tree
        if (isinstance(obj, model.DataBlock) or
            isinstance(obj, model.DataArray)):
            keys = list(str(key) for key in model.Tracker(
                obj, iter_mode=model.Tracker.ITER_MEMBER_NAMES))
            self.columnize(keys)
        else:
            raise CommandError('keys',
                               'cannot list keys on object of type %s'
                               % type(obj))

    def complete_keys(self, text, begin, end):
        return self._complete_expression(text, begin, end, True)


    def do_print(self, args):
        """Print the value of an expression

    Usage: print <expression>
"""
        expr = args
        if not expr:
            raise CommandError('print',
                               "missing expression argument to 'print'")
        if self.format_spec and self.bin_file:
            self.open_data_tree('print')
        print repr(model.make_python_object(self.data_tree.eval_expr(expr)))

    def complete_print(self, text, begin, end):
        return self._complete_expression(text, begin, end)


    def do_xdump(self, args):
        """Print the value of a dpath expression in hexadecimal + ascii
        format, 16 bytes per line

    Usage: xdump <expression>
"""
        expr = args
        if not expr:
            raise CommandError('xdump',
                               "missing expression argument to 'xdump'")
        self.open_data_tree('xdump')
        obj = self.data_tree.eval_expr(expr, require_dpath=True)
        try:
            memview = memoryview(obj)
            lines = hexdump.hexdump(memview, result='generator')
            abs_offset = obj.get_offset()
            for line in lines:
                print('%08X +%s' % (abs_offset, line))
                abs_offset += 16
        except TypeError:
            raise TypeError("cannot hexdump expression: "
                            "not a dpath expression")

    def complete_xdump(self, text, begin, end):
        return self._complete_expression(text, begin, end)


    def do_set(self, args):
        """Set various configuration entries

    Usage: set <key> <value>
"""
        raise MissingSubCommandError('set')

    def do_set_loglevel(self, value):
        """Set the logging level

    Usage: set loglevel (critical|error|warning|info|debug)
"""
        if not value:
            raise CommandError('set loglevel', 'missing argument')

        logger = logging.getLogger()
        loglevel = value.lower()
        if loglevel in CLI.LOGLEVELS:
            logger.setLevel(CLI.LOGLEVELS[loglevel])
        else:
            raise CommandError('set loglevel',
                               'invalid log level "%s"' % loglevel)
        self.stdout.write('log level set to "%s"\n' % loglevel)

    def complete_set_loglevel(self, text, *ignored):
        logging.debug('complete_set_loglevel text="%s"' % (text))
        return [level for level in CLI.LOGLEVELS.keys()
                if level.startswith(text)]


if __name__ == '__main__':
    cli = CLI()
    if len(sys.argv) > 1:
        print "loading binary file " + repr(sys.argv[1])
        cli.load_file(sys.argv[1], bp_path=None)
    cli.cmdloop()
