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
from ruamel.yaml import YAML
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
        self.expr_operators_delim = (' ', '(', ')', '&', '<>')

        self.format_spec = None
        self.format_spec_path = None
        self.bin_file = None
        self.data_tree = None
        self.board = model.Board()

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
        # completion, as we want to do it only for leaf items
        completion.suppress_append = True

        def extract_active_dpath_expr(text):
            rightmost_boundary_index = None
            rightmost_boundary = None
            for boundary in self.expr_operators_delim:
                boundary_index = text.rfind(boundary)
                if (boundary_index != -1 and
                    (rightmost_boundary_index is None or
                     boundary_index > rightmost_boundary_index)):
                    rightmost_boundary_index = boundary_index
                    rightmost_boundary = boundary
            if rightmost_boundary_index is not None:
                if rightmost_boundary != ')':
                    return text[rightmost_boundary_index+len(rightmost_boundary):]
            else:
                return text

            extra_closing_parenthesis = 1
            limit = rightmost_boundary_index
            while True:
                parenthesis_index = max(text[:limit].rfind('('),
                                        text[:limit].rfind(')'))
                if parenthesis_index == -1:
                    return '' # unbalanced parenthesis: cannot complete
                extra_closing_parenthesis += \
                    text[parenthesis_index] == ')' and 1 or -1
                if extra_closing_parenthesis == 0:
                    return text[parenthesis_index:]
                limit = parenthesis_index
            return ''

        def is_identifier(key):
            str_key = str(key)
            return ((str_key[0].isalpha() or str_key[0] in ['_', '?']) and
                    all(c.isalnum() or c == '_' for c in str_key[1:]))

        def is_attribute(key):
            str_key = str(key)
            return len(str_key) > 0 and str_key[0] == '@'

        def filter_type(obj, key):
            if not ignore_primitive_types:
                return False
            try:
                child_obj = obj[key]
                return (child_obj.get_filter_type() not in ['composite', 'array'])
            except Exception:
                return True

        def build_completion(candidate, obj, completion_base):
            if (obj.get_filter_type() == 'composite' or
                isinstance(candidate, str)):
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

        expr = extract_active_dpath_expr(text[:end])

        if self.format_spec and self.bin_file:
            self.open_data_tree('(complete expression)')

        #FIXME: not working with slices
        if expr.endswith(']'):
            item = expr
            item_complement = ''
            sep = '.'
            sep_end = ''
            completion_base = ''
        else:
            item_complement = ''
            sep = rightmost_sep(expr, ".[)")
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
            if sep == ')':
                left += ')' # put back the missing parenthesis to make
                            # a valid expression
                sep = '.'
            if (not left or left.endswith(' ')) and sep != '':
                return
            item = left
            completion_base = right

        completion_prefix = text[begin:end-len(expr)]
        logging.debug('complete_expression text=%s expr=%s begin=%d end=%d item=%s item_complement=%s sep=%s sep_end=%s completion_base=%s'
                      % (repr(text), repr(expr), begin, end,
                         repr(item), repr(item_complement),
                         repr(sep), repr(sep_end),
                         repr(completion_base)))
        if item:
            obj = self.data_tree.eval_expr(item)
            if not (obj.get_filter_type() == 'composite' and sep == '.' or
                    obj.get_filter_type() == 'array' and sep in ('.', '[', ':')):
                return
            item += item_complement
        else:
            obj = None
            if 'sizeof'.startswith(completion_base):
                yield completion_prefix + 'sizeof('

            for builtin in model.get_builtin_names(prefix=completion_base):
                yield completion_prefix + builtin + '('

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
                        def yield_key(key):
                            dottable = is_identifier(key) or is_attribute(key)
                            return ((sep != '.' or dottable)
                                    and (sep != '[' or not dottable))
                        if nb_found_keys == 1:
                            if yield_key(first_key_str):
                                yield completion_prefix + item + sep + first_key_str + sep_end
                        if yield_key(key_str):
                            yield completion_prefix + item + sep + key_str + sep_end
                    nb_found_keys += 1


        if nb_found_keys == 1:

            try:
                child_obj = obj[first_key]
                if (child_obj.get_filter_type() == 'composite' or
                    child_obj.get_filter_type() == 'array'):

                    if str(first_key) != completion_base or sep_end:
                        key_str = build_completion(first_key, obj,
                                                   completion_base)
                        if (is_attribute(key_str)):
                            yield completion_prefix + item + '.' + key_str
                        else:
                            yield completion_prefix + item + sep + key_str + sep_end
                        return

                    for child_key in model.Tracker(
                            child_obj,
                            iter_mode=model.Tracker.ITER_MEMBER_NAMES):
                        if not filter_type(child_obj, child_key):
                            if (is_attribute(child_key) or
                                child_obj.get_filter_type() == 'composite'):
                                yield completion_prefix + expr + sep_end + '.' + child_key
                            else:
                                key_str = build_completion(child_key,
                                                           child_obj,
                                                           completion_base)
                                yield completion_prefix + expr + sep_end + '[' + key_str + ']'

                elif first_key_str != completion_base or sep_end:
                        yield completion_prefix + item + sep + first_key_str + sep_end + ' '
                        return
            except Exception:
                yield completion_prefix + item + sep + first_key_str + sep_end


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

    def _do_dump_generic(self, args, dump_type, dump_func):
        pargs = self.parse_dump_args('dump {0}'.format(dump_type), args)
        if self.format_spec and self.bin_file:
            self.open_data_tree('dump {0}'.format(dump_type))
        dump_obj = self.data_tree.eval_expr(pargs.expression)
        if pargs.output_file == '-':
            dump_func(dump_obj, sys.stdout)
        else:
            try:
                with open(pargs.output_file, 'wxb') as output_file:
                    dump_func(dump_obj, output_file)
            except IOError as e:
                if e.errno != errno.EEXIST:
                    raise e
                sys.stderr.write('File {0} exists, replace? (y/N) '
                                 .format(pargs.output_file))
                answer = sys.stdin.readline().strip()
                if answer.lower() in ['y', 'yes']:
                    with open(pargs.output_file, 'wb') as output_file:
                        dump_func(dump_obj, output_file)

    def do_dump_json(self, args):
        """Dump the value of an expression in JSON format

    Usage: dump json <output_file> <expression>
"""
        def dump_json_to_file(obj, out):
            # required for JSON serialization
            deep_obj = model.make_python_object(obj)
            json.dump(deep_obj, out, encoding='iso8859-1', indent=4)
            out.write('\n')

        self._do_dump_generic(args, 'json', dump_json_to_file)

    def complete_dump_json(self, text, begin, end):
        logging.debug('complete_dump_json text=%s begin=%d end=%d'
                      % (repr(text), begin, end))
        return self._complete_dump(text, begin, end)


    def do_dump_python(self, args):
        """Dump the value of an expression represented as a python object

    Usage: dump python <output_file> <expression>
"""
        def dump_python_to_file(obj, out):
            # not strictly necessary, but enables pretty-printing
            deep_obj = model.make_python_object(obj)
            pprint.pprint(deep_obj, out)

        self._do_dump_generic(args, 'python', dump_python_to_file)

    def complete_dump_python(self, text, begin, end):
        logging.debug('complete_dump_python text=%s begin=%d end=%d'
                      % (repr(text), begin, end))
        return self._complete_dump(text, begin, end)


    def do_dump_yaml(self, args):
        """Dump the value of an expression in YAML format

    Usage: dump yaml <output_file> <expression>
"""
        def dump_yaml_to_file(obj, out):
            # required for YAML serialization
            deep_obj = model.make_python_object(obj)
            yaml = YAML()
            yaml.default_flow_style = False
            yaml.dump(deep_obj, out)

        self._do_dump_generic(args, 'yaml', dump_yaml_to_file)

    def complete_dump_yaml(self, text, begin, end):
        logging.debug('complete_dump_yaml text=%s begin=%d end=%d'
                      % (repr(text), begin, end))
        return self._complete_dump(text, begin, end)


    def do_dump_raw(self, args):
        """Dump the raw contents of a dpath expression target

    Usage: dump raw <output_file> <dpath_expression>
"""
        def dump_raw_to_file(obj, out):
            buf = bytearray(memoryview(obj))
            out.write(buf)

        self._do_dump_generic(args, 'raw', dump_raw_to_file)

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
            format_spec_path = os.path.expanduser(bp_path)
            self.format_spec = model.FormatSpec(open(format_spec_path, 'r'))
            self.format_spec_path = format_spec_path
        if data_path:
            filepath = os.path.expanduser(data_path)
            new_bin_file = open(filepath, 'r')
            if not bp_path:
                extension = filepath[filepath.rindex('.') + 1:]
                format_handler = model.find_handler(extension=extension)
                if not format_handler:
                    new_bin_file.close()
                    raise CommandError(
                        'file', 'no handler found by file extension, please ' +
                        'provide path to bitpunch model as second argument')
                self.format_spec = model.FormatSpec(format_handler)
                self.format_spec_path = format_handler.name;
            if self.bin_file:
                self.bin_file.close()
            self.bin_file = new_bin_file
            self.data_tree = None
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


    def do_let(self, args):
        """Attach an expression to a local name

    Usage:

    let <name> <expression>

        """
        name, expr = args.split(' ', 1)
        if not name:
            raise CommandError('let',
                               "missing expression name argument to 'let'")
        if not expr:
            raise CommandError('let',
                               "missing expression argument to 'let'")
        self.board.add_spec(name, expr)


    def do_list(self, args):
        """List attributes of an object

    Usage: list [<expression>]
"""
        expr = args
        if self.format_spec and self.bin_file:
            self.open_data_tree('list')
        obj = self.data_tree.eval_expr(expr) if expr else self.data_tree
        keys = list(str(key) for key in model.Tracker(
            obj, iter_mode=model.Tracker.ITER_MEMBER_NAMES))
        self.columnize(keys)

    def complete_list(self, text, begin, end):
        return self._complete_expression(text, begin, end, True)


    def do_print(self, args):
        """Print the value of an expression

    Usage: print <expression>
"""
        expr = args
        if not expr:
            raise CommandError('print',
                               "missing expression argument to 'print'")
        #if self.format_spec and self.bin_file:
        #    self.open_data_tree('print')
        expr_value = self.board.eval_expr(expr)
        print repr(model.make_python_object(expr_value))

    def complete_print(self, text, begin, end):
        return self._complete_expression(text, begin, end)


    def do_xdump(self, args):
        """Print the raw contents of a dpath expression in hexadecimal + ascii
        format, 16 bytes per line

    Usage: xdump <expression>
"""
        expr = args
        if not expr:
            raise CommandError('xdump',
                               "missing expression argument to 'xdump'")
        self.open_data_tree('xdump')
        obj = self.data_tree.eval_expr(expr)
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
