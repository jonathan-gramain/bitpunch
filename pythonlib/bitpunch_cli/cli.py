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

def is_identifier(key):
    str_key = str(key)
    return ((str_key[0].isalpha() or str_key[0] in ['_', '?']) and
            all(c.isalnum() or c == '_' for c in str_key[1:]))

def is_attribute(key):
    str_key = str(key)
    return len(str_key) > 0 and str_key[0] == '@'

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
        self.board = model.Board()
        self.using_spec_file = None

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

        def filter_type(obj, key):
            if (not ignore_primitive_types or
                isinstance(obj, model.Board) or
                isinstance(obj, model.SpecNode)):
                return False
            try:
                child_obj = obj[key]
                return (child_obj.get_filter_type() not in
                        ['struct', 'union', 'array'])
            except Exception:
                return True

        def build_completion(candidate, obj, completion_base):
            if (isinstance(candidate, str) or
                isinstance(candidate, model.SpecNode) or
                obj.get_filter_type() in ['struct', 'union']):
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

        def iter_members(obj):
            if isinstance(obj, model.SpecNode):
                return iter(obj)
            else:
                return model.Tracker(
                    obj, iter_mode=model.Tracker.ITER_MEMBER_NAMES)

        expr = extract_active_dpath_expr(text[:end])

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
            obj = self.board.eval_expr(item)
            if not ((isinstance(obj, model.SpecNode) or
                     obj.get_filter_type() in ['struct', 'union']) and sep == '.' or
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
            obj = self.board.get_spec()
            if obj is None:
                return
        key_source = iter_members(obj)

        nb_found_keys = 0
        first_key = None
        for key in key_source:
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
                if (isinstance(child_obj, model.SpecNode) or
                    child_obj.get_filter_type() in ['struct', 'union', 'array']):

                    if str(first_key) != completion_base or sep_end:
                        key_str = build_completion(first_key, obj,
                                                   completion_base)
                        if (is_attribute(key_str)):
                            yield completion_prefix + item + '.' + key_str
                        else:
                            yield completion_prefix + item + sep + key_str + sep_end
                        return

                    key_source = iter_members(child_obj)
                    for child_key in key_source:
                        if not filter_type(child_obj, child_key):
                            if (is_attribute(child_key) or
                                isinstance(child_obj, model.SpecNode) or
                                child_obj.get_filter_type() in ['struct', 'union']):
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

    def do_func_with_exceptions(do_func):
        def do_func_with_exception_handler(*args, **kwargs):
            try:
                do_func(*args, **kwargs)
            except model.DataError, e:
                err = e.args[0]
                print err['description']
        do_func_with_exception_handler.__doc__ = do_func.__doc__
        return do_func_with_exception_handler

    def attach_board(self, board):
        self.board = board

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

    @do_func_with_exceptions
    def _do_dump_generic(self, args, dump_type, dump_func):
        pargs = self.parse_dump_args('dump {0}'.format(dump_type), args)
        dump_obj = self.board.eval_expr(pargs.expression)
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


    def parse_import_args(self, args):
        parser = ArgListParser('import')
        parser.add_argument('name', type=str,
                            help='local name to associate to the import')
        parser.add_argument('spec_file', type=str,
                            help='path to specification file')
        pargs = parser.parse_line(args)
        if not is_identifier(pargs.name):
            raise CommandError('import',
                               'import name "{0}" is not a valid identifier'
                               .format(pargs.name))
        logging.debug('pargs=%s', repr(pargs))
        logging.debug('name=%s', repr(pargs.name))
        logging.debug('spec_file=%s', repr(pargs.spec_file))
        return pargs

    def import_spec_from_file(self, name, path):
        self.board.remove(name)
        self.board.add_spec(name=name, path=os.path.expanduser(path))

    @do_func_with_exceptions
    def do_import(self, args):
        """Import a specification file into the board under a local name

    Usage:

    import <name> <file_path>

        """
        pargs = self.parse_import_args(args)
        self.import_spec_from_file(pargs.name, pargs.spec_file)

    def complete_import(self, text, begin, end):
        logging.debug('complete_import text=%s begin=%d end=%d'
                      % (repr(text), begin, end))
        if text[:end].find(' ') != -1:
            return self.complete_filename(text[begin:end])


    def parse_use_args(self, args):
        parser = ArgListParser('use')
        parser.add_argument('spec_file', type=str,
                            help='path to specification file',
                            nargs='?', default=None)
        pargs = parser.parse_line(args)
        logging.debug('pargs=%s', repr(pargs))
        logging.debug('spec_file=%s', repr(pargs.spec_file))
        return pargs

    def use_spec_from_file(self, path):
        self.board.use_spec(path=os.path.expanduser(path))

    @do_func_with_exceptions
    def do_use(self, args):
        """Import all top-level names from a specification file

        Import all names defined in a specification file at top level
        into the board

    Usage:

    use <file_path>

        """
        pargs = self.parse_use_args(args)
        if pargs.spec_file:
            self.using_spec_file = pargs.spec_file
            self.use_spec_from_file(pargs.spec_file)
        else:
            if self.using_spec_file:
                print '  using: {0}'.format(self.using_spec_file)
            else:
                print '  no specification file in use'

    def complete_use(self, text, begin, end):
        logging.debug('complete_use text=%s begin=%d end=%d'
                      % (repr(text), begin, end))
        return self.complete_filename(text[begin:end])


    def do_clear(self, args):
        """Clear names set by 'use' command

        Clear names that have been defined by the previous 'use'
        command, but keep the locally-defined (with 'let') names

    Usage:

    clear

        """

        if args.strip():
            raise CommandError('clear', 'extra arguments')

        self.board.forget_spec()


    @do_func_with_exceptions
    def do_let(self, args):
        """Attach an expression to a local name

        Create a local name bound to an expression. The local name
        must be a valid bitpunch identifier.

    Usage:

    let <name> = <expression>

        """
        arglist = args.split('=', 1)
        name, expr = ([arg.strip() for arg in arglist] + [None, None])[:2]
        if not name:
            raise CommandError('let', 'missing expression name')
        if not is_identifier(name):
            raise CommandError(
                'let', 'expression name "{0}" is not a valid identifier'
                .format(name))
        if not expr:
            raise CommandError('let', 'missing expression')

        self.board.remove(name)
        self.board.add_expr(name, expr)

    def complete_let(self, text, begin, end):
        logging.debug('complete_let text=%s begin=%d end=%d'
                      % (repr(text), begin, end))
        if text[:begin].find('=') != -1:
            return self._complete_expression(text, begin, end)
        if text[:begin].find(' ') != -1:
            return ['=']


    def do_unset(self, args):
        """Unset one or more local expression name(s)

        Remove one or more named expression(s) from the board (type
        'list' to see currently defined named expressions).

    Usage:

    unset <name> [<name>...]
        """
        names = [arg.strip() for arg in args.split()]
        if len(names) == 0:
            raise CommandError('unset', 'missing name')
        for name in names:
            if not is_identifier(name):
                raise CommandError(
                    'unset', 'expression name "{0}" is not a valid identifier'
                    .format(name))

            n_removed = self.board.remove(name)
            if n_removed == 0:
                logging.warning('no such name "{0}" exists on board'
                                .format(name))

    def complete_unset(self, text, begin, end):
        logging.debug('complete_unset text=%s begin=%d end=%d'
                      % (repr(text), begin, end))
        completion_prefix = text[begin:end]
        for key in self.board:
            if key.startswith(completion_prefix):
                yield key


    def parse_bind_args(self, args):
        parser = ArgListParser('bind')
        parser.add_argument('name', type=str,
                            help='local name to associate to the bind')
        parser.add_argument('file_path', type=str,
                            help='file path to bind to the local name')
        pargs = parser.parse_line(args)
        if not is_identifier(pargs.name):
            raise CommandError('bind',
                               'bind name "{0}" is not a valid identifier'
                               .format(pargs.name))
        logging.debug('pargs=%s', repr(pargs))
        logging.debug('name=%s', repr(pargs.name))
        logging.debug('file_path=%s', repr(pargs.file_path))
        return pargs

    def bind_data_source_file(self, name, path):
        self.board.remove(name)
        self.board.add_expr(name, "file {{ @path: '{0}'; }}".format(path))

    @do_func_with_exceptions
    def do_bind(self, args):
        """Bind a local data source file as a local name

    The chosen file path then becomes a bitpunch object on the board
    that can be linked to specification objects (with <> operator) for
    inspection.

    Note: this is a convenience function that provides a shorter
    syntax than declaring a file filter explicitly, and provides
    autocompletion of file paths.

    Usage:

    bind <name> <file_path>

        This is equivalent to the following let expression:
        let <name> = file { @path: "<file_path>"; }

        """
        pargs = self.parse_bind_args(args)
        self.bind_data_source_file(pargs.name, pargs.file_path)

    def complete_bind(self, text, begin, end):
        logging.debug('complete_bind text=%s begin=%d end=%d'
                      % (repr(text), begin, end))
        if text[:end].find(' ') != -1:
            return self.complete_filename(text[begin:end])


    @do_func_with_exceptions
    def do_list(self, args):

        """List attributes of an object

    Usage: list [<expression>]
"""
        expr = args
        if expr:
            obj = self.board.eval_expr(expr)
            if isinstance(obj, model.SpecNode):
                keys = list(str(key) for key in obj)
            else:
                keys = list(str(key) for key in model.Tracker(
                    obj, iter_mode=model.Tracker.ITER_MEMBER_NAMES))
        else:
            keys = list(str(key) for key in self.board)
        self.columnize(keys)

    def complete_list(self, text, begin, end):
        return self._complete_expression(text, begin, end, True)


    @do_func_with_exceptions
    def do_print(self, args):
        """Print the value of an expression

    Usage: print <expression>
"""
        expr = args
        if not expr:
            raise CommandError('print',
                               "missing expression argument to 'print'")
        expr_value = self.board.eval_expr(expr)
        print repr(model.make_python_object(expr_value))

    def complete_print(self, text, begin, end):
        logging.debug('complete_print text=%s begin=%d end=%d'
                      % (repr(text), begin, end))
        return self._complete_expression(text, begin, end)


    @do_func_with_exceptions
    def do_xdump(self, args):
        """Print the raw contents of a dpath expression in hexadecimal + ascii
        format, 16 bytes per line

    Usage: xdump <expression>
"""
        expr = args
        if not expr:
            raise CommandError('xdump',
                               "missing expression argument to 'xdump'")
        obj = self.board.eval_expr(expr)
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
