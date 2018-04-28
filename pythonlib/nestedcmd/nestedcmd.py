#
#    Copyright (c) 2017, Jonathan Gramain <jonathan.gramain@gmail.com>. All
#    rights reserved.
#
#    This file is part of the nestedcmd Python package
#
#    The nestedcmd Python package is free software: you can
#    redistribute it and/or modify it under the terms of the GNU
#    General Public License as published by the Free Software
#    Foundation, either version 3 of the License, or (at your option)
#    any later version.
#
#    The nestedcmd Python package is distributed in the hope that it
#    will be useful, but WITHOUT ANY WARRANTY; without even the
#    implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#    PURPOSE.  See the GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with the nestedcmd Python package.  If not, see
#    <http://www.gnu.org/licenses/>.
#

import kmd
import logging
import cmd
import traceback

from rl import completion, completer, print_exc, history
from kmd.completions.filename import FilenameCompletion
from kmd.completions.quoting import backslash_dequote

from commandcompleter import CommandCompleter


class CommandError(Exception):
    def __init__(self, cmd, msg):
        help_msg = (' (type "help %s" for more info)' % cmd if cmd
                    else ' (type "help" for list of available commands)')
        cmd_display = cmd + ': ' if cmd else ''
        super(CommandError, self).__init__(
            '*** ' + cmd_display + msg + help_msg)

class UnknownCommandError(CommandError):
    def __init__(self, base_cmd, unknown_command):
        super(UnknownCommandError, self).__init__(
            base_cmd,
            'unknown %scommand "%s"' % ('sub-' if base_cmd else '',
                                        unknown_command))

class MissingSubCommandError(CommandError):
    def __init__(self, cmd):
        super(MissingSubCommandError, self).__init__(
            cmd, 'expects a sub-command')

class AmbiguousCommandError(CommandError):
    def __init__(self, base_cmd, ambiguous_command):
        super(AmbiguousCommandError, self).__init__(
            base_cmd,
            'ambiguous %scommand "%s"' % ('sub-' if base_cmd else '',
                                          ambiguous_command))

class QuotingFilenameCompletion(FilenameCompletion):

    def __call__(self, text):
        completer.quote_characters = '"\''
        res = super(QuotingFilenameCompletion, self).__call__(text)
        return res

class NestedCmd(kmd.Kmd, object):

    def __init__(self):
        super(NestedCmd, self).__init__()

        self.complete_filename = QuotingFilenameCompletion()

    def __getattr__(self, name):
        # skip special features of getattr(Kmd) that expand attributes
        # to sub-commands which we don't want.
        return super(NestedCmd, self).__getattr__(name)

    def preloop(self):
        super(NestedCmd, self).preloop()
        completer.word_break_characters = ' \t\n()'
        completer.quote_characters = ''

        self.interactive_mode = self.stdin.isatty()

        self.do_commands = CommandCompleter()
        self.help_topics = CommandCompleter()

        #logging.getLogger().setLevel(logging.DEBUG)

        for func in self.get_names():
            if func.startswith('do_'):
                self.do_commands.add_command(func[3:].split('_'))
            elif func.startswith('help_'):
                self.help_topics.add_command(func[5:].split('_'))


    def precmd(self, line):
        return super(NestedCmd, self).precmd(line)

    def postcmd(self, stop, line):
        completer.quote_characters = ''
        return super(NestedCmd, self).postcmd(stop, line)

    # modified code from kmd.Kmd to handle KeyboardInterrupt exception
    # gracefully
    def cmdloop(self, intro=None):
        """Repeatedly issue a prompt, accept input, parse an initial prefix
        off the received input, and dispatch to action methods, passing them
        the remainder of the line as argument.
        """
        self.preloop()
        try:
            if intro is not None:
                self.intro = intro
            if self.intro and self.interactive_mode:
                self.stdout.write(str(self.intro)+"\n")
            stop = None
            while not stop:
                try:
                    if self.cmdqueue:
                        line = self.cmdqueue.pop(0)
                    else:
                        if self.use_rawinput:
                            try:
                                if self.interactive_mode:
                                    line = self.input(self.prompt)
                                else:
                                    line = self.input("")
                            except EOFError:
                                line = 'EOF'
                        else:
                            self.stdout.write(self.prompt)
                            self.stdout.flush()
                            line = self.stdin.readline()
                            if not len(line):
                                line = 'EOF'
                            else:
                                line = line.rstrip('\r\n')
                    line = self.precmd(line)
                    stop = self.onecmd(line)
                    stop = self.postcmd(stop, line)
                except KeyboardInterrupt:
                    self.stdout.write('\n') # reset prompt
        finally:
            self.postloop()

    def onecmd(self, line):
        try:
            if line == 'EOF':
                if self.interactive_mode:
                    self.stdout.write('\n')
                return True # leave the prompt
            if not line:
                return self.emptyline()
            if line[0] == '#':
                return self.comment(line)

            arglist = line.split()
            seeklist = list(arglist)
            command_path = None
            cmd_func = None

            subcmds, command_path = \
                    self.do_commands.get_matching_subcommands_by_prefixes(
                        seeklist)
            
            logging.debug('command_path=%s subcmds=%s seeklist=%s',
                          repr(command_path), repr(subcmds), repr(seeklist))

            if len(seeklist) > len(command_path) and len(subcmds) > 1:
                raise AmbiguousCommandError(' '.join(command_path),
                                            seeklist[len(command_path)])

            if not command_path:
                longest_path = \
                    self.do_commands.get_longest_unambiguous_command_prefix(
                        seeklist, ignore_trailing_unknown=True)
                logging.debug('longest_path=%s', repr(longest_path))
                raise UnknownCommandError(' '.join(longest_path),
                                          seeklist[len(longest_path)])

            try:
                cmd_func = getattr(self, 'do_' + '_'.join(command_path))
            except AttributeError:
                pass

            if cmd_func:
                logging.debug('command_path=%s', repr(command_path))
                arglist = line.split(None, len(command_path))
                args = (arglist[len(command_path)].rstrip()
                        if len(arglist) > len(command_path) else '')
                logging.debug('calling %s with args %s',
                              repr(cmd_func), repr(args))
                return cmd_func(args)

            raise MissingSubCommandError(' '.join(command_path))

        except CommandError as err:
            self.stdout.write(err.message + '\n')
        except EnvironmentError as err:
            self.stdout.write(str(err) + '\n')
        except Exception as err:
            logging.debug("Caught an exception %s", repr(err))
            self.stdout.write(traceback.format_exc())



    def do_help(self, args):
        """List available commands with "help"
    List subcommands of a particular <command> with "help <command>"
"""

        self.show_command_help(*args.split())

    def show_command_help(self, *arglist):
        logging.debug('show help on %s', repr(arglist))
        seeklist = list(arglist)
        help_func = None
        cmd_func = None
        cmd_doc = None

        matching_help_topics, matching_subcommands = \
            (self.help_topics.filter_by_subcommand_prefixes(seeklist),
             self.do_commands.filter_by_subcommand_prefixes(seeklist))

        help_path, command_path = \
            (matching_help_topics.get_common_prefix()[:len(seeklist)],
             matching_subcommands.get_common_prefix()[:len(seeklist)])

        if len(help_path) < len(seeklist) - 1:
            help_topics = []
        else:
            help_topics = matching_help_topics.find_subcommands(help_path)

        if len(command_path) < len(seeklist) - 1:
            subcommands = []
        else:
            subcommands = matching_subcommands.find_subcommands(command_path)

        ambiguous = len(set(help_topics) | set(subcommands)) > 1

        logging.debug('help matching_help_topics=%s help_path=%s help_topics=%s',
                      matching_help_topics, repr(help_path),
                      repr(help_topics))
        logging.debug('help matching_subcommands=%s command_path=%s subcommands=%s',
                      matching_subcommands, repr(command_path),
                      repr(subcommands))

        if len(help_path) == len(seeklist) and not ambiguous:
            try:
                help_func = getattr(self, 'help_' + '_'.join(help_path))
                self.stdout.write('*** Help on topic: %s\n\n'
                                  % ' '.join(help_path))
                help_func()
                self.stdout.write('\n')
            except AttributeError:
                pass

        if len(command_path) == len(seeklist) and not ambiguous:
            try:
                cmd_func = getattr(self, 'do_' + '_'.join(command_path))
                cmd_doc = cmd_func.__doc__
                self.stdout.write('*** Help on command: %s\n\n'
                                  % ' '.join(command_path))
                self.stdout.write('    %s\n\n' % str(cmd_doc))
            except AttributeError:
                pass


        def dump_sub(path, namelist, func_prefix, kind):

            prefix = '_'.join([func_prefix] + path) + '_'
            if namelist:
                if path:
                    self.stdout.write('\n*** Matching sub-%s:\n\n' % kind)
                else:
                    self.stdout.write('\n*** Matching %s:\n\n' % kind)

                for name in namelist:
                    full_name = ' '.join(path) + ' ' + name
                    try:
                        doc = getattr(self, prefix + name).__doc__
                        summary = doc.split('\n', 1)[0]
                        self.stdout.write('    ' + full_name
                                          + ': ' + summary + '\n')
                    except AttributeError:
                        self.stdout.write('    ' + full_name + '\n')
                self.stdout.write('\n')
            return namelist

        if help_topics:
            dump_sub(help_path, help_topics, 'help', 'topics')

        if subcommands:
            dump_sub(command_path, subcommands, 'do', 'commands')

        if not help_func and not cmd_doc and not subcommands:
            longest_path = \
                self.do_commands.get_longest_unambiguous_command_prefix(
                    seeklist,
                    ignore_trailing_unknown=True)

            logging.debug('longest_path=%s', repr(longest_path))

            self.stdout.write(
                '*** No command or help topic matching "%s" '
                '(type "%s" to see available sub-commands and topics)\n'
                % (' '.join(seeklist),
                   ' '.join(['help'] + longest_path)))


    def do_history(self, args):
        """Show history of commands

    usage: history [max_entries]

    max_entries: max number of most recent entries to display (default 50)
"""

        args_array = args.split(None, 1)
        if len(args_array) > 1:
            raise CommandError('history', 'too many arguments')
        total_entries = len(history)
        try:
            max_entries = int(args_array[0]) if args_array else 50
        except ValueError:
            raise CommandError('history', 'max_entries is not an integer')
        n_to_skip = max(total_entries - max_entries, 0)
        index_to_display = n_to_skip + 1
        for history_item in history:
            if n_to_skip > 0:
                n_to_skip -= 1
                continue
            print('{i:5}  {item}'.format(i=index_to_display,
                                         item=history_item))
            index_to_display += 1

    def complete(self, text, state):
        """Return the next possible completion for 'text'.

        If a command has not been entered, then complete against command list.
        Otherwise try to call complete_<command> to get list of completions.
        """
        if state == 0:
            origline = completion.line_buffer
            line = origline.lstrip()
            stripped = len(origline) - len(line)
            begidx = completion.begidx - stripped
            endidx = completion.endidx - stripped
            if begidx < 0: # completion requested before first line character
                begidx, endidx = 0, 0
                line = ''
            self.completion_matches = iter(self.completenames(text, line, begidx, endidx))
        try:
            return self.completion_matches.next()
        except StopIteration:
            return None

    def completenames(self, text, line, begin, end):
        logging.debug('completenames line=%s text=%s begin=%d end=%d',
                      repr(line), repr(text), begin, end)
        completer.quote_characters = ''
        arglist = line[:end].split()
        if end and line[end - 1] != ' ':
            arglist.pop() # drop incomplete last argument

        matching_funcs = \
                self.do_commands.filter_by_subcommand_prefixes(
                    arglist)
        completer_prefix = matching_funcs.get_common_prefix()[:len(arglist)]
        logging.debug('matching_funcs=%s completer_prefix=%s',
                      matching_funcs, repr(completer_prefix))

        completion_func = None
        if completer_prefix:
            completion_func_name = 'complete_' + '_'.join(completer_prefix)
            try:
                completion_func = getattr(self, completion_func_name)
                logging.debug('found completer %s', completion_func_name)
            except AttributeError:
                pass

        if completion_func:
            # Most specific completion func found
            try:
                cmd_arg = line[:end].split(
                    None, len(completer_prefix))[len(completer_prefix)]
            except IndexError:
                cmd_arg = ''
            logging.debug('completenames: calling completion func cmd_arg=%s',
                          repr(cmd_arg))
            return completion_func(cmd_arg,
                                   begin - (end - len(cmd_arg)),
                                   len(cmd_arg))
        else:
            return self.do_commands.get_completions(line[:end])


    def complete_help(self, text, begin, end):
        logging.debug('complete_help text=%s begin=%d end=%d',
                      repr(text), begin, end)
        compl_line = text[:end].lstrip()
        commands = set(self.do_commands.get_completions(compl_line))
        topics = set(self.help_topics.get_completions(compl_line))
        return sorted(list(commands | topics))



    # Overcome a bug in Kmd when completing line starting with spaces
    @print_exc
    def word_break_hook(self, begidx, endidx):
        """word_break_hook(begidx, endidx)
        When completing '?<topic>' make sure '?' is a word break character.
        Ditto for '!<command>'.
        Installed as :attr:`rl.completer.word_break_hook <rl:rl.Completer.word_break_hook>`.
        """
        # This has a flaw as we cannot complete names that contain
        # the new word break character.
        origline = completion.line_buffer
        line = origline.lstrip()
        stripped = len(origline) - len(line)
        if begidx - stripped == 0:
            if line and (line[0] == '?' or line[0] in self.shell_escape_chars):
                if line[0] not in completer.word_break_characters:
                    return line[0] + completer.word_break_characters
