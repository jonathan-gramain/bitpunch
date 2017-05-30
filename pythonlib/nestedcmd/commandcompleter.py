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

from commandfinder import CommandFinder

class CommandCompleter(CommandFinder):

    def __init__(self):
        super(CommandCompleter, self).__init__()

    def get_completions(self, line):
        subcmd_prefixes_line, sep, active_prefix = line.rpartition(' ')
        subcmd_prefixes = subcmd_prefixes_line.split()

        matching_funcs, completion_prefix = \
                self.get_matching_subcommands_by_prefixes(subcmd_prefixes)

        # only complete when previous commands matches are unique
        if len(completion_prefix) == len(subcmd_prefixes):
            completions = [func for func in matching_funcs
                           if func.startswith(active_prefix)]
            return sorted(completions)
        else:
            return []


def test_command_completer():

    cc = CommandCompleter()
    cc.add_command(['print'])
    cc.add_command(['dump', 'python'])
    cc.add_command(['dump', 'json'])
    cc.add_command(['keys'])
    cc.add_command(['bleh'])
    cc.add_command(['blip', 'me'])
    cc.add_command(['blip', 'you'])
    cc.add_command(['frob', 'once', 'knob1'])
    cc.add_command(['frob', 'twice', 'knob1'])
    cc.add_command(['frob', 'once', 'knob2'])


    completions = cc.get_completions('')
    assert completions == ['bleh', 'blip', 'dump', 'frob', 'keys', 'print']

    completions = cc.get_completions('b')
    assert completions == ['bleh', 'blip']

    # no completion because command is ambiguous
    completions = cc.get_completions('b ')
    assert completions == []

    completions = cc.get_completions('ble')
    assert completions == ['bleh']

    # command bleh has no subcommand
    completions = cc.get_completions('ble ')
    assert completions == []

    # but frob has
    completions = cc.get_completions('frob ')
    assert completions == ['once', 'twice']

    # unless no trailing space still calls for completing the main command
    completions = cc.get_completions('frob')
    assert completions == ['frob']

    # no completion allowed if a previous command is ambiguous
    completions = cc.get_completions('bl m')
    assert completions == []

    # strip away the ambiguity
    completions = cc.get_completions('bli m')
    assert completions == ['me']

    # no luck this time
    completions = cc.get_completions('bli moo')
    assert completions == []

    completions = cc.get_completions('f o knob')
    assert completions == ['knob1', 'knob2']

    # leading or middle extra spaces do not change the result
    completions = cc.get_completions('  f   o    knob')
    assert completions == ['knob1', 'knob2']

    completions = cc.get_completions('f o nob2')
    assert completions == []

    completions = cc.get_completions('  f  o    knob2')
    assert completions == ['knob2']

    completions = cc.get_completions('  f  o    knob2 ')
    assert completions == []
