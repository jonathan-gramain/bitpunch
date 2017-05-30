

class CommandFinder(object):

    def __init__(self):
        self.top = {}

    def __eq__(self, other):
        assert isinstance(other, CommandFinder)
        return self.top == other.top

    def __repr__(self):
        return self.get_command_tree_str()

    def __len__(self):
        return self.get_nb_commands()

    def __iter__(self):
        return iter(self.get_all_commands())

    def add_command(self, command_path):
        cur = self.top
        for subcmd in command_path:
            if subcmd not in cur:
                cur[subcmd] = {}
            cur = cur[subcmd]

    def find_subcommands(self, command_path):
        cur = self.top
        for subcmd in command_path:
            if subcmd not in cur:
                return None
            cur = cur[subcmd]
        return sorted(cur.keys())

    def filter_by_subcommand_prefixes(self, prefixes_path,
                                      ignore_trailing_unknown=False):

        res = CommandFinder()

        def recur(prefixes_path, src, dst):
            for child in src:
                if not prefixes_path or child.startswith(prefixes_path[0]):
                    dst[child] = {}
                    recur(prefixes_path[1:], src[child], dst[child])
                    if not ignore_trailing_unknown and src[child] and not dst[child]:
                        del dst[child] # delete non-terminal commands

        recur(prefixes_path, self.top, res.top)
        return res


    def get_matching_subcommands_by_prefixes(self, subcmd_prefixes):
        matching_cmds = \
            self.filter_by_subcommand_prefixes(subcmd_prefixes)
        unambiguous_prefix = matching_cmds.get_common_prefix()

        matching_prefix = unambiguous_prefix[:len(subcmd_prefixes)]
        matching_subcmds = matching_cmds.find_subcommands(matching_prefix)

        return matching_subcmds, matching_prefix


    def get_nb_commands(self):

        def get_nb_in_subtree(subtree):
            nb_total = 0
            for child in subtree:
                sub_nb = get_nb_in_subtree(subtree[child])
                nb_total += sub_nb if sub_nb > 0 else 1
            return nb_total

        return get_nb_in_subtree(self.top)

    def get_all_commands(self):

        all_commands = []
        def recur(prefix, subtree):
            for child in subtree:
                if subtree[child]:
                    recur(prefix + [child], subtree[child])
                else:
                    all_commands.append(prefix + [child])

        recur([], self.top)
        return all_commands

    def get_command_tree_str(self):

        def recur(cur, name, depth):
            tree = (('  ' * depth) + ('\_' if cur else '|_') + name) + '\n'
            for child in cur:
                tree += recur(cur[child], child, depth + 1)
            return tree

        return recur(self.top, '', 0)

    def get_common_prefix(self):
        common_prefix = []
        cur = self.top
        while cur:
            if len(cur) != 1:
                return common_prefix
            cmd = cur.keys()[0]
            common_prefix.append(cmd)
            cur = cur[cmd]
        return common_prefix # sole command

    def get_longest_unambiguous_command_prefix(self, prefixes_path,
                                               ignore_trailing_unknown=False):
        filtered_cmds = self.filter_by_subcommand_prefixes(prefixes_path,
                                                           ignore_trailing_unknown)
        if len(filtered_cmds) == 0:
            return [] if ignore_trailing_unknown else None

        return filtered_cmds.get_common_prefix()[:len(prefixes_path)]


def test_command_finder():

    cf = CommandFinder()
    cf.add_command(['print'])
    cf.add_command(['dump', 'python'])
    cf.add_command(['dump', 'json'])
    cf.add_command(['keys'])
    cf.add_command(['bleh'])
    cf.add_command(['blip', 'me'])
    cf.add_command(['blip', 'you'])
    cf.add_command(['frob', 'once', 'knob1'])
    cf.add_command(['frob', 'twice', 'knob1'])
    cf.add_command(['frob', 'once', 'knob2'])

    assert len(cf) == 10

    subs = cf.find_subcommands([])
    assert subs == ['bleh', 'blip', 'dump', 'frob', 'keys', 'print']

    subs = cf.find_subcommands(['print'])
    assert subs == []

    subs = cf.find_subcommands(['blip'])
    assert subs == ['me', 'you']

    subs = cf.find_subcommands(['frob'])
    assert subs == ['once', 'twice']

    subs = cf.find_subcommands(['frob', 'once'])
    assert subs == ['knob1', 'knob2']

    subs = cf.find_subcommands(['frob', 'twice'])
    assert subs == ['knob1']

    subs = cf.find_subcommands(['twiddle'])
    assert subs is None

    subs = cf.find_subcommands(['frob', 'oncemore'])
    assert subs is None

    subs = cf.find_subcommands(['frob', 'once', 'knob1'])
    assert subs == []

    subs = cf.find_subcommands(['blip', 'you', 'again'])
    assert subs is None


    subcmds, prefix = cf.get_matching_subcommands_by_prefixes([])
    assert subcmds == ['bleh', 'blip', 'dump', 'frob', 'keys', 'print']
    assert prefix == []

    subcmds, prefix = cf.get_matching_subcommands_by_prefixes(['b'])
    assert subcmds == ['bleh', 'blip']
    assert prefix == []

    # unambiguous, shall go one level deeper
    subcmds, prefix = cf.get_matching_subcommands_by_prefixes(['fr'])
    assert subcmds == ['once', 'twice']
    assert prefix == ['frob']

    subcmds, prefix = cf.get_matching_subcommands_by_prefixes(['frog'])
    assert subcmds == []
    assert prefix == []

    subcmds, prefix = cf.get_matching_subcommands_by_prefixes(['frob', 'later'])
    assert subcmds == []
    assert prefix == []

    subcmds, prefix = cf.get_matching_subcommands_by_prefixes(['b', 'me'])
    assert subcmds == ['bleh', 'blip']
    assert prefix == []

    subcmds, prefix = cf.get_matching_subcommands_by_prefixes(['ble'])
    assert subcmds == []
    assert prefix == ['bleh']

    subcmds, prefix = cf.get_matching_subcommands_by_prefixes(['bli', 'me'])
    assert subcmds == []
    assert prefix == ['blip', 'me']

    # once we've found a final command, additional arguments are allowed
    subcmds, prefix = cf.get_matching_subcommands_by_prefixes(['frob', 'once', 'knob2', 'arg1', 'arg2'])
    assert subcmds == []
    assert prefix == ['frob', 'once', 'knob2']

    # same with prefixes
    subcmds, prefix = cf.get_matching_subcommands_by_prefixes(['bli', 'm', 'arg1'])
    assert subcmds == []
    assert prefix == ['blip', 'me']


    cf2 = cf.filter_by_subcommand_prefixes([])
    assert cf == cf2

    cf2 = cf.filter_by_subcommand_prefixes(['frob'])
    cf_ref = CommandFinder()
    cf_ref.add_command(['frob', 'once', 'knob1'])
    cf_ref.add_command(['frob', 'twice', 'knob1'])
    cf_ref.add_command(['frob', 'once', 'knob2'])
    assert cf2 == cf_ref

    cf2 = cf.filter_by_subcommand_prefixes(['bl'])
    cf_ref = CommandFinder()
    cf_ref.add_command(['bleh'])
    cf_ref.add_command(['blip', 'me'])
    cf_ref.add_command(['blip', 'you'])
    assert cf2 == cf_ref

    cf2 = cf.filter_by_subcommand_prefixes(['bl', ''])
    cf_ref = CommandFinder()
    cf_ref.add_command(['bleh']) # empty argument to 'bleh'
    cf_ref.add_command(['blip', 'me'])
    cf_ref.add_command(['blip', 'you'])
    assert cf2 == cf_ref

    cf2 = cf.filter_by_subcommand_prefixes(['bl', 'm'])
    cf_ref = CommandFinder()
    cf_ref.add_command(['blip', 'me'])
    cf_ref.add_command(['bleh']) # 'm' argument to bleh
    assert cf2 == cf_ref

    cf2 = cf.filter_by_subcommand_prefixes(['frobnicate'])
    cf_ref = CommandFinder()
    assert cf2 == cf_ref

    cf2 = cf.filter_by_subcommand_prefixes(['frob', 'once', 'knob2'])
    cf_ref = CommandFinder()
    cf_ref.add_command(['frob', 'once', 'knob2'])
    assert cf2 == cf_ref

    cf2 = cf.filter_by_subcommand_prefixes(['frob', 'once', 'knob3'])
    cf_ref = CommandFinder()
    assert cf2 == cf_ref

    cf2 = cf.filter_by_subcommand_prefixes(['fro', 'onc', 'knob'])
    cf_ref = CommandFinder()
    cf_ref.add_command(['frob', 'once', 'knob1'])
    cf_ref.add_command(['frob', 'once', 'knob2'])
    assert cf2 == cf_ref


    cf2 = cf.filter_by_subcommand_prefixes(['', 'once'])
    cf_ref = CommandFinder()
    cf_ref.add_command(['bleh'])
    cf_ref.add_command(['frob', 'once', 'knob1'])
    cf_ref.add_command(['frob', 'once', 'knob2'])
    cf_ref.add_command(['keys'])
    cf_ref.add_command(['print'])
    assert cf2 == cf_ref


    prefix = cf.get_longest_unambiguous_command_prefix(['frob'])
    assert prefix == ['frob']

    prefix = cf.get_longest_unambiguous_command_prefix(['frobnicate'])
    assert prefix is None

    prefix = cf.get_longest_unambiguous_command_prefix(['frob', 'once'])
    assert prefix == ['frob', 'once']

    prefix = cf.get_longest_unambiguous_command_prefix(['frob', 'twic'])
    assert prefix == ['frob', 'twice']

    prefix = cf.get_longest_unambiguous_command_prefix(['frob', 'thrice'])
    assert prefix is None

    prefix = cf.get_longest_unambiguous_command_prefix(['frob', '', 'nothing_valid'])
    assert prefix is None

    prefix = cf.get_longest_unambiguous_command_prefix(['frob', '', 'knob1'])
    assert prefix == ['frob']

    prefix = cf.get_longest_unambiguous_command_prefix(['frob', '', 'knob2'])
    assert prefix == ['frob', 'once', 'knob2']

    prefix = cf.get_longest_unambiguous_command_prefix(['frob', '', 'knob2', 'arg'])
    assert prefix == ['frob', 'once', 'knob2']

    prefix = cf.get_longest_unambiguous_command_prefix(['frob', ''])
    assert prefix == ['frob']

    prefix = cf.get_longest_unambiguous_command_prefix(['bl'])
    assert prefix == []

    prefix = cf.get_longest_unambiguous_command_prefix(['bli'])
    assert prefix == ['blip']

    prefix = cf.get_longest_unambiguous_command_prefix(['ble'])
    assert prefix == ['bleh']

    prefix = cf.get_longest_unambiguous_command_prefix(['bleh'])
    assert prefix == ['bleh']

    prefix = cf.get_longest_unambiguous_command_prefix(['bleh', 'blah'])
    assert prefix == ['bleh'] # with 'blah' argument


    prefix = cf.get_longest_unambiguous_command_prefix(['frob'],
                                                       ignore_trailing_unknown=True)
    assert prefix == ['frob']

    prefix = cf.get_longest_unambiguous_command_prefix(['frob', 'sometimes'],
                                                       ignore_trailing_unknown=True)
    assert prefix == ['frob']

    prefix = cf.get_longest_unambiguous_command_prefix(['fr', 'on'],
                                                       ignore_trailing_unknown=True)
    assert prefix == ['frob', 'once']

    prefix = cf.get_longest_unambiguous_command_prefix(['fr', 'never'],
                                                       ignore_trailing_unknown=True)
    assert prefix == ['frob']
