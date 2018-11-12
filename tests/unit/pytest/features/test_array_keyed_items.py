#!/usr/bin/env python

import pytest

from bitpunch import model
import conftest

spec_file_array_keyed_items = """

let u32 = [4] byte <> integer { @signed = false; @endian = 'little'; };

let Item = struct {
    name: string { @boundary = '\\0'; };
    value: u32;
    @key = name;
};

env("DATASOURCE") <> struct {
    integers: [] Item;
};

"""

data_file_array_keyed_items = """
"alpha" 00 01 00 00 00
"bravo" 00 02 00 00 00
"charlie" 00 03 00 00 00
"""

spec_file_array_keyed_filtered_keys = """

let u32 = [4] byte <> integer { @signed = false; @endian = 'little'; };

let Item = struct {
    name: string { @boundary = '\\0'; } <> base64 <> string;
    value: u32;
    @key = name;
};

env("DATASOURCE") <> struct {
    integers: [] Item;
};

"""

data_file_array_keyed_filtered_keys = """
"YWxwaGE=" 00 01 00 00 00
"YnJhdm8=" 00 02 00 00 00
"Y2hhcmxpZQ==" 00 03 00 00 00
"""

spec_file_array_keyed_filtered_items = """

let u32 = [4] byte <> integer { @signed = false; @endian = 'little'; };

let Item = string { @boundary = '\\n'; } <> base64 <> struct {
    name: string { @boundary = '\\0'; };
    value: u32;
    @key = name;
};

env("DATASOURCE") <> struct {
    integers: [] Item;
};

"""

data_file_array_keyed_filtered_items = """
"YWxwaGEAAQAAAA==\n"
"YnJhdm8AAgAAAA==\n"
"Y2hhcmxpZQADAAAA\n"
"""

@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_array_keyed_items,
        'data': data_file_array_keyed_items,
    }, {
        'spec': spec_file_array_keyed_filtered_keys,
        'data': data_file_array_keyed_filtered_keys,
    }, {
        'spec': spec_file_array_keyed_filtered_items,
        'data': data_file_array_keyed_filtered_items,
    }])
def params_array_keyed_items(request):
    return conftest.make_testcase(request.param)


def test_array_keyed_items(params_array_keyed_items):
    params = params_array_keyed_items
    dtree = params['dtree']
    assert len(dtree.integers) == 3
    assert dtree.integers['charlie'].value == 3
    assert dtree.integers['alpha'].value == 1
    assert dtree.integers['bravo'].value == 2
    assert dtree.eval_expr('integers["charlie"].value') == 3

    assert ([str(key) for key in dtree.integers.iter_keys()]
            == ['alpha', 'bravo', 'charlie'])



spec_file_array_keyed_items_with_duplicates_1 = """

let u32 = [4] byte <> integer { @signed = false; @endian = 'little'; };

let Item = struct {
    name: string { @boundary = '\\0'; };
    value: u32;
    @key = name;
};

env("DATASOURCE") <> struct {
    integers: [] Item;
};

"""

data_file_array_keyed_items_with_duplicates_1 = """
"juliett" 00 0A 00 00 00
"foxtrot" 00 06 00 00 00
"golf" 00 07 00 00 00
"alpha" 00 01 00 00 00
"hotel" 00 08 00 00 00
"echo" 00 05 00 00 00
"echo" 00 05 00 00 00
"india" 00 09 00 00 00
"hotel" 00 08 00 00 00
"echo" 00 05 00 00 00
"foxtrot" 00 06 00 00 00
"india" 00 09 00 00 00
"india" 00 09 00 00 00
"juliett" 00 0A 00 00 00
"charlie" 00 03 00 00 00
"foxtrot" 00 06 00 00 00
"echo" 00 05 00 00 00
"delta" 00 04 00 00 00
"delta" 00 04 00 00 00
"juliett" 00 0A 00 00 00
"delta" 00 04 00 00 00
"juliett" 00 0A 00 00 00
"hotel" 00 08 00 00 00
"foxtrot" 00 06 00 00 00
"foxtrot" 00 06 00 00 00
"hotel" 00 08 00 00 00
"golf" 00 07 00 00 00
"charlie" 00 03 00 00 00
"delta" 00 04 00 00 00
"india" 00 09 00 00 00
"charlie" 00 03 00 00 00
"india" 00 09 00 00 00
"juliett" 00 0A 00 00 00
"juliett" 00 0A 00 00 00
"golf" 00 07 00 00 00
"juliett" 00 0A 00 00 00
"bravo" 00 02 00 00 00
"golf" 00 07 00 00 00
"juliett" 00 0A 00 00 00
"echo" 00 05 00 00 00
"bravo" 00 02 00 00 00
"india" 00 09 00 00 00
"golf" 00 07 00 00 00
"foxtrot" 00 06 00 00 00
"india" 00 09 00 00 00
"hotel" 00 08 00 00 00
"hotel" 00 08 00 00 00
"hotel" 00 08 00 00 00
"golf" 00 07 00 00 00
"juliett" 00 0A 00 00 00
"juliett" 00 0A 00 00 00
"hotel" 00 08 00 00 00
"golf" 00 07 00 00 00
"india" 00 09 00 00 00
"india" 00 09 00 00 00
"""

spec_file_array_keyed_items_with_duplicates_2 = """

let u32 = [4] byte <> integer { @signed = false; @endian = 'little'; };

let Item = struct {
    name: [8] byte <> string { @boundary = ' '; };
    value: u32;
    @key = name;
};

env("DATASOURCE") <> struct {
    integers: [] Item;
};

"""

data_file_array_keyed_items_with_duplicates_2 = """
"juliett " 0A 00 00 00
"foxtrot " 06 00 00 00
"golf    " 07 00 00 00
"alpha   " 01 00 00 00
"hotel   " 08 00 00 00
"echo    " 05 00 00 00
"echo    " 05 00 00 00
"india   " 09 00 00 00
"hotel   " 08 00 00 00
"echo    " 05 00 00 00
"foxtrot " 06 00 00 00
"india   " 09 00 00 00
"india   " 09 00 00 00
"juliett " 0A 00 00 00
"charlie " 03 00 00 00
"foxtrot " 06 00 00 00
"echo    " 05 00 00 00
"delta   " 04 00 00 00
"delta   " 04 00 00 00
"juliett " 0A 00 00 00
"delta   " 04 00 00 00
"juliett " 0A 00 00 00
"hotel   " 08 00 00 00
"foxtrot " 06 00 00 00
"foxtrot " 06 00 00 00
"hotel   " 08 00 00 00
"golf    " 07 00 00 00
"charlie " 03 00 00 00
"delta   " 04 00 00 00
"india   " 09 00 00 00
"charlie " 03 00 00 00
"india   " 09 00 00 00
"juliett " 0A 00 00 00
"juliett " 0A 00 00 00
"golf    " 07 00 00 00
"juliett " 0A 00 00 00
"bravo   " 02 00 00 00
"golf    " 07 00 00 00
"juliett " 0A 00 00 00
"echo    " 05 00 00 00
"bravo   " 02 00 00 00
"india   " 09 00 00 00
"golf    " 07 00 00 00
"foxtrot " 06 00 00 00
"india   " 09 00 00 00
"hotel   " 08 00 00 00
"hotel   " 08 00 00 00
"hotel   " 08 00 00 00
"golf    " 07 00 00 00
"juliett " 0A 00 00 00
"juliett " 0A 00 00 00
"hotel   " 08 00 00 00
"golf    " 07 00 00 00
"india   " 09 00 00 00
"india   " 09 00 00 00
"""

data_file_array_keyed_items_with_duplicates_2_sorted = """
"alpha   " 01 00 00 00
"bravo   " 02 00 00 00
"bravo   " 02 00 00 00
"charlie " 03 00 00 00
"charlie " 03 00 00 00
"charlie " 03 00 00 00
"delta   " 04 00 00 00
"delta   " 04 00 00 00
"delta   " 04 00 00 00
"delta   " 04 00 00 00
"echo    " 05 00 00 00
"echo    " 05 00 00 00
"echo    " 05 00 00 00
"echo    " 05 00 00 00
"echo    " 05 00 00 00
"foxtrot " 06 00 00 00
"foxtrot " 06 00 00 00
"foxtrot " 06 00 00 00
"foxtrot " 06 00 00 00
"foxtrot " 06 00 00 00
"foxtrot " 06 00 00 00
"golf    " 07 00 00 00
"golf    " 07 00 00 00
"golf    " 07 00 00 00
"golf    " 07 00 00 00
"golf    " 07 00 00 00
"golf    " 07 00 00 00
"golf    " 07 00 00 00
"hotel   " 08 00 00 00
"hotel   " 08 00 00 00
"hotel   " 08 00 00 00
"hotel   " 08 00 00 00
"hotel   " 08 00 00 00
"hotel   " 08 00 00 00
"hotel   " 08 00 00 00
"hotel   " 08 00 00 00
"india   " 09 00 00 00
"india   " 09 00 00 00
"india   " 09 00 00 00
"india   " 09 00 00 00
"india   " 09 00 00 00
"india   " 09 00 00 00
"india   " 09 00 00 00
"india   " 09 00 00 00
"india   " 09 00 00 00
"juliett " 0A 00 00 00
"juliett " 0A 00 00 00
"juliett " 0A 00 00 00
"juliett " 0A 00 00 00
"juliett " 0A 00 00 00
"juliett " 0A 00 00 00
"juliett " 0A 00 00 00
"juliett " 0A 00 00 00
"juliett " 0A 00 00 00
"juliett " 0A 00 00 00
"""

spec_file_array_keyed_items_with_duplicates_filtered_keys = """

let u32 = [4] byte <> integer { @signed = false; @endian = 'little'; };

let Item = struct {
    name: [12] byte <> base64 <> string { @boundary = ' '; };
    value: u32;
    @key = name;
};

env("DATASOURCE") <> struct {
    integers: [] Item;
};

"""

data_file_array_keyed_items_with_duplicates_filtered_keys = """
"anVsaWV0dCA=" 0A 00 00 00
"Zm94dHJvdCA=" 06 00 00 00
"Z29sZiAgICA=" 07 00 00 00
"YWxwaGEgICA=" 01 00 00 00
"aG90ZWwgICA=" 08 00 00 00
"ZWNobyAgICA=" 05 00 00 00
"ZWNobyAgICA=" 05 00 00 00
"aW5kaWEgICA=" 09 00 00 00
"aG90ZWwgICA=" 08 00 00 00
"ZWNobyAgICA=" 05 00 00 00
"Zm94dHJvdCA=" 06 00 00 00
"aW5kaWEgICA=" 09 00 00 00
"aW5kaWEgICA=" 09 00 00 00
"anVsaWV0dCA=" 0A 00 00 00
"Y2hhcmxpZSA=" 03 00 00 00
"Zm94dHJvdCA=" 06 00 00 00
"ZWNobyAgICA=" 05 00 00 00
"ZGVsdGEgICA=" 04 00 00 00
"ZGVsdGEgICA=" 04 00 00 00
"anVsaWV0dCA=" 0A 00 00 00
"ZGVsdGEgICA=" 04 00 00 00
"anVsaWV0dCA=" 0A 00 00 00
"aG90ZWwgICA=" 08 00 00 00
"Zm94dHJvdCA=" 06 00 00 00
"Zm94dHJvdCA=" 06 00 00 00
"aG90ZWwgICA=" 08 00 00 00
"Z29sZiAgICA=" 07 00 00 00
"Y2hhcmxpZSA=" 03 00 00 00
"ZGVsdGEgICA=" 04 00 00 00
"aW5kaWEgICA=" 09 00 00 00
"Y2hhcmxpZSA=" 03 00 00 00
"aW5kaWEgICA=" 09 00 00 00
"anVsaWV0dCA=" 0A 00 00 00
"anVsaWV0dCA=" 0A 00 00 00
"Z29sZiAgICA=" 07 00 00 00
"anVsaWV0dCA=" 0A 00 00 00
"YnJhdm8gICA=" 02 00 00 00
"Z29sZiAgICA=" 07 00 00 00
"anVsaWV0dCA=" 0A 00 00 00
"ZWNobyAgICA=" 05 00 00 00
"YnJhdm8gICA=" 02 00 00 00
"aW5kaWEgICA=" 09 00 00 00
"Z29sZiAgICA=" 07 00 00 00
"Zm94dHJvdCA=" 06 00 00 00
"aW5kaWEgICA=" 09 00 00 00
"aG90ZWwgICA=" 08 00 00 00
"aG90ZWwgICA=" 08 00 00 00
"aG90ZWwgICA=" 08 00 00 00
"Z29sZiAgICA=" 07 00 00 00
"anVsaWV0dCA=" 0A 00 00 00
"anVsaWV0dCA=" 0A 00 00 00
"aG90ZWwgICA=" 08 00 00 00
"Z29sZiAgICA=" 07 00 00 00
"aW5kaWEgICA=" 09 00 00 00
"aW5kaWEgICA=" 09 00 00 00
"""

data_file_array_keyed_items_with_duplicates_filtered_keys_sorted = """
"aG90ZWwgICA=" 08 00 00 00
"aG90ZWwgICA=" 08 00 00 00
"aG90ZWwgICA=" 08 00 00 00
"aG90ZWwgICA=" 08 00 00 00
"aG90ZWwgICA=" 08 00 00 00
"aG90ZWwgICA=" 08 00 00 00
"aG90ZWwgICA=" 08 00 00 00
"aG90ZWwgICA=" 08 00 00 00
"anVsaWV0dCA=" 0A 00 00 00
"anVsaWV0dCA=" 0A 00 00 00
"anVsaWV0dCA=" 0A 00 00 00
"anVsaWV0dCA=" 0A 00 00 00
"anVsaWV0dCA=" 0A 00 00 00
"anVsaWV0dCA=" 0A 00 00 00
"anVsaWV0dCA=" 0A 00 00 00
"anVsaWV0dCA=" 0A 00 00 00
"anVsaWV0dCA=" 0A 00 00 00
"anVsaWV0dCA=" 0A 00 00 00
"aW5kaWEgICA=" 09 00 00 00
"aW5kaWEgICA=" 09 00 00 00
"aW5kaWEgICA=" 09 00 00 00
"aW5kaWEgICA=" 09 00 00 00
"aW5kaWEgICA=" 09 00 00 00
"aW5kaWEgICA=" 09 00 00 00
"aW5kaWEgICA=" 09 00 00 00
"aW5kaWEgICA=" 09 00 00 00
"aW5kaWEgICA=" 09 00 00 00
"Y2hhcmxpZSA=" 03 00 00 00
"Y2hhcmxpZSA=" 03 00 00 00
"Y2hhcmxpZSA=" 03 00 00 00
"YnJhdm8gICA=" 02 00 00 00
"YnJhdm8gICA=" 02 00 00 00
"YWxwaGEgICA=" 01 00 00 00
"Z29sZiAgICA=" 07 00 00 00
"Z29sZiAgICA=" 07 00 00 00
"Z29sZiAgICA=" 07 00 00 00
"Z29sZiAgICA=" 07 00 00 00
"Z29sZiAgICA=" 07 00 00 00
"Z29sZiAgICA=" 07 00 00 00
"Z29sZiAgICA=" 07 00 00 00
"ZGVsdGEgICA=" 04 00 00 00
"ZGVsdGEgICA=" 04 00 00 00
"ZGVsdGEgICA=" 04 00 00 00
"ZGVsdGEgICA=" 04 00 00 00
"Zm94dHJvdCA=" 06 00 00 00
"Zm94dHJvdCA=" 06 00 00 00
"Zm94dHJvdCA=" 06 00 00 00
"Zm94dHJvdCA=" 06 00 00 00
"Zm94dHJvdCA=" 06 00 00 00
"Zm94dHJvdCA=" 06 00 00 00
"ZWNobyAgICA=" 05 00 00 00
"ZWNobyAgICA=" 05 00 00 00
"ZWNobyAgICA=" 05 00 00 00
"ZWNobyAgICA=" 05 00 00 00
"ZWNobyAgICA=" 05 00 00 00
"""

spec_file_array_keyed_items_with_duplicates_filtered_items = """

let u32 = [4] byte <> integer { @signed = false; @endian = 'little'; };

let Item = [16] byte <> base64 <> struct {
    name: [8] byte <> string { @boundary = ' '; };
    value: u32;
    @key = name;
};

env("DATASOURCE") <> struct {
    integers: [] Item;
};

"""

data_file_array_keyed_items_with_duplicates_filtered_items = """
"aW5kaWEgICAJAAAA" # india
"aW5kaWEgICAJAAAA" # india
"anVsaWV0dCAKAAAA" # juliett
"Zm94dHJvdCAGAAAA" # foxtrot
"Y2hhcmxpZSADAAAA" # charlie
"ZWNobyAgICAFAAAA" # echo
"aG90ZWwgICAIAAAA" # hotel
"Zm94dHJvdCAGAAAA" # foxtrot
"aW5kaWEgICAJAAAA" # india
"Y2hhcmxpZSADAAAA" # charlie
"aG90ZWwgICAIAAAA" # hotel
"anVsaWV0dCAKAAAA" # juliett
"Z29sZiAgICAHAAAA" # golf
"anVsaWV0dCAKAAAA" # juliett
"YnJhdm8gICACAAAA" # bravo
"Z29sZiAgICAHAAAA" # golf
"YnJhdm8gICACAAAA" # bravo
"ZWNobyAgICAFAAAA" # echo
"aG90ZWwgICAIAAAA" # hotel
"Zm94dHJvdCAGAAAA" # foxtrot
"Zm94dHJvdCAGAAAA" # foxtrot
"anVsaWV0dCAKAAAA" # juliett
"YWxwaGEgICABAAAA" # alpha
"anVsaWV0dCAKAAAA" # juliett
"aW5kaWEgICAJAAAA" # india
"aG90ZWwgICAIAAAA" # hotel
"ZGVsdGEgICAEAAAA" # delta
"anVsaWV0dCAKAAAA" # juliett
"aG90ZWwgICAIAAAA" # hotel
"ZWNobyAgICAFAAAA" # echo
"anVsaWV0dCAKAAAA" # juliett
"Z29sZiAgICAHAAAA" # golf
"aG90ZWwgICAIAAAA" # hotel
"aW5kaWEgICAJAAAA" # india
"aG90ZWwgICAIAAAA" # hotel
"anVsaWV0dCAKAAAA" # juliett
"ZGVsdGEgICAEAAAA" # delta
"aW5kaWEgICAJAAAA" # india
"ZGVsdGEgICAEAAAA" # delta
"Z29sZiAgICAHAAAA" # golf
"aG90ZWwgICAIAAAA" # hotel
"Z29sZiAgICAHAAAA" # golf
"Zm94dHJvdCAGAAAA" # foxtrot
"Z29sZiAgICAHAAAA" # golf
"Y2hhcmxpZSADAAAA" # charlie
"aW5kaWEgICAJAAAA" # india
"aW5kaWEgICAJAAAA" # india
"anVsaWV0dCAKAAAA" # juliett
"aW5kaWEgICAJAAAA" # india
"Z29sZiAgICAHAAAA" # golf
"ZWNobyAgICAFAAAA" # echo
"ZGVsdGEgICAEAAAA" # delta
"anVsaWV0dCAKAAAA" # juliett
"Zm94dHJvdCAGAAAA" # foxtrot
"ZWNobyAgICAFAAAA" # echo
"""

data_file_array_keyed_items_with_duplicates_filtered_items_sorted = """
"YWxwaGEgICABAAAA" # alpha
"YnJhdm8gICACAAAA" # bravo
"YnJhdm8gICACAAAA" # bravo
"Y2hhcmxpZSADAAAA" # charlie
"Y2hhcmxpZSADAAAA" # charlie
"Y2hhcmxpZSADAAAA" # charlie
"ZGVsdGEgICAEAAAA" # delta
"ZGVsdGEgICAEAAAA" # delta
"ZGVsdGEgICAEAAAA" # delta
"ZGVsdGEgICAEAAAA" # delta
"ZWNobyAgICAFAAAA" # echo
"ZWNobyAgICAFAAAA" # echo
"ZWNobyAgICAFAAAA" # echo
"ZWNobyAgICAFAAAA" # echo
"ZWNobyAgICAFAAAA" # echo
"Zm94dHJvdCAGAAAA" # foxtrot
"Zm94dHJvdCAGAAAA" # foxtrot
"Zm94dHJvdCAGAAAA" # foxtrot
"Zm94dHJvdCAGAAAA" # foxtrot
"Zm94dHJvdCAGAAAA" # foxtrot
"Zm94dHJvdCAGAAAA" # foxtrot
"Z29sZiAgICAHAAAA" # golf
"Z29sZiAgICAHAAAA" # golf
"Z29sZiAgICAHAAAA" # golf
"Z29sZiAgICAHAAAA" # golf
"Z29sZiAgICAHAAAA" # golf
"Z29sZiAgICAHAAAA" # golf
"Z29sZiAgICAHAAAA" # golf
"aG90ZWwgICAIAAAA" # hotel
"aG90ZWwgICAIAAAA" # hotel
"aG90ZWwgICAIAAAA" # hotel
"aG90ZWwgICAIAAAA" # hotel
"aG90ZWwgICAIAAAA" # hotel
"aG90ZWwgICAIAAAA" # hotel
"aG90ZWwgICAIAAAA" # hotel
"aG90ZWwgICAIAAAA" # hotel
"aW5kaWEgICAJAAAA" # india
"aW5kaWEgICAJAAAA" # india
"aW5kaWEgICAJAAAA" # india
"aW5kaWEgICAJAAAA" # india
"aW5kaWEgICAJAAAA" # india
"aW5kaWEgICAJAAAA" # india
"aW5kaWEgICAJAAAA" # india
"aW5kaWEgICAJAAAA" # india
"aW5kaWEgICAJAAAA" # india
"anVsaWV0dCAKAAAA" # juliett
"anVsaWV0dCAKAAAA" # juliett
"anVsaWV0dCAKAAAA" # juliett
"anVsaWV0dCAKAAAA" # juliett
"anVsaWV0dCAKAAAA" # juliett
"anVsaWV0dCAKAAAA" # juliett
"anVsaWV0dCAKAAAA" # juliett
"anVsaWV0dCAKAAAA" # juliett
"anVsaWV0dCAKAAAA" # juliett
"anVsaWV0dCAKAAAA" # juliett
"""


@pytest.fixture(
    scope='module',
    params=[{
        'spec': spec_file_array_keyed_items_with_duplicates_1,
        'data': data_file_array_keyed_items_with_duplicates_1,
    }, {
        'spec': spec_file_array_keyed_items_with_duplicates_2,
        'data': data_file_array_keyed_items_with_duplicates_2,
    }, {
        'spec': spec_file_array_keyed_items_with_duplicates_2,
        'data': data_file_array_keyed_items_with_duplicates_2_sorted,
    }, {
        'spec': spec_file_array_keyed_items_with_duplicates_filtered_keys,
        'data': data_file_array_keyed_items_with_duplicates_filtered_keys,
    }, {
        'spec': spec_file_array_keyed_items_with_duplicates_filtered_keys,
        'data': data_file_array_keyed_items_with_duplicates_filtered_keys_sorted,
    }, {
        'spec': spec_file_array_keyed_items_with_duplicates_filtered_items,
        'data': data_file_array_keyed_items_with_duplicates_filtered_items,
    }, {
        'spec': spec_file_array_keyed_items_with_duplicates_filtered_items,
        'data': data_file_array_keyed_items_with_duplicates_filtered_items_sorted,
    }])
def params_array_keyed_items_with_duplicates(request):
    return conftest.make_testcase(request.param)


def test_array_keyed_items_with_duplicates(
        params_array_keyed_items_with_duplicates):
    params = params_array_keyed_items_with_duplicates
    dtree = params['dtree']
    assert len(dtree.integers) == 55
    for (i, name) in enumerate([
            'alpha', 'bravo', 'charlie', 'delta', 'echo',
            'foxtrot', 'golf', 'hotel', 'india', 'juliett']):
        assert dtree.integers[name].value == i + 1
        assert dtree.eval_expr('integers["{0}"].value'.format(name)) == \
            i + 1

    def build_key_list(name, count):
        return [('{0}{{{1}}}'.format(name, i) if i > 0
                 else name)
                for i in range(count)]

    assert (sorted([str(key) for key in dtree.integers.iter_keys()]) == \
            (build_key_list('alpha', 1) +
             build_key_list('bravo', 2) +
             build_key_list('charlie', 3) +
             build_key_list('delta', 4) +
             build_key_list('echo', 5) +
             build_key_list('foxtrot', 6) +
             build_key_list('golf', 7) +
             build_key_list('hotel', 8) +
             build_key_list('india', 9) +
             build_key_list('juliett', 10)))
