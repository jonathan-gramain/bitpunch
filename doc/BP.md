# BP file syntax

The syntax is not frozen now, existing syntax will probably change and
will be extended.

Examples can be found in the various .bp files

The syntax can look familiar to C coders, indeed syntax is inspired
from C structure and array declarations, which are a natural way to
declare fields and items mapping direcly to actual bytes in memory,
although the format is pretty different than C too in various ways.

The general idea is to describe the format in a high level view so
that the offset computation details are left as an exercise to the
bitpunch library.

Note that the names chosen for the data fields in structures do matter
because they will be the reference names when accessing the various
data items through *dpath* expressions, so they must be chosen
carefully to not confuse the user.

TODO describe syntax in depth
