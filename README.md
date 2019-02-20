# BitPunch

*A Swiss Army knife for binary data files*

BitPunch is a core library and a set of tools that allow a user to
access meaningful data from arbitrary binary files.

BitPunch schemas form a descriptive, declarative language used to
describe the structure of the data stored in the files, and schemas
can express a wide range of binary structures.

Once a schema is defined, APIs in various programming languages can be
used to access data in an object-oriented way using path expressions
or iteration. Currently APIs in C and Python2 are available.

## What for?

- to convert binary data coming from custom binary formats into
  human-friendly representations like JSON with minimal effort
  (e.g. for interoperable data exchange);

- to check sanity of files and be able to recover sane data from
  corrupt files that cannot be read with standard tools;

- to implement a reliable and safe parser for some binary format
  instead of writing it directly in a low-level language like C;

- for reverse-engineering of uncommonly used or proprietary file
  formats, and extract data from them or translate to a better data
  interchange format like JSON;

- for writing a variety of applications that can benefit from knowing
  the structure of the files they manipulate. For example, a
  structure-aware hex editor that can highlight or collapse sections
  in a tree structure that maps to the binary schema;

- because it's cool to find out about all the crap that is in your
  favorite MP4 videos using the CLI! :)

## Development status

The current code is to be considered in alpha stage:

- heavy development ongoing

- syntax of BP files likely to evolve


## Overview

Right now bitpunch is made of:

- a C library (libbitpunch) that contains low-level C routines to:

  - parse schema specifications found in .bp files

  - open binary files to expose their contents to filters defined in
    .bp files

- a Python module that allows accessing raw data through an object
  model

- a CLI with analysis commands to access binary files in an
  interactive fashion, with powerful autocompletion support and inline
  help. The CLI relies on the Python library.


It understands a custom specialized syntax (let's call it the BitPunch
syntax, or BP syntax) that describes precisely the layout of the
binary format. Once loaded, binary files can be opened and mapped to
the described layout, so that their meaningful content becomes readily
available through iteration and path expressions.

It's meant to *be able to* cover as many as possible of the numerous
binary formats in existence, which is possible if a conformant .bp
file *can be* written for those formats. This is a key design goal
because esoteric or legacy formats are notoriously hard to exploit by
the lack of available tools to do it, or lack of support on modern
OSes and languages. With some effort, even undocumented proprietary
formats should be exploitable with some reverse-engineering and help
from bitpunch tooling.

It shall support very large binary files without trouble, since mmap()
is used to map the file contents into memory, and browsing specific
elements does not require to browse the entire file first, information
is gathered in a lazy fashion. (The other side of the coin is that you
may be notified of a format error only when you attempt to access
items affected by the error).

## More

- Install instructions and basic usage in [INSTALL.md](INSTALL.md)

- For details about the BP file format, see [BP.md](doc/BP.md)

- A list of planned features is in [TODO.md](TODO.md)

## History

The project started in Boston, Massachusetts in May 2015.

The original motivation was to investigate a compatibility issue with
MP4 files on an embedded device, and the tool was meant to help in
understanding the format and comparing files in their detailed
layout. It then evolved in scope towards a general-purpose tool for
any kind of structured binary file.

## License

- The libbitpunch C library and the bitpunch Python package (the
  Python bindings) are distributed under a [3-Clause BSD
  License](COPYING.BSD).

- The bitpunch Command-Line tool and its helper Python package
  "nestedcmd" are distributed under a [GPL v3 license](COPYING.GPLv3).
