BitPunch, a Swiss Army knife for structured binary files

# Introduction

BitPunch is a core library and a set of tools that allow a user to
extract meaningful information from structured binary files coming
from widespread or esoteric formats.

## Why would I need it?

- To convert binary data coming from non-widespread binary formats
  into human-friendly representations like JSON with minimal effort
  (e.g. for interoperable data exchange)

- To understand how this binary files is subtly corrupt using extended
  error information from bitpunch, and being able to recover most of
  its payload in a safer way thereafter

- To implement a parser for some random binary format in my C program,
  because it's so boring to write a binary file parser in C which is
  buffer-overflow-proof and reliable and can detect inconsistencies
  with precise error reporting (when speed is not a concern)

- To help me reverse engineer the parsing of uncommonly used or
  proprietary file formats, in order to write a compliant ad-hoc
  parser

- Because I want to write a structure-aware hex editor that can do
  neat structure highlighting or warn me about inconsistencies in my
  edits

- Because it's so cool to explore all the crap that is in your
  favorite .mp4 videos using the CLI! :)

## Development status

alpha

- heavy development ongoing

- syntax of BP files likely to evolve


## Overview

Right now bitpunch is made of:

- a C library (libbitpunch) that contains low-level C routines to
  parse .bp schema specifications, then browse binary contents
  wrt. the schema, reporting inconsistencies when encountered during
  the browse, etc.

- a Python module that allows accessing the raw data through an object
  model

- a CLI to do all sorts of fancy stuff covered by the Python library,
  with powerful autocompletion support and inline help


It understands a custom specialized syntax (let's call it the BitPunch
syntax, or BP syntax) that describes precisely the layout of the
binary format. Once loaded, any compatible file contents can be
explored at will, inner structures can be extracted from
straightforward path expressions, or displayed/dumped in various
human-friendly format for debugging or exporting.

It's meant to *be able to* cover as many as possible of the numerous
binary formats in existence, which is possible if a conformant .bp
file *can be* written for those formats. This is a key design goal
because esoteric or legacy formats are notoriously hard to exploit by
the lack of available tools to do it, or lack of support on modern
OSes and languages. With some effort, even undocumented proprietary
formats should be exploitable with some reverse-engineering skills and
help from bitpunch tooling.

It shall support very large binary files without trouble, since mmap()
is used to map the file contents into memory, and browsing specific
elements does not require to browse the entire file first, information
is gathered in a lazy fashion. (The other side of the coin is that you
may be notified of a format error only when you attempt to access
items affected by the error)

## More

- Install instructions and basic usage in [INSTALL.md](INSTALL.md)

- For a list of specific words used in the project and their meaning,
  see [SLANG.md](doc/SLANG.md)

- For details about the BP file format, see [BP.md](doc/BP.md)

- A list of planned features is in [TODO.md](TODO.md)

## History

The project started in Boston, Massachusetts in May 2015.

The original motivation was to solve a compatibility issue with .mp4
on specific devices, which required understanding the format and
trying various changes in it.

After having written a prototype C parser for .mp4 files, I wanted to
create a syntax to describe more easily how such file should be
parsed, and the scope then quickly turned into making a general
purpose tool instead of restricting it to .mp4.

## License

- The libbitpunch C library and the bitpunch Python package (the
  Python bindings) are distributed under a New BSD License.

- The bitpunch Command-Line tool and its helper Python package
  "nestedcmd" are distributed under a GPL v3 license.

