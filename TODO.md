# Future enhancements

## Short term

### Scriptable filters

Filters now can only be implemented as C code and compiled as part of
the main library, they should be:

* loadable as an external module from the .bp file (via an import
  declaration)

* created as a compiled object plugin (loaded with dlopen) or as a
  script of chosen language, that responds to a precise plugin
  interface. Compiled objects should obey the same rules than builtin
  filters in filters/ directory.

### Views

Views are merging different parts of a file (or filtered contents)
together as a single extent of bytes.

This can typically be used to access contents encapsulated in a
transport stream.

A view provides custom backend functions to access ranges of bytes
(generalizes the 'struct bitpunch_data_source' type).

BP syntax TBD (probably with one or more builtin functions).

### Dpath query language

Define a syntax a la XPath to query a set of dpaths from an expression

### Provide a library of .bp files for the most widespread binary file formats

This will allow those files to be manipulated directly without having
to write custom .bp files.

### List view support

As elements in binaries can be chained structurally, although being
logically considered as an ordered set of items, a "list" view would
implement array-style access on a chained list of items.

A list would be defined by:

- a "dpath" to the first element
- a "dpath" to the next element from inside an item
- use of already-implemented "last" keyword in the last item

### Reverse-lookup of dpath from byte offset or byte range

That would allow a user to e.g. ask bitpunch what does this 0xC001C0DE
bit at offset 0x123456 is for, returning a dpath that represented as a
path string would give something like "blocks[42].foo.bar[3].magic",
essentially telling that it is "the magic number of 3rd element of
array defined by field 'bar' in the structure defined by field 'foo'
of the 42th block in the file". That will definitely make his day.

Although it looks scary at first to implement, it should not be too
complex considering all the machinery already implemented to track
items and their locations. It would consist essentially of browsing
structures and arrays recursively, stopping at each level when a match
between the browsed offsets and requested offsets is reached. May
branch on unions though (keep this for later!) or if the requested
range spans multiple items.

### New language bindings

Any new language binding is welcome to complete the existing C and
Python APIs (C++, JavaScript etc.)

A goal is to have bindings well integrated with the language features,
not just mappings to the C API function calls (so it's more work, but
it's worth it!).

### Full check

CLI command + API call to do a complete sanity check of a file (can be
implemented by forcing a browse through all structures and gathering
errors encountered in a list)

### Byte search

Add ability to search for some sequence of bytes inside a particular
set of dpath expressions, and return at which dpath and which offsets
they are found.

### RegExp

Add support for regexp matching operator in expressions


### Decent build system

For now it's based on plain Makefile, why not but currently it does
not even allow to install the software.

### Other

For random ideas that came along the way, or more code-specific stuff,
see [Haystack](#haystack) below or github issues.



## Long term (aka. dreams)

### GUI

Example projects:

- A hex visualizer/editor that can highlight structure using the
  bitpunch lib

- A graphical user interface that can display and extract info in a
  very interactive way


### Binary file write support

One of the big upcoming targeted features is adding write capability,
so that conformant files can be generated from modified input files or
created from scratch from any comprehensive higher level description
of their contents.

Of course, the implementation can start with a limited set of
supported BP schema features and reject those outside the implemented
scope, then augmenting the supported range bit by bit.

### BP file public repository

A central repository to store and lookup specific format files may
be built.

Along with this, the current matching code should be greatly enhanced
to be able to fully benefit from the repository of BP files with
automatic matching with binary contents (a la "file" unix command)

### Performance optimization

For now performance tuning has not been a goal for this project, but
performance optimization can be considered in the future.

Some algorithmic optimizations have been implemented already where it
was thought useful (e.g. bloom filters for efficient search by key).

### Browse code generation

This is a bit far-fetched, but could be an interesting thing to
add. Since the software knows how to browse items, it could also be
used to generate browse instructions in a specific language from a
higher level browse code relying on the .bp file.


# Haystack

This is a haystack of random features to add to the project.
This is NOT an organized or prioritized list.

- improve error reporting

  - notably, add more context information in various places where
    errors occur, e.g. with tracker_error_add_xxx_context() (show
    expressions from source schema etc.)

- various structured outputs related to 'xdump' command (e.g. list all
  fields and their associated hex dump for a given structure, or add a
  caption line on top of data lines telling which field they represent)

- implement 'in' keyword, existence test (in indexed arrays, or in
  all arrays which value type can be compared directly)

- implement 'flags' interpreter => can display flag's textual
  representation, and possibly warn about unknown flags

- implement 'enum' interpreter => can display enum's textual
  representation, and possibly warn about unknown enum values

- implement stentil management, in arrays that are or are used as
  byte arrays through links. This to both benefit from additional
  structural info (e.g. to limit the available space for a slack
  container to the first known link pointing later in the array not
  referring to the same slack container), and to be able to do more
  sanity checks on boundaries, or to inform about unused space.

- add support for bit fields

- CLI

  - add a command to show the target schema of a given dpath
    expression or type name

  - speed up autocompletion of keys by maintaining a dict of seen
    keys to show twin index, and using tracker_get_item_key() in
    place of tracker_get_item_key_multi().

  - display numeric values in a specific format (hex etc.)

  - add dump mode: C initializer


- implement list view (indexed access but list structure on storage)

  - define syntax more precisely

  - should be based on an expression defining the link from an item to
    the next one, and a stop condition

- implement switch/case statement, as a series of 'if' internally
  (implementation will be naturally efficient by the dispatch of
  conditions per block statement)

- param to choose how sign is encoded in integers (2's complement /
  sign bit)

- filter for PCAP files

- box_get_n_items__slice_generic() can be inefficient (and catch
  further errors in the array too early) by doing a browse of all
  items in the array in case the array is slack

  - if the array is slack, browsing the slice only should be good
    (to know for sure the number of valid items)

- in py.test test files: parse a syntax for data blob description so
  that actual byte offsets can be registered and tested thereafter

  - e.g. "00 01 02 {offset:atX} 03 04"
        then in test: data.offsets.atX == 3
        assert model.get_location(foo) == data.offsets.atX

- filter callbacks should be able to return tracker errors by
  themselves

BUG: filters declared after 'key' statement are not initialized
correctly (b_filter is empty)

- insert type ITEM_ARRAY to distinguish with BYTE_ARRAY after compilation

- rework handling of field alignment

  - detect size dependencies between packed fields to know their
    alignment compatibility

  - compile-time errors when alignment is incorrect (unsatisfiable
    size dependencies)

  - use of proper alignment at runtime when conditionals change alignment

- add properties to arrays, or even arbitrary types:

  - e.g. myfield: [size] byte { minspan 30; };
         myfield: MyStruct { maxspan: 60; };

- ability to compare reference to objects with equal/notequal operators

  - self == entries['abc']

- integrate "expr_transform_dpath_internal()" into generic operator
  evaluation (eval_fn())

- test index() builtin properly (especially go through
  expr_dpath_evaluate_filter_type_internal() with CONTAINER dpath
  type)

- inline docstrings (ala pydoc)

- check tests: change .key_type to .key: { .type }, same for .value_type

- BUG: track path dump of tracker in a slice should substract the
  slice's start index

- BUG: mandatory filter attributes are not checked when filter is
  declared without a block scope {...}

- improvement: instead of "self" syntax, propose another more flexible
  syntax that refers to the closest dpath bound to a given named
  filter type (e.g. "<>filter_type", i.e. filter operator as unary
  operator).

- BUG: "no match for operator 'add' with operands of type 'any
  value-type' and 'integer'"
