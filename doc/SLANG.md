# BitPunch slang

- **array**: series of items packed one after the other

  - Unlike C arrays, bitpunch arrays can also hold variable-sized items

  - The length expression between brackets can be specified or
    omitted: if specified it can be an arbitrary expression resolving
    to an integer, if omitted the number of items will be determined
    by other means (interpreter-defined size, or available slack
    space)

  - Array items are accessed in expressions by appending the index to
    the array expression as an integer expression enclosed between '['
    and ']'. (e.g. `my_array[42]`)

  - Arrays can be indexed and accessed by arbitrary keys instead of
   raw integer index too.

- **block**: fixed set of named fields of various types that together
  take up a data region in the file. Fields are accessed in expression
  syntax by joining the block expression and the field name with a '.'
  (e.g. `my_block.a_field`). Implemented block types are **struct** and
  **union**, which are roughly equivalent to those of C language (see
  **struct** and **union** for details)

- **box**: internal structure that represents a **data container**. A box
   has a parent box (unless it's the root box), and various offsets to
   track the different limits set in various ways.

- **byte array**: byte range in the source file. Follows the same
   principle than **array** except that items are individual bytes.

- **data container**: any data item that can or does contain child items
   (arrays or structures/unions)

- **data tree**: root data container representing the whole file
   contents (always a struct type).

- **dpath**: data path, basically refers to an item mapped to a data
   region in the binary file, though keeps track of the canonical path
   that leads to this item.

- **expression**: any expression containing operators and operands
    that returns a value. Expressions can also return a **dpath**
    which can be used where a value is expected (it will then trigger
    a read in the data at the dpath location).

- **interpreter**: code that reads a data region and transforms it
    into a typed expression value. A typical interpreter is "integer":
    it returns an actual integer number from a series of bytes and
    some configuration parameters (endianness and sign notably). The
    returned value can then be embedded into an expression that
    expects a number, e.g. to define the number of items in an array.

- **schema**: definition of the binary file structure, written in
   bitpunch syntax (usually in files with .bp extension)

- **slack container**: container that has no predefined size: it takes
   as much space as possible from the slack space allocated from the
   parent. An array that has no set size (e.g. [] byte) and no
   interpreter-defined size (like null-terminated strings) is a slack
   container.

- **slack space**: space allocated by a parent container to its children
   that have no predefined size

- **slice**: range of items in an array (regular or byte array). They
   are similar to how Python defines them:

  - defined by an optional start and end boundary, delimited by ':' in
    expressions

  - boundaries can be negative, in which case they point to the n-th
    last element

- **struct**: subtype of **block** similar to C structures, they are
   data containers that contain a series of named items which are
   packed one after the other in the data file. Structures are always
   packed, any alignment constraint must be explicit by adding padding
   byte arrays where appropriate.

- **tracker**: object that can be used to track items across a binary
   file. It can be seen as a multi-layer key-value iterator that can
   enter and leave sub-items at will.

- **union**: subtype of **block** similar to C unions, they are data
    containers that contain a series of named items which all begin at
    the same offset location which is also the offset of the
    union. The size spanned by the union is the size of its largest
    sub-item. Unions can be used when a data region can be interpreted
    in multiple different ways. In most cases though the region has a
    format that is set depending on the value of some other field, in
    which case using a conditional (`if`) is more appropriate.
