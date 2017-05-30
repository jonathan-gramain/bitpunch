# BitPunch slang

- **array**: series of concatenated items of varying length.

  - Unlike C arrays, bitpunch arrays can also hold variable-sized items

  - The length expression between brackets can be omitted, in which
    case the number of items will be determined by other means
    (interpreter-defined size, or available slack space)

  - Arrays can be indexed and accessed by arbitrary keys instead of
   raw integer index too.

- **box**: internal structure that represents a **data container**. A box
   has a parent box (unless it's the root box), and various offsets to
   track the different limits set in various ways.

- **byte array**: byte range in the source file. Follows the same
   principle than **array** except that items are individual bytes.

- **data container**: any data item that can or does contain child items
   (arrays or structures/unions)

- **data tree**: root data container representing the whole file
   contents (always a struct type).

- **dpath**: data path, basically refers to an item mapped to a range of
   bytes in the binary file, though keeps track of the canonical path
   that leads to this item.

- **schema**: definition of the binary file structure, written in
   bitpunch syntax (usually in files with .bp extension)

- **slack container**: container that has no predefined size: it takes
   as much space as possible from the slack space allocated from the
   parent. An array that has no set size (e.g. byte[]) and no
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

- **struct**: similar to C structures, they map onto a series of named
   items which are concatenated in the source file. Structures are
   always packed, any alignment must be explicit by adding padding
   byte arrays where appropriate.

- **tracker**: object that can be used to track items across a binary
   file. It can be seen as a multi-layer key-value iterator that can
   enter and leave sub-items at will.
