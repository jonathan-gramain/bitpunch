# Install and use

Tested on Ubuntu 14.04 LTS.

Not tested on other platforms, should work on other linux distribs
with minimal changes.


## Dependencies

### Runtime dependencies

- python 2.x (tested with 2.7)

- python modules readline, hexdump, kmd (pip install readline hexdump kmd)

### Development dependencies

#### Build dependencies

- GNU toolchain (gcc, make)

- pkg-config

- flex

- bison

- python distutils

#### Testing

- py.test (http://pytest.org, Ubuntu package "python-pytest")

- "check", C unit testing framework
  (https://libcheck.github.io/check/, Ubuntu package "check")


## Building

To compile everything, run:

```
make
make
```

Yes, run it twice, a dependency issue makes the first one complain
about a missing generated file but creates it anyway (did not yet take
the time to find a workaround for this).

## Getting started

To run the CLI:

```
./bitpunch
bitpunch> 
```

or to run it in debug mode with GDB:

```
./bitpunch.debug
bitpunch> 
```

You can start computing expressions right away:

```
bitpunch> print 42*2
84
bitpunch>
```

To start wandering around a binary file, get going with:

```
bitpunch> file /path/to/my/binary/file [/path/to/my/binary/file/schema.bp]
```

If you don't specify a BP file, bitpunch will try to find a match with
one of the BP files in its internal library which name without the
final .bp matches the file extension (OK this is too dumb, will be
reworked at some point).

No output on console is good news, You can check anyway if everything
has loaded fine:

```
bitpunch> file
file   -> /path/to/my/binary/file
format -> /path/to/my/binary/file/schema.bp
```

Then you can use a series of commands to start exploring or extracting
the contents. Try `print` or `xdump` (`help` or `help command` can
help too). &lt;TAB&gt; can help with autocompletion in most contexts.

All commands can be typed as a prefix if it is unambiguous (otherwise
the CLI complains and gives you the choices to disambiguate). So
e.g. you can print with `p` or do a hex dump of some item with `x`.

The CLI is based on readline, so any line-editing feature supported by
the default readline configuration is available (including a history
of recent commands).


## Testing

To run all test, type in:

```
make check
```

- Some tests are using the check C unit testing framework
(https://libcheck.github.io/check)

  - They can be found in the tests/unit/check directory

- Other tests are built with py.test (http://pytest.org)

  - They are located under tests/unit/pytest    
