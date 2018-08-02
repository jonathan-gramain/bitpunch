#!/usr/bin/env python

import os
from distutils.core import setup, Extension

try:
    build_dir = os.environ['BITPUNCH_BUILD_DIR']
except KeyError:
    build_dir = 'build'

model = Extension('bitpunch.model.model_ext',
                  sources = ['pythonlib/bitpunch/model/modelmodule.c'],
                  include_dirs = ['libbitpunch/include', '.'],
                  library_dirs = ['{}/lib'.format(build_dir)],
                  libraries = ['bitpunch'],
                  define_macros=[
                      ('DEBUG', None),
                      ('BUILD_DIR', build_dir),
                      ('PATH_TO_PARSER_TAB_H',
                       '"{}/libbitpunch/tmp/core/parser.tab.h"'
                       .format(build_dir))
                  ],
                  extra_compile_args=['-O0'])

setup (name = 'bitpunch',
       version = '0.1.0',
       author = 'Jonathan Gramain',
       author_email = 'jonathan.gramain@gmail.com',
       classifiers = [
           'Development Status :: 3 - Alpha',
           'Environment :: Console',
           'License :: OSI Approved :: BSD License',
           'Operating System :: POSIX :: Linux',
           'Programming Language :: C',
           'Programming Language :: Python',
           'Topic :: Software Development :: Libraries :: Python Modules',
           'Topic :: Software Development :: Interpreters',
           'Topic :: System :: Recovery Tools',
       ],
       description = 'BitPunch, a Swiss Army knife for structured binary files (Python library)',
       package_dir = {'': 'pythonlib'},
       package_data = {'': ['resources/__init__.py',
                            'resources/bp/*',
                            'resources/bp/*/*',
                            'resources/bp/*/*/*']},
       packages = ['nestedcmd', 'bitpunch.model', 'bitpunch', 'bitpunch_cli'],
       py_modules = [],
       ext_modules = [model])
