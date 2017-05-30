#!/usr/bin/env python

from distutils.core import setup, Extension

model = Extension('bitpunch.model.model_ext',
                  sources = ['pythonlib/bitpunch/model/modelmodule.c'],
                  include_dirs = ['libbitpunch/include', '.'],
                  library_dirs = ['build/lib'],
                  libraries = ['bitpunch'],
                  define_macros=[('DEBUG', None)])

setup (name = 'bitpunch',
       version = '0.1',
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
                            'resources/bp/__init__.py',
                            'resources/bp/*.bp']},
       packages = ['nestedcmd', 'bitpunch.model', 'bitpunch', 'bitpunch_cli'],
       py_modules = [],
       ext_modules = [model])
