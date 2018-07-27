#!/bin/bash

BUILD_DIR=${BITPUNCH_BUILD_DIR:-build}

PYTHONPATH=${BUILD_DIR}/lib.linux-x86_64-2.7 \
          LD_LIBRARY_PATH=${BUILD_DIR}/lib \
          py.test $(dirname $0)/unit/pytest
