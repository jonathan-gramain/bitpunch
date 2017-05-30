#!/bin/bash

export PYTHONPATH=build/lib.linux-x86_64-2.7
export LD_LIBRARY_PATH=build/lib

py.test $(dirname $0)/unit/pytest
