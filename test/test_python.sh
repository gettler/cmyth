#!/bin/bash

TOP=`git rev-parse --show-toplevel`

INSTDIR=${TOP}/install
TESTDIR=${TOP}/test
PYTHONDIR=${INSTDIR}/lib/python
LIBDIR=${INSTDIR}/lib

export PYTHONPATH=${PYTHONDIR}
export LD_LIBRARY_PATH=${PYTHONDIR}:${LIBDIR}

${TESTDIR}/test_python.py
