#!/bin/bash

TOP=`git rev-parse --show-toplevel`

if [ "${TOP}" == "--show-toplevel" ] ; then
    TOP=`pwd`
fi

if [ "${PREFIX}" == "" ] ; then
    INSTDIR=${TOP}/install
else
    INSTDIR=${PREFIX}
fi

TESTDIR=${TOP}/test
PYTHONDIR=${INSTDIR}/lib/python
LIBDIR=${INSTDIR}/lib

export PYTHONPATH=${PYTHONDIR}
export LD_LIBRARY_PATH=${PYTHONDIR}:${LIBDIR}
export DYLD_LIBRARY_PATH=${PYTHONDIR}:${LIBDIR}

${TESTDIR}/test_python.py $@
