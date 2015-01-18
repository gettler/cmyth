#!/bin/sh

TOP=`git rev-parse --show-toplevel`

if [ "${TOP}" = "--show-toplevel" ] ; then
    TOP=`pwd`
fi

TESTDIR=${TOP}/test

LIBCMYTH=${TOP}/libcmyth
LIBCPPMYTH=${TOP}/libcppmyth
LIBREFMEM=${TOP}/librefmem

LIBRARY_PATH=${LIBCMYTH}:${LIBCPPMYTH}:${LIBREFMEM}

export LD_LIBRARY_PATH=${LIBRARY_PATH}
export DYLD_LIBRARY_PATH=${LIBRARY_PATH}

export CMYTH_MODULE=${TOP}/swig/cmyth

nodejs ${TESTDIR}/test_js $@
