#!/bin/bash

TOP=`git rev-parse --show-toplevel`

if [ "${TOP}" == "--show-toplevel" ] ; then
    TOP=`pwd`
fi

LIBCMYTH=${TOP}/libcmyth
LIBCPPMYTH=${TOP}/libcppmyth
LIBREFMEM=${TOP}/librefmem

TESTDIR=${TOP}/test
SWIGDIR=${TOP}/swig

export PERLLIB=${SWIGDIR}/perl

LIBRARY_PATH=${LIBCMYTH}:${LIBCPPMYTH}:${LIBREFMEM}:${SWIGDIR}

export LD_LIBRARY_PATH=${LIBRARY_PATH}
export DYLD_LIBRARY_PATH=${LIBRARY_PATH}

${TESTDIR}/test_perl.pm $@
