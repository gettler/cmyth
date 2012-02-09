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
LIBDIR=${INSTDIR}/lib

export LD_LIBRARY_PATH=${LIBDIR}

${TESTDIR}/test_cpp $@
