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

LIBRARY_PATH=${LIBCMYTH}:${LIBCPPMYTH}:${LIBREFMEM}

export LD_LIBRARY_PATH=${LIBRARY_PATH}
export DYLD_LIBRARY_PATH=${LIBRARY_PATH}
export CMYTH_SWIGLIB=${SWIGDIR}/libcmyth_java.so

cd ${TESTDIR}

clojure -classpath ${SWIGDIR}/cmyth.jar test_clojure.clj $@
