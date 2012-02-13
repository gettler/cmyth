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

TESTDIR=${TOP}/test/test_java.class
LIBDIR=${INSTDIR}/lib

export LD_LIBRARY_PATH=${LIBDIR}

cd ${TESTDIR}

java -classpath ${TESTDIR}:${LIBDIR}/cmyth.jar -Djava.library.path=${LIBDIR} test_java $@
