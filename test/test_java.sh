#!/bin/bash

TOP=`git rev-parse --show-toplevel`

if [ "${PREFIX}" == "" ] ; then
    INSTDIR=${TOP}/install
else
    INSTDIR=${PREFIX}
fi

TESTDIR=${TOP}/test/test_java.class
LIBDIR=${INSTDIR}/lib

export LD_LIBRARY_PATH=${LIBDIR}

cd ${TESTDIR}

java -classpath ${TESTDIR}:${LIBDIR}/cmyth.jar test_java $@
