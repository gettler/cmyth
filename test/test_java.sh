#!/bin/bash

TOP=`git rev-parse --show-toplevel`

INSTDIR=${TOP}/install
TESTDIR=${TOP}/test
LIBDIR=${INSTDIR}/lib

export LD_LIBRARY_PATH=${LIBDIR}

cd ${TESTDIR}

java -classpath ${TESTDIR}:${LIBDIR}/cmyth.jar test_java
