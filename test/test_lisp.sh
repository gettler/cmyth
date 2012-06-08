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
LISPDIR=${LIBDIR}/lisp

export LD_LIBRARY_PATH=${PYTHONDIR}:${LIBDIR}
export DYLD_LIBRARY_PATH=${PYTHONDIR}:${LIBDIR}

export LISPDIR

sbcl --noinform --non-interactive --load ${LIBDIR}/lisp/cmyth.lisp --load ${TESTDIR}/test_lisp.lisp $@

