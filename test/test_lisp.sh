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

export LISPDIR=${SWIGDIR}/lisp/

SBCL=`which sbcl`
CCL=`which ccl`
ECL=`which ecl`
CLISP=`which clisp`

if [ "$SBCL" != "" ] ; then
    sbcl --noinform --non-interactive --load ${TESTDIR}/test_lisp.lisp $@
elif [ "$CCL" != "" ] ; then
    ccl --load ${TESTDIR}/test_lisp.lisp --eval "(quit)" -- $@
elif [ "$ECL" != "" ] ; then
    ecl -q -load ${TESTDIR}/test_lisp.lisp -eval "(quit)" -- $@
elif [ "$CLISP" != "" ] ; then
    clisp -q -q ${TESTDIR}/test_lisp.lisp -- $@
else
    echo "No supported Lisp implementation found!"
fi
