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
LISPDIR=${LIBDIR}/lisp/

export LD_LIBRARY_PATH=${PYTHONDIR}:${LIBDIR}
export DYLD_LIBRARY_PATH=${PYTHONDIR}:${LIBDIR}

export LISPDIR

SBCL=`which sbcl`
CLISP=`which clisp`
ECL=`which ecl`

if [ "$SBCL" != "" ] ; then
    sbcl --noinform --non-interactive --load ${LISPDIR}/cmyth.lisp --load ${TESTDIR}/test_lisp.lisp $@
elif [ "$CLISP" != "" ] ; then
    clisp -q -q ${TESTDIR}/test_lisp.lisp -- $@
elif [ "$ECL" != "" ] ; then
    ecl -q -load ${LISPDIR}/cmyth.lisp -load ${TESTDIR}/test_lisp.lisp -eval "(quit)" -- $@
else
    echo "No supported Lisp implementation found!"
fi
