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
RUBYDIR=${INSTDIR}/lib/ruby
LIBDIR=${INSTDIR}/lib

export RUBYLIB=${RUBYDIR}
export LD_LIBRARY_PATH=${RUBYDIR}:${LIBDIR}
export DYLD_LIBRARY_PATH=${RUBYDIR}:${LIBDIR}

${TESTDIR}/test_ruby.rb $@
