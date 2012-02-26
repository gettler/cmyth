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
PHPDIR=${INSTDIR}/lib/php

export LD_LIBRARY_PATH=${LIBDIR}
export DYLD_LIBRARY_PATH=${LIBDIR}

ARGS="-- $@"

php -d enable_dl=On -d extension_dir=${PHPDIR} -d include_path=${PHPDIR} ${ARGS} < ${TESTDIR}/test_php.php 2> /dev/null
