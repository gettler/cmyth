#!/bin/bash

TOP=`git rev-parse --show-toplevel`

INSTDIR=${TOP}/install
TESTDIR=${TOP}/test
LIBDIR=${INSTDIR}/lib
PHPDIR=${INSTDIR}/lib/php

export LD_LIBRARY_PATH=${LIBDIR}

php -d enable_dl=On -d extension_dir=${PHPDIR} -d include_path=${PHPDIR} < ${TESTDIR}/test_php.php 2> /dev/null
