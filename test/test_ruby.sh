#!/bin/bash

TOP=`git rev-parse --show-toplevel`

INSTDIR=${TOP}/install
TESTDIR=${TOP}/test
RUBYDIR=${INSTDIR}/lib/ruby
LIBDIR=${INSTDIR}/lib

export RUBYLIB=${RUBYDIR}
export LD_LIBRARY_PATH=${RUBYDIR}:${LIBDIR}

${TESTDIR}/test_ruby.rb
