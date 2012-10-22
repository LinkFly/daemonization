#!/bin/sh
cd "$(dirname "$(readlink -e "$0")")"
LISP_RUNNER=${LISP_RUNNER:-sbcl}
NOINFORM=${NOINFORM:-'--noinform'}
LOAD=${LOAD:-'--load'}
EVAL=${EVAL:-'--eval'}
$LISP_RUNNER $NOINFORM $LOAD $(pwd)/run-tests.lisp $EVAL "(root-run-tests \"$1\")"

