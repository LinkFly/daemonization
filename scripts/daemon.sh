#!/bin/sh -e
cd "$(dirname "$(readlink -e "$0")")"
LISP_RUNNER=${LISP_RUNNER:-sbcl}
NOINFORM=${NOINFORM:-'--noinform'}
LOAD=${LOAD:-'--load'}
$LISP_RUNNER $NOINFORM $LOAD $(pwd)/daemon.lisp $1 $2
