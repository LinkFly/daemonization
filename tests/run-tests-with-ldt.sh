#!/bin/sh
cd "$(dirname "$(readlink -e "$0")")"
LISP_RUNNER=run-lisp NOINFORM=' ' ./run-tests.sh
