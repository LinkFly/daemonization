#!/bin/sh
cd "$(dirname $0)"
sbcl --noinform --load run-tests.lisp --eval "(root-run-tests \"$1\")"
