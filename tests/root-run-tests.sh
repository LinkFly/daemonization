#!/bin/sh
sbcl --noinform --load run-tests.lisp --eval "(root-run-tests \"$1\")"