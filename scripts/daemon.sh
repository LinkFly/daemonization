#!/bin/sh -e
cd $(dirname $0)
sbcl --noinform --load daemon.lisp $1 $2
