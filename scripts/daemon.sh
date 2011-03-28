#!/bin/sh -e
cd $(dirname $0)
sbcl --noinform --noprint --load daemon.lisp $1 $2
