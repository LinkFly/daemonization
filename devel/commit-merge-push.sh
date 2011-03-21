#!/bin/bash
cd $(dirname $0)
cd ..
git add .
git commit -am "$1"
git checkout master
git merge devel
git push
git checkout devel
