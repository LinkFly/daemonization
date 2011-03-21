#!/bin/bash
git add .
git commit -am "$1"
git checkout master
git merge devel
git push
git checkout devel
