#!/bin/sh

git submodule init
git pull --recurse-submodules
git submodule update --recursive
