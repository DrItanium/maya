#!/bin/bash

make clean
rm -rf *.pail *.s *.o asmtool_*
make -j12
mkdir aux
mv asmtool_* aux/
pushd aux
gcc -c -o asmtool_senddata.o asmtool_senddata.c
popd
make asmlink
