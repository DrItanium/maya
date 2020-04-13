#!/bin/bash

make clean
rm -f *.pail *.s *.o asmtool_* aux/
make -j12
mkdir aux
mv asmtool_* aux/
pushd aux
gcc -c -o asmtool_senddata.o asmtool_senddata.c
popd
make asmlink
