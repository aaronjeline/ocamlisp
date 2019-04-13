#!/bin/bash

echo "Compiling $1 to $2"

dune exec ./bin/cmp.exe < $1 2> $1.bc
llc-8 -filetype=obj $1.bc
gcc $1.o -o $2
# Cleanup
rm $1.o
if [ $# != 3 ]
then
	rm $1.bc
fi
