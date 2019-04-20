#!/bin/bash
# Usage: ./compile {source} {output}
 
echo "Compiling $1 to $2"

# Load source, remove newlines, and compile to llvm ir
cat $1 | tr "\n" " " | dune exec ./bin/cmp.exe 2> $1.bc
# Compile llvm to x86-64
llc-8 -O3 -filetype=obj $1.bc
# Use gcc's linker to build ELF
gcc $1.o -o $2
# Cleanup
rm $1.o
if [ $# != 3 ]
then
	rm $1.bc
fi
