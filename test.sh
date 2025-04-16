#!/bin/bash

rm -r test
mkdir test
cp samples/TreeSitter/$1 test/grammar.js
cd test
tree-sitter generate
# tree-sitter parse ../$2
