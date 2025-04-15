#!/bin/bash

rm -r test
mkdir test
cp $1 test/grammar.js
cd test
tree-sitter generate
