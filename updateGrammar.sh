#!/bin/bash

rm -r $1
mkdir $1
cd $1
bnfc --haskell ../samples/LBNF/$1.cf
