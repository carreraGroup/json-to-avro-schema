#! /bin/bash

source=${1?"Usage $0 source dest"}
dest=${2?"Usage $0 source dest"}

for f in src/test/resources/integration-tests/$source.*; do
    cp $f ${f/$source/$dest}
done
