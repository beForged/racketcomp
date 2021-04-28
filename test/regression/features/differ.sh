#! /bin/bash

for f in *.run; do
    echo $f
    diff "${f%.run}.res" "${f%.run}.ref"
done
