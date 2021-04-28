#! /bin/bash

for f in *.run; do
    diff "${f%.run}.res" "${f%.run}.ref"
done
