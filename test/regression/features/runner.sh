#!/bin/bash
cd test/regression/features
for f in *.run; do
	./"$f" > "${f%.run}.res"
done
