#!/bin/bash
cd src
for file in ../test/regression/features/*.rkt; do
	make "${file%.rkt}.run"
done

cd ../test/regression/features
for f in *.run; do
	./"$f" > "${f%.run}.res"
done
for f in *.rkt; do
	racket "$f" > "${f%.rkt}.ref"
done

for f in *.res; do
	diff "$f" "${f%.res}.ref" &> /dev/null
	es=$?
	if [ $es -ne 0 ]; then
		echo "${f%.res} bad" 
	else
		echo "${f%.res} : test passed"
	fi
done
