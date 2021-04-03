#!/bin/bash
cd src
for file in ../test/regression/features/*.rkt; do
	echo "${file%.rkt}.run"
	make "${file%.rkt}.run"
done
