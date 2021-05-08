#!/bin/bash
cd ./regression/features
for f in *.rkt; do
	racket "$f" > "${f%.rkt}.ref"
done
