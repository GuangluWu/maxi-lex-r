#!/bin/bash -eu
b=`basename $1 .txt`
echo "processing $1..."
grep \ BEGIN $1 > response/$b.begin.txt
grep SELECT $1 > response/$b.select.txt
grep SYNCTIME $1 > response/$b.sync.txt
grep 'MOVE\|DOWN' $1 > response/$b.dat.txt
