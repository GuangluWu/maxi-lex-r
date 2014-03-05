#!/bin/bash
for f in *.wav 
do 
    bname=`basename $f .wav`
    echo "processing $bname.wav -> $bname.mp3"
    sox -r 22050 -c 1 $bname.wav $bname.mp3
done
