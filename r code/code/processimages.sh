#!/bin/bash
mkdir imgpng2
for f in imgpng/*.png
do 
    b=`basename $f`
    echo "resizing imgpng/$b --> imgpng2/$b"
    convert imgpng/$b -resize 118x118 imgpng2/$b
done

mkdir imgpng3
for f in imgpng2/*.png
do
    b=`basename $f`
    echo "adding border imgpng2/$b --> imgpng3/$b"
    convert imgpng2/$b -bordercolor "#FFFFFF" -border 1x1 imgpng3/$b
done

mkdir imgpng4
for f in imgpng2/*.png
do
    b=`basename $f .png`
    o=imgpng4/${b}_small.png
    echo "processing $f --> $o"
    convert $f -resize 5x5 $o
    mogrify -resize 78x78 $o
    mogrify -bordercolor "#FFFFFF" -border 1x1 $o
done

mv imgpng4/*.png imgpng3
mv imgpng3 allpng mkdir imgpng2
for f in imgpng/*.png
do 
    b=`basename $f`
    echo "resizing imgpng/$b --> imgpng2/$b"
    convert imgpng/$b -resize 118x118 imgpng2/$b
done

mkdir imgpng3
for f in imgpng2/*.png
do
    b=`basename $f`
    echo "adding border imgpng2/$b --> imgpng3/$b"
    convert imgpng2/$b -bordercolor "#FFFFFF" -border 1x1 imgpng3/$b
done

mkdir imgpng4
for f in imgpng2/*.png
do
    b=`basename $f .png`
    o=imgpng4/${b}_small.png
    echo "processing $f --> $o"
    convert $f -resize 5x5 $o
    mogrify -resize 78x78 $o
    mogrify -bordercolor "#FFFFFF" -border 1x1 $o
done
