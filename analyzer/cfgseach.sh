#!/bin/bash

DIR=$1

for file in $DIR/*.dot
do
	dot -Tpng $file > $file.png
	rm $file
done
