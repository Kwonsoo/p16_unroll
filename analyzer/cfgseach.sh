#!/bin/bash

DIR=$1

for file in $DIR/*.cfg
do
	dot -Tpng $file > $file.png
	rm $file
done
