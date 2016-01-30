#!/bin/bash

#directory to store CFGs
DIR=$1
mkdir $DIR

cp lsdot.php $DIR/index.php
cd $DIR
for dotfile in *.dot
do
	echo $dotfile
	dot -Tpng $dotfile > $dotfile.png
done

