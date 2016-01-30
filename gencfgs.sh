#!/bin/bash

#program to print CFGs
PGM=$1

#directory to store CFGs
DIR=$2

mkdir $DIR
./main.native $PGM -cfgs $DIR

cp lsdot.php $DIR/index.php
cd $DIR
for dotfile in *.dot
do
  echo $dotfile
  dot -Tpng $dotfile > $dotfile.png
done
