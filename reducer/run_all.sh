#!/bin/bash

PGMS=$1
NCPU=$2
SPARROW="/Users/seongjoon/pl/sparrow/analyzer/main.native"
mkdir -p ./reduced

for src in $PGMS/*.c
do
	echo "inserting observe - $src" >> log
	PGM_NAME=`basename $src`
	mkdir -p "./reduced/$PGM_NAME"
	$SPARROW -insert_observe $src -dir "./reduced/$PGM_NAME"

	for file in ./reduced/$PGM_NAME/*.c
	do
		echo "reducing - $file" >> log
		./run.sh $file $NCPU
		cp reduce.c "$file.red.c"
		echo "reducing - $file ... done" >> log
	done
done

