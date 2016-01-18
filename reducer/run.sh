#!/bin/bash

FILE=$1
NCPU=$2
SPARROW="/Users/seongjoon/pl/sparrow/analyzer/main.native"

cp $FILE "./reduce.c"

CONDITION=`$SPARROW "reduce.c" | grep "(AIRAC_OBSERVE)"`

echo "#!/bin/bash" > script.sh
echo "gcc -c -Wextra reduce.c &&\\" >> script.sh
echo "$SPARROW reduce.c | grep -F \"$CONDITION\"" >> script.sh
chmod u+x script.sh

creduce -n $NCPU script.sh reduce.c 
