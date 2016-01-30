#!/bin/bash

DIR=$1

echo "=Without threshold="
echo "#proven"
cat $DIR/*.wo.threshold | grep "#proven" | cut -d ":" -f 2 | python -c "import sys; print sum(int(l) for l in sys.stdin)"
echo "#time"

rm /tmp/time
for pgm in $DIR/*.wo.threshold
do
  tail -1 $pgm >> /tmp/time
done
cat /tmp/time | python -c "import sys; print sum(float(l) for l in sys.stdin)"

echo ""
echo "=With threshold="
echo "#proven"
cat $DIR/*.with.threshold.0.256 | grep "#proven" | cut -d ":" -f 2 | python -c "import sys; print sum(int(l) for l in sys.stdin)"
echo "#time"

rm /tmp/time
for pgm in $DIR/*.with.threshold.0.256
do
  tail -1 $pgm >> /tmp/time
done
cat /tmp/time | python -c "import sys; print sum(float(l) for l in sys.stdin)"
