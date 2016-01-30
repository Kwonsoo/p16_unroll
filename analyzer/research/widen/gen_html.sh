#!/bin/bash

DIR=$1

echo "<style>
table, th, td {
}
</style>
"
echo "<table>"

echo "<tr> <th> Program </th> <th> Proven</th> <th>Time</th> <th>Proven</th> <th>Time</th> </tr>"

for pgm in /home/hakjoo/benchmarks/100/*.c
do
  filename=`basename $pgm`

  echo "<tr>"
    echo "<td>" # program 
    echo $filename
    echo "</td>"

    echo "<td>" # proven
    cat $DIR/$filename.wo.threshold | grep "#proven" | cut -d ":" -f 2
    echo "</td>"

    echo "<td>"
    tail -1 $DIR/$filename.wo.threshold 
    echo "</td>"

    echo "<td>" # proven
    cat $DIR/$filename.with.threshold.0.256 | grep "#proven" | cut -d ":" -f 2
    echo "</td>"

    echo "<td>"
    tail -1 $DIR/$filename.with.threshold.0.256
    echo "</td>"



  echo "</tr>"
done

echo "</table>"
