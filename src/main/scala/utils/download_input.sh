#!/bin/bash

DAY=$( date +"%d" | sed 's/^0*//' )
echo "Pulling input for day $DAY"
OUTPUT="../input/$DAY.txt"
echo "Writing to file: $OUTPUT"

URL="https://adventofcode.com/2021/day/$DAY/input"
SESSION="53616c7465645f5fc9fa3f55b9ce830ef91751a9a1452a6cc9cc692950c2cb8b1646f96b974d1a29761491e1fcc35713"

curl --location --request GET "$URL" --header "Cookie: session=$SESSION" -o "$OUTPUT"
echo "Happy Solving :)"

DESTINATION="/Users/bradleyking/dev/aoc2021/src/main/scala/solutions/day$DAY.scala"
cp "/Users/bradleyking/dev/aoc2021/src/main/scala/utils/template" $DESTINATION
sed -i.bak s/DAY/$DAY/g $DESTINATION
sed -i.bak s/utils$/solutions/g $DESTINATION
rm $DESTINATION.bak
