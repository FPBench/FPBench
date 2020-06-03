#!/bin/sh

script_dir="tests/scripts/"
tmp_dir="/tmp/"

target="${script_dir}batch.txt"
test="${script_dir}test-toolserver.txt"

exp_file="${script_dir}export.fpcore"
trans_file="${script_dir}transform.fpcore"
out_name="${tmp_dir}test-toolserver"

# setup
rm -f $test
cd "$(dirname "$0")/../.."

# testing
cat $target | sed -e "s,EXPORT,$exp_file,g" | sed -e "s,TRANSFORM,$trans_file,g" | sed -e "s,OUTNAME,$out_name,g" | racket toolserver.rkt 2>> $test

# compare
if [ -s "$test" ]
then
    cat $test
    rm $test
    exit 1
else 
    rm $test
    exit 0
fi