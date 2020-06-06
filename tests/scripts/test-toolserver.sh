#!/bin/sh

fpcore_dir="tests/scripts/fpcores/"
script_dir="tests/scripts/"
tmp_dir="/tmp/"

target="${script_dir}batch.txt"
test="${script_dir}test-toolserver.txt"
expected="${script_dir}test-toolserver.out.txt"

exp_file="${fpcore_dir}export.fpcore"
trans_file="${fpcore_dir}transform.fpcore"
eval_file="${fpcore_dir}eval2.fpcore"
out_name="${tmp_dir}test-toolserver"

# setup

cd "$(dirname "$0")/../.."
if [ "$1" = "generate" ]
then
    echo "Generating expected output for test-transform.sh..."
    test=$expected
fi
rm -f $test

# testing
cat $target | sed -e "s,EXPORT,$exp_file,g" | sed -e "s,TRANSFORM,$trans_file,g" | sed -e "s,EVALUATE,$eval_file,g" | sed -e "s,OUTNAME,$out_name,g" | racket toolserver.rkt 2>> $test
cat "${out_name}.txt" >> $test

# compare
if [ "$1" != "generate" ]
then
    cmp -s $test $expected
    ret=$?
    if [ "$ret" -ne 0 ]
    then
        diff $test $expected
    fi
    rm $test
    exit $ret
else
    echo "Done"
fi