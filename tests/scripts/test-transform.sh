#!/bin/sh

script_dir="tests/scripts/"
tmp_dir="/tmp/"

target="${script_dir}transform.fpcore"
target2="${script_dir}transform2.fpcore"
output="${tmp_dir}transform.fpcore"
test="${script_dir}test-transform.txt"
expected="${script_dir}test-transform.out.txt"

# setup
rm -f $test
cd "$(dirname "$0")/../.."

# testing
racket transform.rkt --unroll 5 $target $output 2>> $test
cat $output >> $test

racket transform.rkt --skip-loops $target $output 2>> $test
cat $output >> $test

racket transform.rkt --precondition-range $target $output 2>> $test
cat $output >> $test

racket transform.rkt --expand-while* --expand-let* $target $output 2>> $test
cat $output >> $test

racket transform.rkt --cse $target2 $output 2>> $test
cat $output >> $test

racket transform.rkt --subexprs $target2 $output 2>> $test
cat $output >> $test

# compare
ret=$(cmp -s $test $expected)
rm $test
exit $ret