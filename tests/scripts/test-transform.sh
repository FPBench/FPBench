#!/bin/sh

# Arguments
# (none)        test tool
# 'generate'    generate expected output
#

script_dir="tests/scripts/"
tmp_dir="/tmp/"

target="${script_dir}transform.fpcore"
target2="${script_dir}transform2.fpcore"
target3="${script_dir}transform3.fpcore"
output="${tmp_dir}transform.fpcore"
test="${script_dir}test-transform.txt"
expected="${script_dir}test-transform.out.txt"

# setup

cd "$(dirname "$0")/../.."
if [ "$1" = "generate" ]
then
    echo "Generating expected output for test-transform.sh..."
    test=$expected
fi
rm -f $test

# testing
racket transform.rkt --unroll 5 $target $output 2>> $test
cat $output >> $test

racket transform.rkt --skip-loops $target $output 2>> $test
cat $output >> $test

racket transform.rkt --precondition-range $target $output 2>> $test
cat $output >> $test

racket transform.rkt --expand-while* $target $output 2>> $test
cat $output >> $test

racket transform.rkt --expand-let* $target $output 2>> $test
cat $output >> $test

racket transform.rkt --cse $target2 $output 2>> $test
cat $output >> $test

racket transform.rkt --subexprs $target2 $output 2>> $test
cat $output >> $test

racket transform.rkt --propagate-clear --canonicalize $target3 $output 2>> $test
cat $output >> $test

racket transform.rkt --canonicalize-clear --canonicalize $target3 $output 2>> $test
cat $output >> $test

racket transform.rkt --propagate-default --canonicalize $target3 $output 2>> $test
cat $output >> $test

racket transform.rkt --canonicalize-default --canonicalize $target3 $output 2>> $test
cat $output >> $test

racket transform.rkt --propagate-clear +p precision --canonicalize $target3 $output 2>> $test
cat $output >> $test

racket transform.rkt -p precision --canonicalize $target3 $output 2>> $test
cat $output >> $test

racket transform.rkt --canonicalize-clear +c pre --canonicalize $target3 $output 2>> $test
cat $output >> $test

racket transform.rkt -c pre --canonicalize $target3 $output 2>> $test
cat $output >> $test

racket transform.rkt -condense $target3 $output 2>> $test
cat $output >> $test

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