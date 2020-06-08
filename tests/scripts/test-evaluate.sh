#!/bin/sh

# Arguments
# (none)        test tool
# 'generate'    generate expected output
#

fpcore_dir="tests/scripts/fpcores/"
script_dir="tests/scripts/"
tmp_dir="/tmp/"

target="${fpcore_dir}eval.fpcore"
target2="${fpcore_dir}eval2.fpcore"
output="${tmp_dir}eval.txt"
test="${script_dir}test-evaluate.txt"
expected="${script_dir}test-evaluate.out.txt"

# setup

cd "$(dirname "$0")/../.."
if [ "$1" = "generate" ]
then
    echo "Generating expected output for test-evaluate.sh..."
    test=$expected
fi
rm -f $test

# testing
cat ${target} | racket evaluate.rkt >> ${test}
cat ${target2} | racket evaluate.rkt 1.0 2.0 >> ${test}

racket evaluate.rkt -i ${target} >> ${test}
racket evaluate.rkt -i ${target2} 1.0 2.0 >> ${test}

racket evaluate.rkt -i ${target} -o ${output} >> ${test}
cat ${output} >> ${test}
racket evaluate.rkt -i ${target2} -o ${output} 1.0 2.0 >> ${test}
cat ${output} >> ${test}

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