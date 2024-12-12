#!/bin/sh

# Arguments
# (none)        test tool
# 'generate'    generate expected output
#

fpcore_dir="tests/sanity/"
script_dir="tests/scripts/"
tmp_dir="/tmp/"

target="${fpcore_dir}ops.fpcore"
output="${tmp_dir}eval.txt"
test="${script_dir}test-filter.txt"
expected="${script_dir}test-filter.out.txt"

# setup

cd "$(dirname "$0")/../.."
if [ "$1" = "generate" ]
then
    echo "Generating expected output for test-filter.sh..."
    test=$expected
fi
rm -f $test

# Single query

racket infra/filter.rkt -i ${target} operator:pow >> ${test}
racket infra/filter.rkt -i ${target} -o ${output} operator:sqrt  >> ${test}
cat ${output} >> ${test}

# Multi-query

racket infra/filter.rkt -i ${target} operator:if operator:let >> ${test}
racket infra/filter.rkt -i ${target} -o ${output} operator:+ operator:if >> ${test}
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