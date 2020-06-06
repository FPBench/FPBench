#!/bin/sh

# Arguments
# (none)        test tool
# 'generate'    generate expected output
#

fpcore_dir="tests/scripts/fpcores/"
script_dir="tests/scripts/"
tmp_dir="/tmp/"

target="${fpcore_dir}export.fpcore"
output="${tmp_dir}export"
test="${script_dir}test-export.txt"
expected="${script_dir}test-export.out.txt"

# setup

cd "$(dirname "$0")/../.."
if [ "$1" = "generate" ]
then
    echo "Generating expected output for test-export.sh..."
    test=$expected
fi
rm -f $test

# testing
racket export.rkt --bare $target "${output}.c" 2>> $test
cat "${output}.c" >> $test

racket export.rkt --namespace "main" $target "${output}.go" 2>> $test
cat "${output}.go" >> $test

racket export.rkt --runtime "Custom" $target "${output}.js" 2>> $test
cat "${output}.js" >> $test

racket export.rkt --rel-error $target "${output}.gappa" 2>> $test
cat "${output}.gappa" >> $test

racket export.rkt --scale 2 $target "${output}.fptaylor" 2>> $test
cat "${output}.fptaylor" >> $test

racket export.rkt --lang wls $target "${output}.m" 2>> $test
cat "${output}.m" >> $test

racket export.rkt $target "${output}.sollya" 2>> $test
cat "${output}.sollya" >> $test

racket export.rkt $target "${output}.cml" 2>> $test
cat "${output}.cml" >> $test

racket export.rkt $target "${output}.scala" 2>> $test
cat "${output}.scala" >> $test

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