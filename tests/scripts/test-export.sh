#!/bin/sh

script_dir="tests/scripts/"
tmp_dir="/tmp/"

target="${script_dir}export.fpcore"
output="${tmp_dir}export"
test="${script_dir}test-export.txt"
expected="${script_dir}test-export.out.txt"

# setup
rm -f $test
cd "$(dirname "$0")/../.."

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

racket export.rkt --lang wls $target "${output}.wls" 2>> $test
cat "${output}.wls" >> $test

racket export.rkt $target "${output}.sollya" 2>> $test
cat "${output}.sollya" >> $test

racket export.rkt $target "${output}.cml" 2>> $test
cat "${output}.cml" >> $test

# compare
ret=$(cmp -s $test $expected)
rm $test
exit $ret