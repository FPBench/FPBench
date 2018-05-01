#!/bin/sh

set -e -x

THREADS=6

rm -rf reports d2h
mkdir -p d2h/ reports/
git clone -b develop https://github.com/uwplse/herbie.git d2h/herbie/
git clone git-rts@gitlab.mpi-sws.org:AVA/daisy.git d2h/daisy/
[ ! -h benchmarks/herbie ] && ln -s $PWD/d2h/herbie/bench benchmarks/herbie
(cd d2h/daisy && sbt compile script)

mkdir reports/save
python3 tools/daisy_herbie.py \
        --extra-preconditions infra/extra-preconditions.sexp \
        --save reports/save/ \
        --timeout 600 \
        --herbie-flags="--threads $THREADS" \
        --daisy-flags="--rangeMethod=interval --subdiv" \
        --daisy-flags="--rangeMethod=smt --solver=z3" \
        --daisy-flags="--rangeMethod=interval --subdiv --rewrite" \
        --daisy-flags="--rangeMethod=smt --solver=z3 --rewrite" \
        d2h/herbie/ d2h/daisy/ \
        2>reports/error.log | tee reports/log.csv || echo "Python failed" >&2

bash infra/graphs.sh reports/log.csv reports/log.csv reports/log.csv reports/log.csv reports/log.csv reports/log.csv reports/log.csv || echo "Graphs failed!" >&2
