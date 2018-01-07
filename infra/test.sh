#!/bin/sh

set -e

rm -rf reports d2h
mkdir -p d2h/ reports/
git clone -b develop https://github.com/uwplse/herbie.git d2h/herbie/
git clone git-rts@gitlab.mpi-sws.org:AVA/daisy.git d2h/daisy/
[ ! -h benchmarks/herbie ] && ln -s $PWD/d2h/herbie/bench benchmarks/herbie
(cd d2h/daisy && sbt compile script)

d2h () {
    NAME=$1
    shift
    echo "d2h $@ > $NAME"
    mkdir -p reports/save/$NAME
    python3 tools/daisy_herbie.py \
            --extra-preconditions infra/extra-preconditions.sexp \
            --save reports/save/$NAME \
            --timeout 120 \
            --daisy-flags="$*" \
            --herbie-flags="--seed '#(2172947836 2980513634 3572697779 4041609756 2019117808 48320163)'" \
            d2h/herbie/ d2h/daisy/ \
            2>reports/$NAME.error | tee reports/$NAME.csv || echo "Python failed" >&2
}

d2h ival    --rangeMethod=interval
d2h subdiv  --rangeMethod=interval --subdiv
d2h smt     --rangeMethod=smt --solver=dReal
d2h rewrite --rangeMethod=smt --rewrite --solver=dReal
d2h dynamic --dynamic

bash infra/graphs.sh reports/*.csv || echo "Graphs failed!" >&2
