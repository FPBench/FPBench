#!/bin/sh

set -e
set -x

TIMEOUT=600
THREADS=6

rm -rf reports d2h
mkdir -p d2h/ reports/

git clone -b develop https://github.com/uwplse/herbie.git d2h/herbie/
git clone git-rts@gitlab.mpi-sws.org:AVA/daisy.git d2h/daisy/
[ ! -h benchmarks/herbie ] && ln -s $PWD/d2h/herbie/bench benchmarks/herbie
(cd d2h/daisy && sbt compile script)

HERBIE=d2h/herbie
DAISY=d2h/daisy
FPBENCH=.

mkdir reports/save
WHERE="$PWD"/reports/save

cat benchmarks/*.fpcore | \
    racket "$FPBENCH"/tools/filter.rkt operations + - '*' / exp log sin cos let sqrt tan | \
    racket "$FPBENCH"/tools/filter.rkt pre > "$WHERE"/input.fpcore

printf "Input benchmarks: "; grep FPCore "$WHERE/input.fpcore" | wc -l
racket "$HERBIE"/src/herbie.rkt improve --timeout "$TIMEOUT" --threads "$THREADS" "$WHERE"/input.fpcore "$WHERE"/output.fpcore
printf "Ouput benchmarks: "; grep FPCore "$WHERE/input.fpcore" | wc -l

racket "$FPBENCH"/tools/core2scala.rkt <"$WHERE"/input.fpcore >"$WHERE"/input.scala
racket "$FPBENCH"/tools/core2scala.rkt <"$WHERE"/output.fpcore >"$WHERE"/output.scala

OLD="$PWD"
cd "$DAISY"
./daisy --results-csv=input.1.csv  --rangeMethod=interval --subdiv "$WHERE"/input.scala
./daisy --results-csv=output.1.csv --rangeMethod=interval --subdiv "$WHERE"/output.scala
./daisy --results-csv=input.2.csv  --rangeMethod=smt --solver=z3 "$WHERE"/input.scala
./daisy --results-csv=output.2.csv --rangeMethod=smt --solver=z3 "$WHERE"/output.scala
./daisy --results-csv=input.3.csv  --rangeMethod=interval --subdiv --rewrite "$WHERE"/input.scala
./daisy --results-csv=output.3.csv --rangeMethod=interval --subdiv --rewrite "$WHERE"/output.scala
./daisy --results-csv=input.4.csv  --rangeMethod=smt --solver=z3 --rewrite "$WHERE"/input.scala
./daisy --results-csv=output.4.csv --rangeMethod=smt --solver=z3 --rewrite "$WHERE"/output.scala
cd "$OLD"

cp "$DAISY"/output/*.csv "$WHERE"
