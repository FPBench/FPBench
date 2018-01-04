#!/usr/bin/env bash

# exit on error
set -e

gnuplot -V > /dev/null 2>&1 \
  || { echo "ERROR: gnuplot is required."; exit 1; }

# names for CSV columns
readonly    BENCH_INDEX=1
readonly     BENCH_NAME=2
readonly      HERBIE_TM=3
readonly HERBIE_SRC_ERR=4
readonly HERBIE_RES_ERR=5
readonly   DAISY_SRC_TM=6
readonly   DAISY_RES_TM=7
readonly  DAISY_SRC_ERR=8
readonly  DAISY_RES_ERR=9
readonly          NCOLS=9

function main {
  while [ "$#" -gt 0 ]; do
    plot "$1"
    shift
  done
}

function plot {
  local data="$1"
  daisy_herbie_bar       "$data"
  herbie_time_improve    "$data"
  cmp_src_error_measures "$data"
  cmp_res_error_measures "$data"
}

function terminal {
  w=800
  h=600
  [ "$1" != "" ] && w="$1"
  [ "$2" != "" ] && h="$2"
  cat <<EOF
set terminal pngcairo dashed size $w,$h font "Helvetica,11"
EOF
}

function clean_csv {
  local data="$1"

  awk -F',' "
    NR == 1 || NF == $NCOLS && \$$DAISY_RES_ERR ~ /\./ {
      print \$0;
      next
    }

    {
      print \"Warning: skipping \"     \
        \$$BENCH_NAME \" in '$data' \" \
        \" (bad row [NF = \" NF \"])\" \
        > \"/dev/stderr\"
    }" \
  "$data"
}

function daisy_herbie_bar {
  local data="$1"

  local tmp="$(mktemp "$data.XXXXX")"
  clean_csv "$data" \
    | awk -F',' "
        NR == 1 {
          print \"name,err_rat\";
          next
        }

        \$$DAISY_SRC_ERR != 0.0 {
          print \$$BENCH_NAME \",\" \$$DAISY_RES_ERR/\$$DAISY_SRC_ERR;
          next
        }

        {
          print \"Warning: skipping \" \$$BENCH_NAME \" in '$data' (div by 0)\" \
            > \"/dev/stderr\"
        }" \
    > "$tmp"

  local maxY=1.5
  gnuplot <<EOF
$(terminal 3000 1000)
set datafile separator ","
set key autotitle columnhead

set style fill solid border -1
set boxwidth 0.75

set xtics nomirror
set xtics rotate by -45

set xlabel "Benchmark"
set ylabel "Error Change (res / src)"

set key horizontal top left
set linetype 1 linecolor rgb "#000099"

set output "$data.daisy_herbie_bar.png"
set title "Daisy Error Bound: post-Herbie / pre-Herbie ($data) [OUTLIER THRESHOLD $maxY]"

set yrange [0:$maxY]
plot "$tmp"         \
  using 0:2:xtic(1) \
  with boxes        \
  title "Error Ratio"
EOF
  rm "$tmp"
}

function cmp_src_error_measures {
  local data="$1"

  gnuplot <<EOF
$(terminal)
set datafile separator ","
set key autotitle columnhead

set xtics nomirror

set xlabel "Herbie Source Error"
set ylabel "Daisy Source Error"

set autoscale x
set autoscale y
set logscale  y

set output "$data.cmp_src_error_measures.png"
set title "Herbie Source Error vs. Daisy Source Error ($data)"

plot "$data" \
  using $HERBIE_SRC_ERR:$DAISY_SRC_ERR \
  notitle linecolor rgb "#000099" pointtype 7
EOF
}

function cmp_res_error_measures {
  local data="$1"

  gnuplot <<EOF
$(terminal)
set datafile separator ","
set key autotitle columnhead

set xtics nomirror

set xlabel "Herbie Result Error"
set ylabel "Daisy Result Error"

set autoscale x
set autoscale y
set logscale  y

set output "$data.cmp_res_error_measures.png"
set title "Herbie Result Error vs. Daisy Result Error ($data)"

plot "$data" \
  using $HERBIE_RES_ERR:$DAISY_RES_ERR \
  notitle linecolor rgb "#000099" pointtype 7
EOF
}

function herbie_time_improve {
  local data="$1"

  local maxY=1.5
  gnuplot <<EOF
$(terminal)
set datafile separator ","
set key autotitle columnheader

set xtics nomirror

set xlabel "Herbie Time"
set ylabel "Error Change (res / src)"

set output "$data.herbie_time_improve.png"
set title "Herbie Time vs. Daisy Error Improvement ($data) [OUTLIERS DROPPED $maxY]"

set yrange [0:$maxY]
plot "$data" \
  using $HERBIE_TM:(\$$DAISY_RES_ERR/\$$DAISY_SRC_ERR) \
  notitle linecolor rgb "#000099" pointtype 7
EOF
}

main "$@"
