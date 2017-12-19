#!/usr/bin/env bash

# exit on error
set -e

gnuplot -V &> /dev/null \
  || { echo "ERROR: gnuplot is required."; exit 1; }

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

HERBIE_TM="3"
HERBIE_SRC_ERR="4"
HERBIE_RES_ERR="5"
DAISY_SRC_TM="6"
DAISY_RES_TM="7"
DAISY_SRC_ERR="8"
DAISY_RES_ERR="9"

function daisy_herbie_bar {
  local data="$1"

  gnuplot <<EOF
$(terminal 1500)
set datafile separator ","

set xtics    rotate by 45 right
set xtics    nomirror
set ytics    nomirror
set offsets  1, 1, 5, 0

set style fill       solid
set style data       histogram
set style histogram  clustered

set boxwidth 1

set key invert horizontal top left
set key autotitle columnheader

set xlabel "Benchmark"
set ylabel "Error Change (res / src)"

set logscale y

set output "$data.daisy_herbie_bar.png"
set title "Daisy error bound after Herbie / before Herbie ($data)"
plot "$data" using \
  (\$$DAISY_RES_ERR/\$$DAISY_SRC_ERR):xtic(2) \
  title "Error Ratio" linecolor rgb "#000099"
EOF
}

function cmp_src_error_measures {
  local data="$1"

  gnuplot <<EOF
$(terminal)
set datafile separator ","

set xtics    nomirror
set ytics    nomirror
set offsets  1, 1, 5, 0

set key autotitle columnheader
set key vertical top left box opaque width 1.5 samplen 0
set border back

set xlabel "Herbie Source Error"
set ylabel "Daisy Source Error"

set autoscale x
set logscale  y
set autoscale y

set output "$data.cmp_src_error_measures.png"
set title "Herbie Source Error vs. Daisy Source Error ($data)"
plot "$data" using \
  $HERBIE_SRC_ERR:$DAISY_SRC_ERR \
  notitle linecolor rgb "#000099" pointtype 7
EOF
}

function cmp_res_error_measures {
  local data="$1"

  gnuplot <<EOF
$(terminal)
set datafile separator ","

set xtics    nomirror
set ytics    nomirror
set offsets  1, 1, 5, 0

set key autotitle columnheader
set key vertical top left box opaque width 1.5 samplen 0
set border back

set xlabel "Herbie Result Error"
set ylabel "Daisy Result Error"

set autoscale x
set logscale  y
set autoscale y

set output "$data.cmp_res_error_measures.png"
set title "Herbie Result Error vs. Daisy Result Error ($data)"
plot "$data" using \
  $HERBIE_RES_ERR:$DAISY_RES_ERR \
  notitle linecolor rgb "#000099" pointtype 7
EOF
}

function herbie_time_improve {
  local data="$1"

  gnuplot <<EOF
$(terminal)
set datafile separator ","

set xtics    nomirror
set ytics    nomirror
set offsets  1, 1, 5, 0

set key autotitle columnheader
set key vertical top left box opaque width 1.5 samplen 0
set border back

set xlabel "Herbie Time"
set ylabel "Error Change (res / src)"

set logscale  y
set autoscale y

set output "$data.herbie_time_improve.png"
set title "Herbie Time vs. Daisy Error Improvement ($data)"
plot "$data" using \
  $HERBIE_TM:(\$$DAISY_RES_ERR/\$$DAISY_SRC_ERR) \
  notitle linecolor rgb "#000099" pointtype 7
EOF
}

main "$@"
