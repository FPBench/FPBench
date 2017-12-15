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
  daisy_herbie_bar "$data"
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

function daisy_herbie_bar {
  local data="$1"
  local derr_src="8"
  local derr_res="9"

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

set output "$data.png"
set title "Daisy error bound after Herbie / before Herbie ($data)"
plot "$data" using \
  (\$$derr_res/\$$derr_src):xtic(2) \
  title "Error Ratio" linecolor rgb "#000099"
EOF
}

main "$@"
