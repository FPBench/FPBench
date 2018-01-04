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

readonly     BENCH_NAME="2"
readonly      HERBIE_TM="3"
readonly HERBIE_SRC_ERR="4"
readonly HERBIE_RES_ERR="5"
readonly   DAISY_SRC_TM="6"
readonly   DAISY_RES_TM="7"
readonly  DAISY_SRC_ERR="8"
readonly  DAISY_RES_ERR="9"

function clean_csv {
  local data="$1"

  awk -F',' "
    NR == 1 || NF == 9 && \$$DAISY_RES_ERR ~ /\./ {
      print \$0;
      next
    }

    {
      print \"Warning: skipping \" \$$BENCH_NAME \" in '$data' (bad row)\" \
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

  gnuplot <<EOF
$(terminal 2000 1000)
set datafile separator ","

set xtics    rotate by 45 right
set xtics    nomirror
set ytics    nomirror

set style fill       solid
set style data       histograms
set style histogram  clustered

set boxwidth 1

set xlabel "Benchmark"
set ylabel "Error Change (res / src)"

set output "$data.daisy_herbie_bar.png"
set multiplot

bm    = 0.12
lm    = 0.08
rm    = 0.95
gap   = 0.03
size  = 0.80
relsz = 0.66
y1    = 0.0
y2    = 1.5
y3    = 30
y4    = 50

set key autotitle columnheader
set lmargin at screen lm
set rmargin at screen rm
set logscale y

unset key
set border 1+2+8
set bmargin at screen bm
set tmargin at screen bm + size * relsz

set yrange [y1:y2]
plot "$tmp" using (\$2):xtic(1) \
  notitle linecolor rgb "#000099"

set title "Daisy error bound after Herbie / before Herbie ($data)"
set key invert horizontal top left

unset xtics
set   ytics   autofreq
unset xlabel
unset ylabel

set border 2+4+8
set bmargin at screen bm + size * relsz + gap
set tmargin at screen bm + size + gap


set yrange [y3:y4]
plot "$tmp" using (\$2):xtic(1) \
  title "Error Ratio" linecolor rgb "#000099"
EOF
  rm "$tmp"
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
