#!/bin/bash

lpq_jobs() {
  lpq -l | grep '\[job'
}

if [[ `lpq_jobs` ]] ; then
  echo "<icon=printer.xbm/><fc=BlueViolet>$(lpq_jobs | awk '{print $1 $4}' | tr "\n", " ")</fc>"
fi
