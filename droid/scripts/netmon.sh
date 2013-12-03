#!/bin/ash

polling_netstat() {
  local pollTime="$1"
  shift
  local nsArgs="$1"
  shift
  
  local now
  while [ -z "" ]; do
    now=`date '+%Y-%m-%d %H:%M:%S'`
    netstat $nsArgs 2>&1 | \
      sed -e '/^\(Active\|Proto\)/d' \
        -e '/\(tc\|ud\)p6/d' \
        -e "s/^/{$now} /"
    sleep $pollTime
  done
}


netstatmon() {
  local pollTime="$1"
  shift
  local logFile="$1"
  shift
  
  polling_netstat "$pollTime" -npaut | tee -a "$logFile"
}
