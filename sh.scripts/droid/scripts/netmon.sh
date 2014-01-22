#!/bin/ash


polling_netstat() {
  local pollTime="$1"
  shift
  local nsArgs="$1"
  shift

  # Check args; null-out the options if there's an error.
  case "$pollTime" in
      -h|--help)
          pollTime=''
          nsArgs=''
          ;;
      [0-9]*)
          :
          ;;
      *)
          pollTime=''
          nsArgs=''
          ;;
  esac

  if [ -z "$pollTime$nsArgs" ]; then
      echo "usage:  polling_netstat <pauseTime> [<netstatArgs⃨>]"
      return 1
  fi

  # Default vals:
  if [ -z "$nsArgs" ]; then
      nsArgs="-naut"
  fi

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
add_help polling_netstat \
  "<pauseTime> [<netstatArgs⃨>]"


netstatmon() {
  local pollTime="$1"
  shift
  local logFile="$1"
  shift

  # Check args; null-out the options if there's an error.
  case "$pollTime" in
      -h|--help)
          pollTime=''
          ;;
      [0-9]*)
          :
          ;;
      *)
          pollTime=''
          ;;
  esac

  if [ -z "$pollTime" -o -z "$logFile" ]; then
      echo "usage:  netstatmon <pauseTime> <logFile>"
      echo ""
      echo "If run as root, adds the '-p' option "
      echo "to the 'netstat' call."
      return 1
  fi

  local netstatOpts="-naut"
  if [ $UID -eq 0 ]; then
      netstatOpts="${netstatOpts}p"
  fi

  polling_netstat "$pollTime" $netstatOpts | tee -a "$logFile"
}
add_help netstatmon \
  "<pauseTime> <logFile>"