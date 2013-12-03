MV=/system/xbin/mv
#MV="echo mv"


mv_v() {
  local verbose="y"
  if [ "$1" = "-v" -o "$1" = "--verbose" ]; then
    shift
  elif [ "$1" = "-q" -o "$1" = "--quiet" ]; then
    verbose=''
    shift
  fi
  
  if [ -n "$verbose" ]; then
    local nFiles=$#

    local oldFile nuFile f
    for f in "$@"; do
      if [ "${f:0:1}" = "-" ]; then
        let --nFiles
      elif [ -z "$oldFile" ]; then
        oldFile="$f"
      elif [ -z "$nuFile" ]; then
        nuFile="$f"
      fi
    done
    
    if [ $nFiles -gt 2 ]; then
      echo "mv $@"
    else
      echo "\"$oldFile\" → \"$nuFile\""
    fi
  fi

  $MV "$@"
}
add_help mv_v  "[-q|-v|--quiet|--verbose] <'mv'-args…>"


jpw__split_datetime() {
  local v
  case "$1" in
    [0-9][0-9][0-9][0-9][-/,:.][0-9][0-9][-/,:.][0-9][0-9][-/,:.]*[0-9])
      v="$1"
      ;;
    [0-9][0-9][0-9][0-9][-/,:.][0-9][0-9][-/,:.][0-9][0-9])
      v="$1"
      ;;
    [0-9][0-9][-/,:.][0-9][0-9])
      v="$1"
      ;;
  esac
  
  local IFS="-/,:.$IFS"
  set -- $v
  echo "$@"
}


ren__tool__zeroPad() {
  local max="$1"
  shift
  local val="$1"
  shift
  local prefix="$1"
  shift
  local suffix="$1"
  shift
  
  let max/=10
  local zpad=''
  while [ $max -gt 1 -a $val -lt $max ]; do
    zpad="${zpad}0"
    let max/=10
  done

  echo "$prefix$zpad$suffix"
}


ren__tool__byPattern() {
  local verbose="$1"
  shift
  local oldPat="$1"
  shift
  local nuPat="$1"
  shift
  local f="$1"
  shift

  [ "$verbose" = "--verbose" ] && verbose="-v"

  local nfn="${f/$oldPat/$nuPat}"
  if [ "$f" = "$nfn" ]; then
    if [ "$verbose" = "-v" ]; then
      echo "Skipping:  New filename is also \"$nfn\""
    fi
  else
    mv_v ${verbose:--q} -i "$f" "$nfn"
  fi
}


ren__tool__renumber() {
  verbose="$1"
  shift
  zeroPad="$1"
  shift
  renumberFrom="$1"
  shift
  oldPat="$1"
  shift
  
  local marker="-±_«®éñüm»_±-"
  local nuPatPre="$marker${oldPat%%\?*}"
  local nuPatSuf="${oldPat##*\?}"

  local zpad
  local count=$renumberFrom
  for f in "$@"; do
    if [ -n "$zeroPad" ]; then
      zpad="$(ren__tool__zeroPad 1000 $count)"
    fi
    
    ren__tool__byPattern "$verbose" "$oldPat" \
        "$nuPatPre$zpad$count$nuPadSuf" "$f"
    
    let ++count
  done
  
  for f in *${marker}*; do
    [[ -e $f ]] || continue
    ren__tool__byPattern "$verbose" "$marker" "" "$f"
  done  
}


ren__pre_suf() {
  local verbose="-q"
  if [ "$1" = "-v" -o "$1" = "--verbose" ]; then
    verbose=""
    shift
  fi
  #
  local prefix="$1"
  shift
  local suffix="$1"
  shift
  local file="$1"
  shift

  local d="${file%/*}"
  if [ "$file" = "$d" ]; then
    d=""
  else
    [ -n "$d" ]  && d="${d}/"
  fi

  local bf="${file##*/}"

  local fsuf="${bf##*.}"
  if [ "$file" = "$fsuf" ]; then
    fsuf=""
  else
    [ -n "$fsuf" ]  && fsuf=".${fsuf}"
  fi

  local bfns="${bf%.*}"

  local nu="$d$prefix${prefix:+-}$bfns"
  nu="$nu${suffix:+-}$suffix$fsuf"

  mv_v $verbose -i "$file" "$nu"
}


rename() {
  local count prefix suffix oldPat nuPat renumberFrom
  local zeroPad verbose
  while [ -n "$1" ]; do
    case "$1" in
      -v)
        verbose="-v"
        ;;
      -[ni])
        count=0
        ;;
      -r)
        shift
        renumberFrom="$1"
        shift
        oldPat="$1"
        ;;
      -z)
        zeroPad=y
        ;;
      -p)
        shift
        prefix="$1"
        ;;
      -s)
        shift
        suffix="$1"
        ;;
      -h|--help)
        echo "usage:"
        echo " rename [-n [-z]] [-p <prefix>] [-s <suffix>] \\"
        echo "    <file> [<file> …]"
        echo " rename [-z] -r <startNum> <numPatStr> \\"
        echo "    <file> [<file> …]"
        echo " rename <oldPatStr> <newStr> <file> [<file> …]"
        echo ""
        echo ""
        echo "“<oldPatStr>” can have '?' and '*' wildcard.  The"
        echo "matching text will be REMOVED by the rename."
        echo "“<numPatStr>” MUST have '?' ONLY, matching"
        echo "all of the digits to renumber."
        echo "“<startNum>” can be negative, which will"
        echo "reverse the numbering.  [But you'll have to"
        echo "remove the extra '-' in another pass.]"
        return 0
        ;;
      *)
        break
        ;;
    esac
    
    shift
  done


  if [ -z "$count$prefix$suffix$renumberFrom" ]; then

    local oldPat="$1"
    shift
    local nuPat="$1"
    shift

    if [ -z "$oldPat$nuPat" ]; then
      echo "No oldPat/new strings specified!  Nothing to do."
      echo ""
      rename --help
      return 1
    fi

  elif [ -n "$renumberFrom" -a -z "$oldPat" ]; then
  
    echo "'-r' option requires both a start number and a"
    echo "glob pattern (for mathcing the existing numbers)."
    rename --help
    return 1
  
  fi
  
  if [ -z "$*" ]; then
    echo "No files specified!  Nothing to rename."
    echo ""
    rename --help
    return 1
  fi

  local f
  if [ -n "$renumberFrom" ]; then
    ren__tool__renumber "$verbose" "$zeroPad" \
        "$renumberFrom" "$oldPat" "$@"
  elif [ -z "$oldPat$nuPat" ]; then

    local zpad=''
    for f in "$@"; do
      if [ -n "$count" ]; then
        let ++count

        if [ -n "$zeroPad" ]; then
          zpad="$(ren__tool__zeroPad 1000 $count)"
        fi
      fi
      
      ren__pre_suf $verbose \
          "$prefix" "$suffix$zpad$count" "$f"
    done

  else

    for f in "$@"; do
      ren__tool__byPattern "$verbose" "$oldPat" "$nuPat" \
          "$f"
    done

  fi
}
add_help rename \
  "[-v|--verbose] <ModeArgs…> <files>…" \
  "    The different <ModeArgs> are:" \
  "     ① “ <oldPatStr> <newStr> ”" \
  "     ② “ [-n [-z]] [-p <prefix>] [-s <suffix>] ”" \
  "     ③ “ [-z] -r <startNum> <numPatStr> ”"


ren__IMAG() {
  local nextImgNum="$1"
  shift
  local lastImgNum="$1"
  shift
  
  # grab the suffix arg, if any
  local renameArgs sufArg
  while [ -n "$1" ]; do
    renameArgs="$renameArgs $1"
    if [ "$1" = "-s" ]; then
      shift
      sufArg="$1"
      renameArgs="$renameArgs $1"
    fi
    shift
  done
  
  local argErr
  if [ -z "$nextImgNum" ]; then
    argErr=y
    echo "Initial 'IMAG*' number is missing!"
  fi
  if [ -z "$lastImgNum" ]; then
    argErr=y
    echo "Last 'IMAG*' number is missing!"
  fi
  
  if [ -z "$argErr" -a $nextImgNum -gt $lastImgNum ]; then
    argErr=y
    echo "Initial 'IMAG*' number must be less than the"
    echo "last 'IMAG*' number."
  fi
  
  if [ -n "$argErr" ]; then
    [ -n "$ctrl__suppressUsage" ] && return 1
    # else
    
    echo ""
    echo "usage:  ren__IMAG <startNum> <endNum> \\"
    echo "    […opts…]"
    echo ""
    echo "All remaining args are passed to 'rename'.  Note"
    echo "that this function always adds '-n', and may add"
    echo "'-z' as needed."
    return 1
  fi
  
  local bfn imgList cleanupPatList
  local nIMAG=0
  local sufPat="${sufArg}[0-9]*.jpg"
  while [ $nextImgNum -le $lastImgNum ]; do
    bfn="$(ren__tool__zeroPad 10000 $nextImgNum IMAG)"
    bfn="${bfn}${nextImgNum}"
    
    if [[ -e ${bfn}.jpg ]]; then
      imgList="$imgList ${bfn}.jpg"
      cleanupPatList="$cleanupPatList *${bfn}-${sufPat}"
      let ++nIMAG
    fi

    let ++nextImgNum
  done
  
  [ $nIMAG -gt 1 ] && renameArgs="-n $renameArgs"
  [ $nIMAG -gt 9 ] && renameArgs="-z $renameArgs"

  set -- $imgList
  if [ $# -eq 0 ]; then
    echo "No 'IMAG*' files matched the specified range."
    echo "Nothing to do."
    return 0
  fi
  
  rename $renameArgs "$@"
  
  local f
  imgList=""
  for f in $cleanupPatList; do
    [[ -e $f ]] && imgList="$imgList $f"
  done

  if [ -z "$imgList" ]; then
    echo "Internal Error:  Failed to find the renamed files"
    echo "during the cleanup pass!  Aborting…"
    return 1
  fi

  local extraZero=''
  [ $nIMAG -gt 9 -a $nIMAG -lt 100 ] && extraZero='y'

  if [ $nIMAG -lt 2 ]; then
    rename "-IMAG????" "" $imgList
  elif [ -n "$extraZero" -a -z "$sufArg" ]; then
    rename "-IMAG????-0" "-" $imgList
  elif [ -n "$extraZero" -a "$sufArg" = "p" ]; then
    rename "-IMAG????-p0" "-p" $imgList
  else
    rename "-IMAG????-" "-" $imgList
  fi
}


name_imgs() {
  local verbose=""
  if [ "$1" = "-v" -o "$1" = "--verbose" ]; then
    verbose="-v"
    shift
  fi
  #
  local nuName="$1"
  shift
  local nextImgNum="$1"
  shift
  local lastImgNum="$1"
  shift
  
  local c__usgMesg="\nusage:  name_imgs <fname>"
  c__usgMesg="$c__usgMesg <startNum> <endNum>"
  
  if [ -z "$nuName" ]; then
    echo "New base-filename is missing."
    echo -e "$c__usgMesg"
    return 1
  fi

  ctrl__suppressUsage=y
  ren__IMAG "$nextImgNum" "$lastImgNum" \
    -p "$nuName" $verbose || hasErr=y
  if [ -n "$hasErr" ]; then
    echo -e "$c__usgMesg"
  fi
  unset hasErr ctrl__suppressUsage
}
add_help name_imgs \
  "[-v|--verbose] <fname> <startNum> \\" \
  "    <endNum>" \
  "        (Must be run in a directory with 'IMAG*'" \
  "         files in it.)"


panorama() {
  local verbose=""
  if [ "$1" = "-v" -o "$1" = "--verbose" ]; then
    verbose="-v"
    shift
  fi
  #
  local nuName="$1"
  shift
  local nextImgNum="$1"
  shift
  local lastImgNum="$1"
  shift
  
  local c__usgMesg="\nusage:  panorama <fname>"
  c__usgMesg="$c__usgMesg <startNum> <endNum>"
  
  if [ -z "$nuName" ]; then
    echo "New base-filename is missing."
    echo -e "$c__usgMesg"
    return 1
  fi

  ctrl__suppressUsage=y
  ren__IMAG "$nextImgNum" "$lastImgNum" \
    -s p -p "$nuName" $verbose || hasErr=y
  if [ -n "$hasErr" ]; then
    echo -e "$c__usgMesg"
  fi
  unset hasErr ctrl__suppressUsage
}
add_help panorama \
  "[-v|--verbose] <fname> <startNum> \\" \
  "    <endNum>" \
  "        (Must be run in a directory with 'IMAG*'" \
  "         files in it.)"


forceImgOrder() {
  # Handle Alternate Separators
  local one="$(jpw__split_datetime $1)"
  [ -n "$one" ] && shift
  local two="$(jpw__split_datetime $1)"
  [ -n "$two" ] && shift
  if [ -n "$one$two" ]; then
    set -- $one $two "$@"
  fi

  #
  # Now we can get on with the actual args processing:
  #
  local yr="$1"
  shift
  local mo="$1"
  shift
  local day="$1"
  shift
  local hr="$1"
  shift
  local min="$1"
  shift
  local secs=""
  local secStep=5
  if [ "$1" = "-s" ]; then
    secs=0
    shift
    case "$1" in
      [1-9]|[0-5][0-9])
        secStep="$1"
        shift
        ;;
    esac
  fi
  
  if [ -z "$yr" -o -z "$mo" -o -z "$day" -o \
       -z "$hr" -o -z "$min" -o -z "$*" ]; then
    echo "usage:"
    echo -n "    forceImgOrder <yyyy> <mm> <dd> "
    echo "<HH> <MM>"
    echo "        [-s [<secStep>]] <files>…"
    echo ""
    echo "'-s'  ―  Increment time by 5 seconds, instead of"
    echo "          by minutes."
    echo "'<secStep>'  ―  must be between 1 and 59,"
    echo "          inclusive."
    echo "The date/time parts can be separated by '-' ',' '/'"
    echo "'.' or ':' but must have leading '0's if they are."
    echo ""
    echo -n "The Gallery app arranges photos by file "
    echo "datestamp,"
    echo "instead of alphabetically.  This fn. will re'touch'"
    echo "the files passed to it with new datestamps that"
    echo "increment “in alphabetical order.”  The date/time"
    echo "args specify the starting datestamp."
    return 1
  fi
  
  local dtpart="$yr-$mo-$day"
  
  local zhr zmin
  local f tt
  for f in "$@"; do
    [ $min -lt 10 ] && zmin="0" || zmin=""
    [ $hr -lt 10 ] && zhr="0" || zhr=""
    tt="$dtpart $zhr$hr:$zmin$min"
    
    if [ -n "$secs" ]; then
      tt="${tt}:"
      [ $secs -lt 10 ] && tt="${tt}0"
      tt="${tt}$secs"
    fi
    
    touch -c -d "$tt" "$f"
    
    if [ -n "$secs" ]; then
      let secs+=$secStep
      if [ $secs -gt 59 ]; then
        let secs%=60
        let ++min
      fi
    else
      let ++min
    fi
    
    if [ $min -gt 59 ]; then
      min=0
      let ++hr
    fi
  done
}
add_help forceImgOrder \
  "<yyyy> <mm> <dd> <HH> <MM> [-s] \\" \
  "    <files>…" \
  "        (Run w/o args for full help.)"
