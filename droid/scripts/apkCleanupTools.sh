apkls_noVersion() {
  local targdir="$1"
  shift
  local outfile="${1:-$PWD/apkls-out.log}"
  shift

  if [ -z "$targdir" ]; then
    echo "usage: apkls_noVersion <targpath> [outfile]"
    return 1
  fi

  local owd="$PWD"
  case "$outfile" in
    ./*|../*|/*)
      :
      ;;
    *)
      outfile="$PWD/$outfile"
  esac

  cd $targdir || return 1

  ls -1 --color=never |\
     sed -e 's/*\\([^\\]+)$/\1/' -e 's/[-_][0-9].*//' |\
     uniq > $outfile
     
  cd $owd
}


cleanup_apks() {
  local bakdir="$1"
  shift
  
  local iidir="$bakdir/is.installed"
  [ -d $iidir ] || mkdir $iidir || return 2
  
  apkls_noVersion apks in_apks.log || return 3
  apkls_noVersion $bakdir in_stashdir.log || return 4
  
  comm -12 in_apks.log in_stashdir.log |\
    sed 's/ /?/g' >is_installed.log
  local bf f
  for bf in $(cat is_installed.log); do
    f="$(echo $bakdir/${bf}*.apk)"
    #echo "%${f}%"
    if [ -r "$f" ]; then
      echo "Stashing $f"
      mv "$f" $iidir/ || return 5
    fi
  done
}