#!/bin/ash


BARE_LS="/system/xbin/ls -1c"
if [[ ! -x /system/xbin/ls ]]; then
    BARE_LS="/bin/ls -1c"
fi


tibak__tool__verify_exists() {
  if [[ -e $1 ]]; then
      echo $1
  else
      echo "@@@ File does not exist:  \"$1\"" 1>&2
      return 1
  fi
}


tibak__tool__find_basenames() {
  local path="$1"
  shift
  local suffix="$1"
  shift

  local d_re='[0-9]'
  local date_re="20${d_re}${d_re}${d_re}${d_re}${d_re}${d_re}"
  local time_re="${d_re}${d_re}${d_re}${d_re}${d_re}${d_re}"

  find $path -name '*.'"$suffix" | \
    sed -e 's/'"-${date_re}-${time_re}.$suffix"'$//' | \
    sort -u
}


tibak__tool__find_most_recent() {
  local bak_basename="$1"
  shift

  local \
    latest_prop=`$BARE_LS "${bak_basename}"*.properties | head -n 1`
  tibak__tool__verify_exists "$latest_prop" \
    || return 1

  case "$bak_basename" in
    *com.keramidas.virtual*)
      return 0
      ;;
  esac

  local latest_tgz="${latest_prop%.properties}.tar.gz"
  tibak__tool__verify_exists "$latest_tgz" \
    || return 1

  local apkMD5=`grep app_apk_md5 $latest_prop | sed 's/^[^=][^=]*=//'`
  local latest_apk="${bak_basename}-${apkMD5}.apk.gz"
  tibak__tool__verify_exists "$latest_apk" \
    || return 1

  local odexMD5=`grep app_odex_md5 $latest_prop | sed 's/^[^=][^=]*=//'`
  if [ -z "$odexMD5" ]; then
    return 0
  fi

  local latest_odex="${bak_basename}-${odexMD5}.odex.gz"
  if tibak__tool__verify_exists "$latest_odex"; then
    :
  else
    echo "@@@    Ignoring missing *.odex.gz file." 1>&2
  fi
}


tibak_make_manifest() {
  local mfstFile="$1"
  shift
  local baks="$1"
  shift

  if [ -z "$mfstFile" ]; then
    echo "usage:  tibak_make_manifest <manifestFilename> [<bakDir>]"
    echo "<bakDir>:  Either a path with the subdirectory"
    echo "           \"TitaniumBackup\" or the backup "
    echo "           directory itself."
    return 1
  fi

  local oPWD="$PWD"
  if [[ ! -d TitaniumBackup ]]; then
    if [ -n "$baks" ] && [[ -d $baks ]]; then
      cd "$baks"
    else
      echo "Unknown path:  \"$baks\""
      baks="/mnt/sdcard/Backups/TitaniumBackup"
      echo "Trying fallback:  \"$baks\""
      if [[ -d $baks ]]; then
        cd /mnt/sdcard/Backups
      else
        echo "Cannot find Titanium Backup path."
        return 2
      fi
    fi
  fi

  local searchIn='.'
  if [[ -d TitaniumBackup ]]; then
    searchIn='TitaniumBackup'
  fi


  rm ${mfstFile}
  tibak__tool__find_basenames $searchIn "properties" \
    >${mfstFile}.basenames

  local f hasErrs
  for f in `cat ${mfstFile}.basenames`; do
    tibak__tool__find_most_recent "$f" >>${mfstFile}
    if [ $? -ne 0 ]; then
      echo "@@@    Backup of \"$f\" will be incomplete."
      echo "$f" >>${mfstFile}.incomplete
      hasErrs='y'
    fi
  done

  cd "$oPWD"

  if [ -n "$hasErrs" ]; then
    echo ""
    echo ""
    echo "Failed to construct a complete manifest."
    echo ""
    echo "Check the file \"${mfstFile}.incomplete\""
    echo "for the base-files that wouldn't be"
    echo "stored correctly."
    return 1
  else
    rm ${mfstFile}.basenames
  fi

  return 0
}
add_help tibak_make_manifest \
  "<manifestFilename> [<bakDir>]" \
  "        (Run w/o args for full help.)"


tar_tibak() {
  local today=`date +%Y%m%d`
  local targDir="."
  local theManifest="tibak-${today}.manifest"
  local tiBakDir=""
  local createManifest="y"
  while [ -n "$1" ]; do
    case "$1" in
      -s)
        shift
        tiBakDir="$1"
        ;;
      -[dt])
        shift
        targDir="$1"
        ;;
      -m)
        shift
        theManifest="$1"
        ;;
      -F)
        shift
        theManifest="$1"
        createManifest=""
        ;;
      -h|--help)
        echo "usage: tar_tibak [Options]"
        echo "   -s <srcDir>"
        echo "       Path to the Titanium Backups.  If"
        echo "       the path ends in \"TitaniumBackup\""
        echo "       you can pass the parent-dir."
        echo "   -d <targDir>"
        echo "   -t <targDir>"
        echo "       Where to put the backup-tarball & the manifest"
        echo "       [when not using the \"-F\" option]."
        echo "   -m <bakManifest>"
        echo "       Custom name of the manifest file to create.  Will"
        echo "       contain the list of everything in the tarball."
        echo "   -F <bakManifest>"
        echo "       Like \"-m\", but specified a pre-existing"
        echo "       manifest to use."
        return 0
        ;;
      *)
        echo "Unknown option:  \$1\""
        echo ""
        tar_tibak --help
        return 1
        ;;
    esac
    shift
  done

  if [ -n "$tiBakDir" ] && [[ ! -d $tiBakDir ]]; then
    echo "Directory does not exist:  \"$tiBakDir\""
    echo ""
    tar_tibak --help
    return 1
  fi

  if [[ ! -d $targDir ]]; then
    echo "Directory does not exist:  \"$tiBakDir\""
    echo ""
    tar_tibak --help
    return 1
  fi

  if [ -n "$createManifest" ]; then
    case "$theManifest" in
      */*)
        :
        ;;
      *)
        theManifest="$targDir/${theManifest}"
        ;;
    esac
    tibak_make_manifest "${theManifest}" "$tiBakDir" \
      || return 1
  fi

  local targ="${targDir}/TiBackup${today}.tar.bz2"
  tar -cjvf "$targ" -T ${theManifest}

  if [ -z "$createManifest" ]; then
    rm $theManifest
  fi
}
add_help tar_tibak \
  "[-h|--help] \\" \
  "[-s <srcDir>] [-d <targDir>] \\" \
  "[-m <bakManifest>] [-F <bakManifest>]"
