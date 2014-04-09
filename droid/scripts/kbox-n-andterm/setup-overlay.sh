# -*- mode: sh; fill-column: 56; sh-basic-offset: 2 -*-
#
# This file should be sourced.


####################
# Config Envvars
####################


keep_ANDTERM_DATA_DIR=${ANDTERM_DATA_DIR:+y}
ANDTERM_DATA_DIR=${ANDTERM_DATA_DIR:-/data/data/jackpal.androidterm}

keep_OVERLAY_HOME=${OVERLAY_HOME:+y}
OVERLAY_HOME=${OVERLAY_HOME:-/sdcard/home}

# Default value of this envvar is '':
#UNIT_TEST=''


####################
# Other Envvars
####################

OVERLAY_TARBALL_DEFAULT="$PWD/overlay-bundle.tar.bz2"

KBOX_STARTER_SCRIPTS="start_bash.sh start_shell.sh"
KBOX_BASE_DIRS="kbox kbox2"

CFG_TEMPLATE='###FOR::MOD::DURING::INSTALL###'

MSG_RUN_ROOTLESS='    . setup-overlay.sh AndroidTerminal'
MSG_JACKPAL_APP_NAME="\"Android Terminal Emulator\""
MSG_COWARD='Cowardly refusing to continue.'

# Placeholders; set before the call to 'sov__main_fn'
TAR_BIN='echo !!!ERROR!!!'
CAT_BIN='echo !!!ERROR!!!'
SED_BIN='echo !!!ERROR!!!'
GREP_BIN='echo !!!ERROR!!!'
BB_BIN='echo !!!ERROR!!!'

export OVERLAY_TARBALL_DEFAULT OVERLAY_HOME \
  TAR_BIN CAT_BIN SED_BIN GREP_BIN BB_BIN


########################################
# Functions:  General-Purpose
########################################


ln_sf() {
  local origFile="$1"
  [ $# -gt 0 ] && shift
  local targlink="$1"
  [ $# -gt 0 ] && shift
  # N.B.:  Some shells hork up an error if you try to
  #        'shift' an empty arglist.  Hence the odd
  #        song-n-dance above.

  if [ -z "$origFile" -o -z "$targlink" ]; then
    echo "usage:  ln_sf <srcFile> <destLink>"
    return 1
  fi

  if [ ! -e $origFile ]; then
    echo "Error:  No such file:  \"$origFile\""
    return 1
  fi

  if [ -d $targlink ]; then
    targlink="${targlink%%/}/${origFile##*/}"
  fi

  if [ -L $targlink ]; then
    echo "Replacing Symlink:  $targlink"
    $BB_BIN rm $targlink || return $?
  elif [ -e $targlink ]; then
    echo "Backing up symlink target:  $targlink"
    $BB_BIN mv $targlink ${targlink}.~bak~ || return $?
  fi

  # At this point, there should be nothing left in the
  # way.
  echo "ln -s:  $targlink -> $origFile"
  $BB_BIN ln -s "$targlink" "$origFile" || return $?
}


chmod_v() {
  local numod="$1"
  [ $# -gt 0 ] && shift
  local targFile="$1"
  [ $# -gt 0 ] && shift
  # N.B.:  See 'ln_sf' for an explanation of the odd
  #        song-n-dance above.

  if [ ! -e $targFile ]; then
    echo "Cannot set mode; cannot find file:"
    echo "    '$targFile'"
    return 2
  fi

  echo "Setting mode:  '$targFile'"
  $BB_BIN chmod $numod $targFile
}


cp_v() {
  local origFile="$1"
  [ $# -gt 0 ] && shift
  local targDir="$1"
  [ $# -gt 0 ] && shift
  local numod="$1"
  [ $# -gt 0 ] && shift
  # N.B.:  See 'ln_sf' for an explanation of the odd
  #        song-n-dance above.

  echo "cp:  '$origFile' -> '$targDir'"
  $BB_BIN cp $origFile $targDir || return $?

  # Stop now unless we're setting the new mode.
  [ -z "$numod" ] && return 0

  local targFile="$targDir"
  if [ -d $targDir ]; then
    targFile="${targFile%/}/${origFile##*/}"
  fi

  chmod_v $numod "$targFile"
}


get_symlink_targ() {
  if [ "$1" = "-a" ]; then
    to_abspath=y
    shift
  fi
  local symlink="$1"

  # Punt & return "failure" if not a symlink.
  if [ ! -L $symlink ]; then
    echo "$symlink"
    return 2
  fi

  set -- `ls -l $symlink 2>/dev/null`

  while [ -n "$1" -a "$1" != "->" ]; do
    shift
  done

  local targ="$2"

  # Convert to an absolute path, if desired.
  if [ -n "$to_abspath" ]; then
    # Find the true path of the symlink, in case the
    # target is relative to it.
    local sld=`sov__resolve_path ${symlink%/*}`
    local owd="$PWD"
    if [ -n "$sld" ]; then
      cd $sld
    fi
    # Find the true path of the target, using the
    # symlink's directory as the base.
    targ=`sov__resolve_path $targ`
    cd $owd
  fi

  echo "$targ"
}


########################################
# Script-Specific Functions:
#     Tools
########################################


sov__maybe_mkdir() {
  local nuDir="$1"
  shift
  local numod="$1"
  shift

  if [ -d $nuDir ]; then
    return 0
  fi

  echo "Creating:  \"$nuDir\""
  $BB_BIN mkdir "$nuDir" || return $?

  # Stop now unless we're setting the new mode.
  [ -z "$numod" ] && return 0

  chmod_v $numod "$nuDir"
}


sov__find_binary() {
  local progNm="$1"
  shift
  local defaultDir="$1"

  local d
  for d in xbin bin; do
    if [ -e /system/$d/$progNm ]; then
      echo /system/$d/$progNm
      return 0
    fi
  done

  # else:  Find the first "$progNm" binary "at the front
  # of" the "$PATH".

  local via_command=`command -v "$progNm"`
  # 'command -v' often prints the entire definition of
  # an alias.  So, we'll need to handle that case
  # separately.
  case "$via_command" in
    alias\ *=*)
      via_command=$progNm
      ;;
  esac

  if [ -n "$via_command" ]; then
    echo ${via_command}
    return 0
  fi

  # else:  Punt.  Return the name passed, prepending
  # the default location, if provided.

  echo ${defaultDir}${defaultDir:+/}${progNm}
  return 1
}


sov__resolve_path() {
  local f="$1"

  case "$f" in
    ../*|./*|*/./*|*/../*|*/.|*/..)
      local owd="$PWD"
      local f_nm="${f##*/}"
      local fd="${f%/*}"
      [ -n "$fd" ] && cd $fd
      f="$PWD/$f_nm"
      cd $owd
      ;;
  esac

  echo "$f"
}


sov__has_non_jackpal_targ() {
  local t
  for t in "$@"; do
    case "$t" in
      *jackpal*)
        :
        ;;
      *)
        echo "Target requires root access: \"$t\""
        return 1
        ;;
    esac
  done

  # else:
  return 0
}


sov__has_data_local() {
  local t
  for t in "$@"; do
    case "$t" in /data/local/*) return 0;; esac
  done

  # else:
  return 1
}


sov__chkGetArg() {
  local optName="$1"
  shift
  local val="$1"

  local no_arg_err="No argument provided to option"
  no_arg_err="$no_arg_err \"--$optName\"."

  # Check that the 'value' isn't another option.  If so,
  # capture the error in the 'if'-statement below.
  case "$val" in -*) val='' ;; esac

  if [ -z "$val" ]; then
    sov__usage $no_arg_err
    return 4
  fi
  # else:
  # We're ok.

  echo "$val"
}


sov__getmode() {
  local fileDir="$1"
  shift
  local fileName="$1"
  shift
  local defaultMode="$1"
  shift

  if [ -z "$fileDir" ]; then
    fileDir="${fileName%/*}"
    fileName="${fileName##*/}"
  fi

  local modfile="$fileDir/.perms..$fileName"
  if [ -e $modfile ]; then
    $CAT_BIN $modfile && return 0
  fi
  # else:
  #
  # If either the 'cat' failed or there's no '$modfile',
  # use the default
  echo $defaultMode
}


sov__targAlias2Dirs() {
  local targAlias="$1"
  shift
  local noKboxSubdirs="$1"

  local targDirBase

  case "$targAlias" in
    [jJ]ackpal|[Aa]nd*[-_.][Tt]erm*|[Aa]nd*[Tt]erm)
      targDirBase=$ANDTERM_DATA_DIR
      ;;

    sd2|sdext|sd[-_.]ext)
      for targDirBase in /data/sdext2 /sd-ext /system/sd
      do
        if [ -d $targDirBase ]; then
          break
        fi
      done
      ;;

    local|data[-_.]local)
      targDirBase=/data/local
      ;;

    # N.B.:  Catch the path, '/sdcard/ext' here so that
    # it's not caught up in the safety-check later.
    xmount|/sdcard/ext/*)
      targDirBase=/sdcard/ext
      ;;

    --help)
      echo "Aliases for installation/overlay targets:"
      echo ""
      echo "Jackpal"
      echo "AndroidTerminal"
      echo "AndroidTerm"
      echo "AndTerm"
      echo "    $ANDTERM_DATA_DIR"
      echo ""
      echo "sd2"
      echo "sdext"
      echo "sd-ext"
      echo "    One of the usual mount points for"
      echo "    the 2nd partition of your SD card."
      echo "    [There are several; the first one"
      echo "     found is the one used.]"
      echo ""
      echo "local"
      echo "data-local"
      echo "    /data/local"
      echo ""
      echo "xmount"
      echo "    The directory, '/sdcard/ext' ... *if*"
      echo "    you're using it as a cross-mount for"
      echo "    the 2nd partition of your SD card."
      echo ""
      echo "You can lowercase any capital letters in any"
      echo "of the aliases.  You can also put a '-' '_'"
      echo "or '.' between each word [i.e. before the"
      echo "capital letter or in place of the '-' in the"
      echo "aliases shown above]."
      ;;

    *)
      echo $1
      return 0
      ;;
  esac

  local targs
  if [ -d $targDirBase ]; then
    targs="$targs${targs:+ }$targDirBase"
  fi

  local kd
  if [ -z "$noKboxSubdirs" ]; then
    for kd in $KBOX_BASE_DIRS; do
      if [ -d $targDirBase/$kd ]; then
        targs="$targs${targs:+ }$targDirBase/$kd"
      fi
    done
  fi

  if [ -z "$targs" ]; then
    echo "!!! The alias \"$targAlias\" refers to a" 1>&2
    echo "!!! path that doesn't exist!" 1>&2
    echo "!!! Ignoring..." 1>&2
    echo "!!!" 1>&2
    return 1
  fi
  # else:

  echo $targs
}


########################################
# Script-Specific Functions:
#     "setup-overlay" Actions
########################################


sov__tweak_andterm_start() {
  local theScript="$1"
  shift
  local theTarg="$1"
  shift
  local shell="$1"
  shift
  local shellHome="$1"
  shift
  local etcDir="$1"
  shift


  local sed_cmds
  if [ -n "$shell" ]; then
    sed_cmds="-e 's/#SHELL0=${CFG_TEMPLATE}/"
    sed_cmds="${sed_cmds}SHELL0=${shell}/' "
  fi

  if [ -n "$etcDir" ]; then
    sed_cmds="${sed_cmds}-e '"
    sed_cmds="${sed_cmds}s/#ENV_DIR=${CFG_TEMPLATE}/"
    sed_cmds="${sed_cmds}ENV_DIR=${etcDir}/' "
  fi

  if [ -n "$shellHome" ]; then
    sed_cmds="${sed_cmds}-e '"
    sed_cmds="${sed_cmds}s/#SHELL_DIR=${CFG_TEMPLATE}/"
    sed_cmds="${sed_cmds}SHELL_DIR=${shellHome}/'"
  fi

  if [ -d $theTarg ]; then
    theTarg=${theTarg}/${theScript##*/}
  fi

  # FIXME:  Add verbose error messages, including
  # modification start/stop:

  $SED_BIN $sed_cmds $theScript | \
    $GREP_BIN -v "$CFG_TEMPLATE" >$theTarg \
    || return $?

  chmod_v 0644 $theTarg
}


sov__check_extract() {
  local extractStat="$1"
  shift
  local targ="$1"
  shift
  local tarball="$1"

  if [ $extractStat -eq 0 ]; then
    echo "...Done!"
    return 0
  fi
  #else

  echo "Extraction Failed!"
  echo ""
  echo "\"$targ\" does not exist or"
  echo "is not writable.  Or, $TAR_BIN"
  echo "doesn't support the compression format of the"
  echo "tarball, \"$tarball\"."
  return $extractStat
}


sov__backup_targfile() {
  targfileDir="$1"
  shift
  targfileBase="$1"
  shift

  # Make a backup file of the specified target, if no
  # backup was ever made before.

  local theTarg=$targfileBase/$targfileBase
  if [ ! -e $theTarg ]; then
    # No need to back anything up.
    return 0
  fi
  # else

  local marker=$targfileBase/.was.customized-_-
  marker="$marker$targfileBase"
  if [ -e $marker -o -e ${theTarg}.orig ]; then
    # Stop if:
    # - The marker exists.
    # - There's already a backup file present.
    return 0
  fi

  echo "Backing up original '$targfileBase'..."

  mv $theTarg ${theTarg}.orig
  stat=$?
  if [ $stat -ne 0 ]; then
    echo "Backup failed!"
    echo "$MSG_COWARD"
    return $stat
  fi

  touch $marker
  stat=$?
  if [ $stat -ne 0 ]; then
    echo "Failed to write marker to \"$targfileDir\"."
    echo "$MSG_COWARD"
    return $stat
  fi
}


sov__extract_overlay() {
  local overlay_tarball="$1"
  shift
  local targdirBase_etcFiles="$1"
  shift
  local printWarning="$1"

  local compType
  case "$overlay_tarball" in
    *.tgz|*.tar.gz)
      compType="-z"
      ;;
    *.bz2)
      compType="-j"
      ;;
  esac

  local stat
  local etcDir=$targdirBase_etcFiles/etc

  if [ -z "$targdirBase_etcFiles" ]; then
    echo "Unarchiving overlay..."

    $TAR_BIN $compType -xf $overlay_tarball \
      -C $OVERLAY_HOME
    stat=$?
    sov__check_extract $stat $OVERLAY_HOME \
      $overlay_tarball || return $?

  elif [ -d $etcDir ]; then
    # Backup the 'profile' if we haven't touched it before.
    sov__backup_targfile "$etcDir" profile

    echo "Unarchiving overlay to:"
    echo "    '$etcDir'"

    $TAR_BIN $compType -xf $overlay_tarball \
      -C $targdirBase_etcFiles 'etc/*' || return $?
    stat=$?
    sov__check_extract $stat $targdirBase_etcFiles \
      $overlay_tarball || return $?

  elif [ -n "$printWarning" ]; then
    echo "No such directory:  $etcDir"
    echo "No place to extract the overlay!"
    echo "Skipping..."
  fi

  return 0
}


sov__update_files() {
  local srcTargSpec="$1"
  shift
  local targBase="${1%/}"
  shift

  # Split out and verify the source path:

  local srcDir="${srcTargSpec%%:*}"
  case "$srcDir" in
    # Do nothing if this is an abspath.
    /*)
      :
      ;;

    # Relative paths are looked for under '$OVERLAY_HOME'.
    *)
      srcDir="$OVERLAY_HOME/$srcDir"
      ;;
  esac

  if [ ! -e $srcDir ]; then
    echo "Does not exist:  \"$srcDir\""
    echo "Cannot perform update!"
    echo ""
    echo "$MSG_COWARD"
    return 2
  fi

  # Split out and verify the targed directory:

  local targSubdir="${srcTargSpec##*:}"
  if [ -n "$targSubdir" ]; then
    targSubdir="${targSubdir#/}"
    targSubdir="${targSubdir%/}"
  fi
  local targDir="$targBase/$targSubdir/"

  if [ ! -d $targDir ]; then
    echo "Directory does not exist:  \"$targDir\""
    echo "Cannot perform update!"
    echo ""
    echo "$MSG_COWARD"
    return 2
  fi

  # Special case:  Copying a single file:
  if [ ! -d $srcDir ]; then
    cp_v $srcDir $targDir \
      `sov__getmode "" $srcDir 0644`
    return $?
  fi

  # else:
  #
  # Copy all of the *plain* files from the src-dir to
  # the targ.
  local f targmod
  for f in $srcDir/*; do
    # Skip non-files.
    if [ ! -f $f ]; then
      continue
    fi

    targmod=`sov__getmode $srcDir ${f##*/} 0644`
    cp_v $f $targDir $targmod
  done
}


sov__has_kbox_starter() {
  local targdirBase="$1"
  shift

  # Check for a startup script in the expected
  # location(s):
  local kboxdir sf
  for kboxdir in $KBOX_BASE_DIRS; do
    kboxdir="$targdirBase/$kboxdir"
    if [ -d $kboxdir ]; then
      for sf in $KBOX_STARTER_SCRIPTS; do
        if [ -e $kboxdir/bin/$sf ]; then
          return 0
        fi
      done
    fi
  done

  # Not in the expected location?  Then let's do a
  # somewhat more detailed check:
  local notFound
  if [ -d $targdir/bin ]; then
    for sf in $KBOX_STARTER_SCRIPTS; do
      notFound=''

      if [ ! -e $targdir/bin/kbox_shell ]; then
        notFound='y'
      fi
      if [ ! -e $targdir/bin/$sf ]; then
        notFound='y'
      fi

      if [ -z "$notFound" ]; then
        return 0
      fi
    done
  fi

  # Nope.  Not there.
  return 1
}


sov__tweak_kbox_starter() {
  local nuShellStarter="$1"
  shift
  local targdirBase="$1"
  shift

  local f tf dtf
  for f in $KBOX_STARTER_SCRIPTS; do
    tf=$targdirBase/bin/$f
    dtf=$targdirBase/bin/.was.customized-_-$f

    if [ -e $tf ]; then
      if [ ! -e ${tf}.orig -a ! -e $dtf ]; then
        echo "Backup up original '$f'..."
        mv $tf ${tf}.orig || return $?
      fi

      echo "Replacing '$f'..."
      cp_v $nuShellStarter $tf 0755 && touch $dtf
    fi
  done
}


sov__overlay_new_busybox() {
  # Will put a different busybox binary in place of *an*
  # existing one, and will refresh the symlinks
  # accordingly.
  #
  # N.B.:  Setting 'UNIT_TEST=y' does not fully test
  # this function.

  local nu_bb="$1"
  shift
  local nu_bb_targDir=`sov__resolve_path "${1%/}"`
  shift
  local mk_paths="$1"
  shift

  # Find the 'busybox' binary "at the front of" the
  # "$PATH".  We'll invoke several Unix tools directly,
  # through this executable.  However, we only need it
  # to put the new busybox into place.
  #
  # NOTE:  DO NOT use '[ ]' on its own!  Use
  # 'if'-statements and hope that ends up calling a
  # shell-builtin '[ ]'-operator.
  local bb_bin=`type busybox`
  bb_bin=${bb_bin##busybox is }

  #==========
  # Move the new 'busybox' to the same dir as the old one,
  # with a version-suffx appended [if not already
  # there].

  local nu_bb_ver=`$nu_bb | $bb_bin head -n 1 | \
    $bb_bin awk '{ print $2 }'`
  if [ -z "$nu_bb_ver" ]; then
    return 5
  fi

  local nu_bb_name
  echo -n "Copying:  "
  case "$nu_bb" in
    */busybox)
      # Add the version
      nu_bb_name="busybox-$nu_bb_ver"
      cp_v $nu_bb $nu_bb_targDir/$nu_bb_name 0755 \
        || return $?
      ;;
    *)
      # Copy it bare
      nu_bb_name="${nu_bb##*/}"
      cp_v $nu_bb $nu_bb_targDir/ 0755 || return $?
      ;;
  esac
  nu_bb="$nu_bb_targDir/$nu_bb_name"

  #==========
  # Use the new 'busybox', in its new location, to do
  # all further work.
  #
  # We'll also make vars for all of the cmds we'll need,
  # using direct calls to the new 'busybox' in its new
  # location.  Note that functions like 'ln_sf', 'cp_v',
  # 'chmod_v', and 'sov__maybe_mkdir' all use the
  # '$BB_BIN' envvar if set.

  BB_BIN=$nu_bb

  local rm_bin="$nu_bb rm"
  local mv_bin="$nu_bb mv"

  local touch_bin="$nu_bb touch"
  local sed_bin="$nu_bb sed"
  local sort_bin="$nu_bb sort"
  local grep_bin="$nu_bb grep"

  # Setup for unit testing:
  if [ -n "$UNIT_TEST" ]; then
    BB_BIN="echo $BB_BIN"
    rm_bin="echo rm"
    mv_bin="echo mv"
    touch_bin="echo touch"
  fi

  #==========
  # Find the version of the existing 'busybox' *in*
  # *the* *target* *directory*.  If it's not a symlink,
  # "turn it into one".

  local owd=$PWD
  local cur_bb="${nu_bb_targDir}/busybox"
  local old_bb
  if [ -L $cur_bb ]; then
    # Get its true name.

    echo "Dereferencing old 'busybox' symlink."
    old_bb=`get_symlink_targ "$cur_bb"`
  else
    # Rename with a version-suffix and convert to a symlink.

    local old_bb_ver=`$cur_bb | $bb_bin head -n 1 | \
      $bb_bin awk '{ print $2 }'`
    if [ -z "$old_bb_ver" ]; then
      return 5
    fi

    cd $nu_bb_targDir || return $?

    echo "Renaming:  busybox -> busybox-$old_bb_ver"
    $mv_bin busybox busybox-$old_bb_ver || return $?
    old_bb="${cur_bb}-$old_bb_ver"

    ln_sf busybox-$old_bb_ver busybox || return $?

    cd $PWD
  fi

  # For good measure, let's just pry out the base
  # filenames of the var, "$old_bb", just in case it
  # points to a different directory.
  local old_bb_name="${old_bb##*/}"

  #==========
  # Link 'busybox' to the new binary.

  owd=$PWD
  cd $nu_bb_targDir || return $?
  ln_sf $nu_bb_name busybox || return $?

  #==========
  # Fix links to 'busybox' that aren't in the new
  # version.

  local f bf d last_d lnTarg
  local rel_bb=busybox
  local targDirBase="${nu_bb_targDir##*/}"

  # Create any missing paths.
  #
  if [ -n "$mk_paths" ]; then
    local nuDirList=`$nu_bb --list-full | \
      $grep_bin '/' | $sed_bin 's|/[^/]*$||' | \
      $sort_bin -u`

    for d in $nuDirList; do
      if [ "$d" = "bin" ]; then
        :
      elif [ ! -d ../$d ]; then
        sov__maybe_mkdir ../$d
      fi
    done
  fi

  # Create some temp-files that indicate which tools are
  # in the new busybox.  At the same time, add symlinks
  # for new tools.
  #
  for f in `$nu_bb --list-full | $nu_bb sort`; do
    d="${f%/*}"
    case "$d" in
      bin|sbin|usr/bin|usr/sbin)
        :
        ;;
      *)
        # Bare filename or an unsupported dir.
        d=''
        ;;
    esac

    # Switch to any subdirs as needed.
    if [ "$last_d" != "$d" ]; then
      # Start by resetting the CWD and the var holding
      # the relative location of 'busybox'
      cd $nu_bb_targDir || return $?
      rel_bb=busybox

      if [ -n "$d" -a "$d" != "bin" ]; then
        if [ -d ../$d ]; then
          cd ../$d || return $?

          case "$d" in
            sbin)
              rel_bb=../$targDirBase/busybox
              ;;
            usr/bin|usr/sbin)
              rel_bb=../../$targDirBase/busybox
              ;;
          esac
        fi
      fi

      last_d="$d"
    fi

    # Create the link, if missing.
    bf="${f##*/}"
    if [ ! -e $bf ]; then
      ln_sf $rel_bb "$bf" || return $?
    fi
    # Mark that the link is in the new 'busybox'.
    if [ -L $bf ]; then
      $touch_bin "nu_has_$bf" || return $?
    fi
  done
  cd $nu_bb_targDir || return $?

  # Now we perform one more loop, examining every
  # symlink to 'busybox' for ones not in the new
  # version.
  #
  for d in . ../sbin ../usr/bin ../usr/sbin; do
    if [ ! -d $nu_bb_targDir/$d ]; then
      continue
    fi

    # Change directories, if needed, and determine the
    # relative path to the old 'busybox'.
    if [ "$d" != '.' ]; then
      cd $nu_bb_targDir/$d || return $?

      case "$d" in
        ../sbin)
          rel_bb=../$targDirBase/$old_bb_name
          ;;
        ../usr/*)
          rel_bb=../../$targDirBase/$old_bb_name
          ;;
      esac
    fi

    # Iterate over all files.
    #
    for f in *; do
      # Skip irrelevant files
      if [ -L $f ]; then
        lnTarg=`get_symlink_targ "$f"`
        if [ "${lnTarg##*/}" != "busybox" ]; then
          # Not a symlink to 'busybox'.
          continue
        fi
      else
        # Not a symlink.
        continue
      fi

      # Is this tool-symlink supported in the new
      # 'busybox'?
      if [ -e "nu_has_$f" ]; then
        # Yep.  Clean up the marker.

        $rm_bin "nu_has_$f"
      elif [ -e "$nu_bb_targDir/nu_has_$f" ]; then
        # The marker exists, but in the "main dir".  The
        # new 'busybox' probably doesn't put paths on
        # the tool names.

        # Well, it's there, so clean up the marker.
        $rm_bin "$nu_bb_targDir/nu_has_$f"
      else
        # Nope.  Gotta re-link it to the old version.

        ln_sf $rel_bb "$f" || return $?
      fi
    done

    cd $owd || return $?
  done

  # Now clean up any lingering marker files.
  #
  cd $nu_bb_targDir || return $?
  for f in nu_has_* ../sbin/nu_has_* \
    ../usr/bin/nu_has_* ../usr/sbin/nu_has_*
  do
    $rm_bin "$f"
  done

  cd $owd || return $?

  echo ""
  echo "Success!"
  echo "Replaced '$old_bb_name' with the new version."
  echo ""
}


########################################
# Script-Specific Functions:
#     Init & Usage
########################################


sov__faux_recurse_fix_modes() {
  local d_mode="$1"
  shift
  local f_mode="$1"
  shift
  local cur_d="$1"

  # Special Case:  we were passed a non-directory.
  # Change its mode and stop.
  if [ ! -d $cur_d ]; then
    chmod_v $f_mode $cur_d
    return $?
  fi
  # else

  # Set the mode of the first directory:
  chmod_v $d_mode $cur_d || return $?

  # Set the mode of any plain-files [non-symlinks] and
  # stash the names of any subdirs.
  local subdirs f
  for f in $cur_d/*; do
    if [ -d $f ]; then
      subdirs="$subdirs $f"
    elif [ ! -L $f ]; then
      # No need to try and chmod a symlink.
      chmod_v $f_mode $f || return $?
    fi
  done

  # Now recurse into each subdir.
  # WARNING:  This could fail rather spectacularly if
  # '$cur_d' has a very complex and deep tree structure!
  local d
  for d in $subdirs; do
    echo "Recursing into: \"$d\""
    sov__faux_recurse_fix_modes $d_mode $f_mode $d \
      || return $?
  done
}


sov__init() {
  local the_tarball="$1"

  # Clear the $BB_BIN var if 'busybox' wasn't found.
  if [ ! -e $BB_BIN ]; then
    case "$BB_BIN" in
      echo*)
        # Unit-test mode.  NoOp
        :
        ;;
      *)
        BB_BIN=''
        ;;
    esac
  fi

  if [ ! -e $CAT_BIN ]; then
    case "$CAT_BIN" in
      echo*)
        # Unit-test mode.  NoOp
        :
        ;;
      *)
        echo "Error:  Can't find \"cat\" anywhere!"
        return 1
        ;;
    esac
  fi

  # Configure the storage area on /sdcard:
  sov__maybe_mkdir $OVERLAY_HOME || return $?

  local d
  for d in shell root etc scripts; do
    sov__maybe_mkdir $OVERLAY_HOME/$d || return $?
  done

  # Do some config in the Android Terminal area:
  local kbox_d
  if [ -d $ANDTERM_DATA_DIR ]; then
    echo "Setting mode of $ANDTERM_DATA_DIR so that"
    echo "any terminal emulator app can access it."
    chmod_v 0755 $ANDTERM_DATA_DIR || return $?

    # Now we'll also fix the modes of the common places
    # where KBox lives:
    for d in $KBOX_BASE_DIRS; do
      kbox_d=$ANDTERM_DATA_DIR/$d
      if [ -d $kbox_d ]; then
        echo "Setting mode of \"$d\" subdir to make it"
        echo "accessible."

        if [ -n "$BB_BIN" ]; then
          $BB_BIN chmod -R go+r $kbox_d || return $?
        else
          # Couldn't find 'busybox'!  We have to try and
          # do a workaround:
          sov__faux_recurse_fix_modes \
            0755 0644 $kbox_d || return $?
        fi

      fi
    done
  fi

  # If we're performing the overlay, make sure that the
  # tarball exists and that we can use "tar" on it.
  if [ -n "$the_tarball" ]; then

    if [ ! -e $the_tarball ]; then
      echo "Error:  Can't find file:  \"$the_tarball\""
      return 1
    fi

    if [ ! -e $TAR_BIN ]; then
      case "$TAR_BIN" in
        echo*)
          # Unit-test mode.  NoOp
          :
          ;;
        *)
          echo "Error:  Can't find \"tar\" anywhere!"
          return 1
          ;;
      esac
    fi

  fi
}


sov__cfg_data_local() {
  chmod_v 0755 /data/local || return $?

  sov__maybe_mkdir /data/local/bin || return $?
  # sov__maybe_mkdir does nothing if the dir exists.  We
  # want to always reset perms on '/data/local/bin'.
  chmod_v 0775 /data/local/bin || return $?
}


# FIXME___sov__usage()
sov__usage() {
  local l

  for l in "$@"; do
    echo "$l"
  done
  unset l

  # Various stuff about usage:
  #
  # 1. Each --targ will also be checked for a 'kbox' or
  #    'kbox2' subdir(s) and also add those as targets
  #    if found.
  #    [I may need to change this.]
  #
  # 2. The --ast target, if '/data/local' will actually
  #    be changed to '/data/local/bin'
  #
  # 3. If --ast target list omits '/data/local', a
  #    default startup using 'ash' will be put in place
  #    [if no such script exists.  Hmm... should be an option...
  #
  # 4. Use the '--update' task only to deploy
  #    spot-fixes.  It'll force-use '0644' unless you
  #    specify a '.perms..*' file.

  echo ""
  echo "Usage:"
  echo "    . setup-kbox.sh defaults"
  echo "$MSG_RUN_ROOTLESS"
  echo -n "    . setup-kbox.sh --targ <targdir> "
  echo "[--pkg <tarball>]"
  echo ""
  echo "The first form uses a set of sane defaults"
  echo "[or whatever you set the environment."
  echo " variables to; see below]."
  echo ""
  echo "The second form is like the first, but it"
  echo "uses the \"/data\"-directory of the"
  echo "$MSG_JACKPAL_APP_NAME app as the"
  echo "installation directory."
  echo ""
  echo "The third form is for manually installing."
  echo ""
  echo " * '-t' is the short-form of '--targ'"
  echo " * \"<targdir>\" is where you want to install"
  echo "   the KBOX environment."
  echo -n " * \"<tarball>\" is the "
  echo "\"overlay-bundle.tar.bz2\""
  echo "   file [or whatever you might have renamed it"
  echo "   to]."
  echo ""
  echo "There are also environment variables that you"
  echo "can use to configure the installation:"
  echo ""
}


####################
# Main
####################


sov__main_fn() {
  # N.B.
  #
  # For some weird reason, ending this function's name
  # with 'main' causes emacs to autoindent it wrong.
  # Curious...

  if [ -z "$1" ]; then
    sov__usage "No arguments specified!"
    return 4
  fi

  local overlayTarball=$OVERLAY_TARBALL_DEFAULT
  local default_shell=ash
  local default_etcDir dir_explicitShell
  local updateFromTo
  local targList andtermStartTargs
  local hasNonAndTermTarg newBusybox
  local tweak_kbox
  local installDefault_andterm_start

  local fullOpt rawVal xlated
  while [ -n "$1" ]; do
    case "$1" in
      -h|--help)
        sov__usage
        return 0
        ;;

      -s|--shell|--default*hell)
        shift
        fullOpt=default-shell
        default_shell=`sov__chkGetArg $fullOpt "$1"`
        if [ $? -ne 0 ]; then
          return 4
        fi
        ;;

      -e|--etc[-_]dir|--default[-_]etc[-_]dir)
        shift
        fullOpt=default-etc-dir
        default_etcDir=`sov__chkGetArg $fullOpt "$1"` \
          || return $?
        ;;

      --sd|--shell*ir)
        shift
        fullOpt=shell-dir
        dir_explicitShell=`sov__chkGetArg \
          $fullOpt "$1"` || return $?
        ;;

      -b|--busybox)
        shift
        fullOpt=busybox
        newBusybox=`sov__chkGetArg $fullOpt "$1"` \
          || return $?
        ;;

      -o|--[oO]verlay*|-p|--pkg|--tarball)
        shift
        fullOpt=overlay-tarball
        overlayTarball=`sov__chkGetArg $fullOpt "$1"` \
          || return $?
        ;;

      +o|--no-[oO]verlay*)
        overlayTarball=''
        ;;

      -u|--update)
        shift
        fullOpt=update
        updateFromTo=`sov__chkGetArg $fullOpt "$1"` \
          || return $?
        overlayTarball=''
        ;;

      -k|--kbox)
        tweak_kbox='y'
        ;;

      --idats|--install*efault[-_][aA]ndterm*tart)
        installDefault_andterm_start='y'
        ;;

      --atst|--[aA]ndterm*tart*arg)
        shift
        fullOpt=andterm_start-targ
        rawVal=`sov__chkGetArg $fullOpt "$1"` \
          || return $?

        xlated=`sov__targAlias2Dirs "$rawVal" y`
        if [ $? -eq 0 ]; then
          andtermStartTargs="$andtermStartTargs $xlated"
        fi
        ;;

      -t|--targ)
        ## Special Handling:
        #
        #  Check the next argument, but don't shift
        #  the arglist.  We just want to check it.
        rawVal=`sov__chkGetArg targ "$2"` \
          || return $?

        # If the next argument is a valid value, do
        # nothing.  '-t' [or '--targ'] was passed an
        # arg, which we'll process in the next iteration
        # through [in the '*' case].
        ;;

      -*|+*)
        sov__usage "Unsupported option:  \"$1\""
        return 4
        ;;

      *)
        xlated=`sov__targAlias2Dirs "$1"`
        if [ $? -eq 0 ]; then
          if sov__non_jackpal_targ $xlated; then
            hasNonAndTermTarg='y'
          fi

          targList="$targList $xlated"
        fi
        ;;
    esac

    shift
  done


  # Is there anything to even do???
  local noTargs=y
  if [ -n "$targList$andtermStartTargs" ]; then
    noTargs=''
  elif [ -n "$overlayTarball$newBusybox" ]; then
    noTargs=''
  fi
  if [ -n "$noTargs" ]; then
    echo "No tasks specified ... nothing to do!"
    echo ""
    return 1
  fi


  # Check that we're running as root depending on the
  # target directories.
  local needs_root
  if [ "$UID" != 0 ]; then
    sov__has_non_jackpal_targ $targList && needs_root=y

    if [ -n "$andtermStartTargs" ]; then
      sov__has_non_jackpal_targ $andtermStartTargs \
        && needs_root=y
    fi

    if [ -n "$needs_root" ]; then
      echo "You must rerun this script as root to"
      echo "use the specified target(s)."
      echo ""
      echo ""
      echo "If you haven't rooted your phone,"
      echo "1. Install the $MSG_JACKPAL_APP_NAME app"
      echo "   [if you haven't already]."
      echo "2. Rerun this script like so:"
      echo "$MSG_RUN_ROOTLESS"

      return 1
    fi
  fi

  # Perform setup steps.

  local retstat
  if sov__init $overlayTarball; then
    if sov__has_data_local $targList $andtermStartTargs
    then
      sov__cfg_data_local
      retstat=$?

      if [ $retstat -ne 0 ]; then
        echo "Failed!"
        echo ""
        echo "$MSG_COWARD"
        return $retstat
      fi
    fi
  else
    echo ""
    echo "Please fix the errors and rerun this"
    echo "script."
    echo ""
    echo "$MSG_COWARD"
    return $retstat
  fi

  # Now, if everything else worked, install.

  # 1. Extract the new overlay into $OVERLAY_HOME
  if [ -n "$overlayTarball" ]; then
    sov__extract_overlay $overlayTarball || return $?
  fi

  # Tasks (2) and (3)
  local targ
  for targ in $targList; do
    # 2. Extract the 'etc' part of the overlay to each
    #    valid targ.
    if [ -n "$overlayTarball" ]; then
      sov__extract_overlay $overlayTarball $targ \
        || return $?
    elif [ -n "$updateFromTo" ]; then
      sov__update_files $updateFromTo $targ \
        || return $?
    fi

    # - Now that we have an 'etc' subdir in this target,
    #   See if 'default_etcDir' was set.  If not,
    #   default it to the first --targ:
    if [ -z "$default_etcDir" ]; then
      default_etcDir=$targ/etc
    fi

    # 3. Tweak the kbox startup script, if present.
    if [ -n "$tweak_kbox" ]; then
      if sov__has_kbox_starter $targ; then
        sov__tweak_kbox_starter "???" $targ \
          || return $?
      fi
    fi
  done

  # 4. Update the 'andterm-start.sh' scripts:
  local andtermScript=$OVERLAY_HOME/andterm-start.sh
  local ast_has_data_local
  for targ in $andtermStartTargs; do
    if [ "$targ" = "/data/local" ]; then
      targ=/data/local/bin
    fi

    # - No default for the 'shellHome'; scan for it unless
    #   the caller wants to force a particular copy in a
    #   specific dir.
    sov__tweak_andterm_start "$andtermScript" \
      $targ "$default_shell" "$dir_explicitShell" \
      "$default_etcDir" || return $?
  done

  # 4a. Update/Create the default 'andterm-start.sh'
  #     script, if desired:
  if [ -n "$installDefault_andterm_start" ]; then
    if [ -z "$ast_has_data_local" ]; then
      sov__tweak_andterm_start "$andtermScript" \
        /data/local/bin "ash" "" "" \
        || return $?
    fi
  fi

  return 0
}


#----------
#
# Startup
#
#----------


# Initialize various envvars:

if [ -z "$UID" ]; then
  ls /root >/dev/null 2>&1 && UID=0 || UID=32768
fi


TAR_BIN=`sov__find_binary tar`
CAT_BIN=`sov__find_binary cat`
SED_BIN=`sov__find_binary sed`
GREP_BIN=`sov__find_binary grep`
BB_BIN=`sov__find_binary busybox`

if [ -n "$UNIT_TEST" ]; then
  TAR_BIN="echo $TAR_BIN"
  BB_BIN="echo $BB_BIN"
fi


#
# Run, using args provided when this was sourced.
#
sov__main_fn "$@"
x=$?


#-------------------
# Cleanup:
# Because we're sourcing this file, we need to unset all
# of the variables that it uses.
#-------------------

unset UNIT_TEST OVERLAY_TARBALL_DEFAULT \
  TAR_BIN CAT_BIN SED_BIN GREP_BIN BB_BIN \
  KBOX_BASE_DIRS KBOX_STARTER_SCRIPTS \
  CFG_TEMPLATE \
  MSG_RUN_ROOTLESS MSG_JACKPAL_APP_NAME MSG_COWARD

if [ -z "$keep_ANDTERM_DATA_DIR" ]; then
  unset ANDTERM_DATA_DIR
fi
if [ -z "$keep_OVERLAY_HOME" ]; then
  unset OVERLAY_HOME
fi
unset keep_ANDTERM_DATA_DIR keep_OVERLAY_HOME

unset -f sov__maybe_mkdir sov__find_binary \
  sov__resolve_path sov__getmode \
  sov__has_non_jackpal_targ sov__targAlias2Dirs \
  sov__cfg_data_local sov__has_data_local \
  sov__chkGetArg sov__check_extract \
  sov__has_kbox_starter sov__tweak_kbox_starter \
  sov__backup_targfile sov__extract_overlay \
  sov__update_files \
  sov__tweak_andterm_start sov__overlay_new_busybox \
  sov__faux_recurse_fix_modes \
  sov__init sov__usage \
  sov__main_fn


# 'x' is the one variable that we can't get rid of, as
# it contains the 'exit' status of this sourced script.
return $x

####################
# End
#
