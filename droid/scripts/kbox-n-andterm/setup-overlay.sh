# -*- mode: sh; fill-column: 56; sh-basic-offset: 2 -*-
#
# This file should be sourced.


####################
# Config Envvars
####################


JACKPAL_DATADIR=/data/data/jackpal.androidterm
TARGDIR=${TARGDIR:-$JACKPAL_DATADIR}
OVERLAY_TARBALL=${OVERLAY_TARBALL:-$PWD/overlay-bundle.tar.bz2}


####################
# Other Envvars
####################


TMP_BIN_BASE=/app-cache
# FIXME:  I may just require a working 'busybox'
# someplace on the PATH.
TMP_BIN=$TMP_BIN_BASE/s.ovl.tmp.bin

TAR_BIN=`sov__find_binary tar $TMP_BIN`
SED_BIN=`sov__find_binary sed $TMP_BIN`
GREP_BIN=`sov__find_binary grep $TMP_BIN`

if [ -z "$UID" ]; then
  ls /root >/dev/null 2>&1 && UID=0 || UID=32768
fi

CFG_TEMPLATE='###FOR::MOD::DURING::INSTALL###'

MSG_RUN_ROOTLESS='    . setup-overlay.sh AndroidTerminal'
MSG_JACKPAL_APP_NAME="\"Android Terminal Emulator\""

export TAR_BIN SED_BIN TMP_BIN TARGDIR OVERLAY_TARBALL


####################
# Functions
####################


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
    rm $targlink || return $?
  elif [ -e $targlink ]; then
    mv $targlink ${targlink}.~bak~ || return $?
  fi

  # At this point, there should be nothing left in the way
  ln -s "$targlink" "$origFile" || return $?
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
  chmod $numod $targFile
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

  echo "cp:  '$origFile' -> '/data/local/bin/'"
  cp $origFile $targDir || return $?

  # Stop now unless we're setting the new mode.
  [ -z "$numod" ] && return 0

  local targFile="$targDir"
  if [ -d $targDir ]; then
    targFile="${targFile%/}/${origFile##*/}"
  fi

  chmod_v $numod "$targFile"
}


sov__maybe_mkdir() {
  local nuDir="$1"
  shift
  local numod="$1"
  shift

  if [ -d $nuDir ]; then
    return 0
  fi

  echo "Creating:  \"$nuDir\""
  mkdir "$nuDir" || return $?

  # Stop now unless we're setting the new mode.
  [ -z "$numod" ] && return 0

  chmod_v $numod "$nuDir"
}


sov__find_binary() {
  local progNm="$1"
  shift
  local defaultDir="$1"
  shift

  local d
  for d in xbin bin; do
    if [ -e /system/$d/$progNm ]; then
      echo /system/$d/$progNm
      return 0
    fi
  done

  echo $defaultDir/$progNm
  return 1
}


FIXME___sov__init() {
  sov__maybe_mkdir /sdcard/home || return $?

  for d in shell root etc scripts; do
    sov__maybe_mkdir /sdcard/home/$d || return $?
  done

# if [ -n "$MOVE_OR_REMOVE_THIS" ]; then
# if [ -e /sdcard/home/andterm-start.sh ]; then
#   echo -n "Already exists: "
#   echo "/sdcard/home/andterm-start.sh"
#   echo "Not overwriting.  If you want to use the newer"
#   echo "file, execute the following:"
#   echo "  cp $PWD/cp andterm-start.sh /sdcard/home/"
# elif [ -e andterm-start.sh ]; then
#   echo "cp:  andterm-start.sh -> /sdcard/home/"
#   cp andterm-start.sh /sdcard/home/ || return $?
# else
#   echo "Error:  Could not copy \"andterm-start.sh\""
#   echo "to \"/sdcard/home/\"."
#   echo "You will need to do this manually."
# fi
# fi

  if [ ! -e $TAR_BIN ]; then
    echo "Error:  Can't find \"tar\" anywhere."
    echo "Make sure you're sourcing this script from the"
    echo "same directory that contains \"tar\"."
    echo ""
    echo "Alternatively, you can set the \$TAR_BIN"
    echo "environment variable to point to the \"tar\""
    echo "executable."

    return 1
  fi

  case "$OVERLAY_TARBALL" in
    /*)
      :
      ;;
    *)
      echo "Converting \$OVERLAY_TARBALL to an absolute"
      echo "path."
      OVERLAY_TARBALL=$PWD/$OVERLAY_TARBALL
      ;;
  esac

  if [ ! -e $OVERLAY_TARBALL ]; then
    echo "Error:  Cannot find file:  \"$OVERLAY_TARBALL\""
    echo "Without this tar-file, this script can't"
    echo "do ANYTHING."
    echo ""
    echo "Make sure you're sourcing this script from the"
    echo "same directory containing \"$OVERLAY_TARBALL\"."
    echo "Alternatively, you can set the environment"
    echo "variable \$OVERLAY_TARBALL to point to the "
    echo "\"overlay-bundle.tar.bz2\" file [or whatever you"
    echo "might have renamed it to]."
    return 1
  fi
}


sov__init_binTools() {
  local bbBin="$1"

  # Let's make sure that the version of 'tar' we're
  # using can support the '-z' option:
  if [ -x $TAR_BIN ]; then
    local tarUsg=`$TAR_BIN 2>&1`
    case "$tarUsg" in
      gzip| z )
        :
        ;;
      *)
        TAR_BIN=$TMP_BIN/tar
        ;;
    esac
  fi


  if [ -x $TAR_BIN -a -x $SED_BIN ]; then
    # Unset this to signal that we don't need to remove
    # anything:
    unset TMP_BIN
    return 0
  fi
  # else:

  sov__maybe_mkdir $TMP_BIN 0777 || return $?

  if [ ! -e $TMP_BIN/$bbBin ]; then
    cp_v $bbBin $TMP_BIN 0755 || return $?
  elif [ ! -x $TMP_BIN/$bbBin ]; then
    chmod_v 0755 $TMP_BIN/$bbBin || return $?
  fi

  local tool
  for tool in tar sed; do
    if [ -e $TMP_BIN/$tool ]; then
      ln_sf $TMP_BIN/$bbBin $TMP_BIN/$tool || return $?
    fi
  done

  local retval=0
  for tool in $TAR_BIN $SED_BIN; do
    if [ ! -x $tool ]; then
      retval=1

      echo "Internal Error!"
      echo "Not a built-in and not a symlink we just"
      echo "set up:"
      echo "  \"$tool\""
      echo "This is a fatal error and will require"
      echo "manual intervention."
      echo ""
    fi
  done

  return $retval
}


sov__init_data_local() {
  chmod_v 0755 /data/local || return $?
  chmod_v 0775 /data/local/bin || return $?

}


sov__cleanup_binTools() {
  if [ -n "$TMP_BIN" ]; then
    local f
    # We have to do this in a loop, in case '$TMP_BIN'
    # is already empty [in which case, 'rm' looks for
    # the literal file, '*'].
    for f in $TMP_BIN/*; do
      if [ -e $f ]; then
        rm -f $f || return $?
      fi
    done

    rmdir $TMP_BIN || return $?
  fi

  return 0
}


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

  $SED_BIN $sed_cmds $theScript | \
    $GREP_BIN -v "$CFG_TEMPLATE" >$theTarg \
    || return $?

  chmod_v 0644 $theTarg
}


sov__data_local_andterm_start() {
  # FIXME:  I may inline this.
  sov__tweak_andterm_start "$1" /data/local/bin \
    "ash" "" ""
}


sov__extract_overlay() {
  local targdirBase_etcFiles="$1"
  shift

  local compType
  case "$OVERLAY_TARBALL" in
    *.tgz|*.tar.gz)
      compType="-z"
      ;;
    *.bz2)
      compType="-j"
      ;;
  esac

  if [ -z "$targdirBase_etcFiles" ]; then
    echo "Unarchiving overlay..."

    $TAR_BIN $compType -xf $OVERLAY_TARBALL \
      -C /sdcard/home || return $?
  else
    local etcDir=$targdirBase_etcFiles/etc

    # Make a backup file of 'profile', if this is the
    # first time we're overwriting it.
    local theProfile=$etcDir/profile
    if [ -e $theProfile -a ! -e ${theProfile}.orig ]
    then
      echo "Backing up original 'etc/profile'..."
      mv $theProfile ${theProfile}.orig || return $?
    fi

    echo "Unarchiving overlay to:"
    echo "    '$etcDir'"

    $TAR_BIN $compType -xf $OVERLAY_TARBALL \
      -C $targdirBase_etcFiles 'etc/*' || return $?
  fi

  echo "...Done!"
}


sov__tweak_kbox_starter() {
  local nuShellStarter="$1"
  shift
  local targdirBase="$1"
  shift

  local f tf

  for f in start_bash.sh start_shell.sh; do
    tf=$targdirBase/bin/$f

    if [ -e $tf ]; then
      echo "Backup up original '$f'..."
      if [ ! -e ${tf}.orig ]; then
        mv $tf ${tf}.orig || return $?
      fi

      echo "Replacing '$f'..."
      cp_v $nuShellStarter $tf 0755
    fi

  done
}


symlink_targ() {
  if [ "$1" = "-a" ]; then
    to_abspath=y
    shift
  fi
  local symlink="$1"

  set -- `ls -l $symlink 2>/dev/null`

  while [ -n "$1" -a "$1" != "->" ]; do
    shift
  done

  local targ="$2"

  # Convert to an absolute path, if desired.
  if [ -n "$to_abspath" ]; then
    case "$targ" in
      ../*|./*|*/./*|*/../*|*/.|*/..)
        local owd="$PWD"
        local sld="${symlink%/*}"
        [ -n "$sld" ] && cd $sld
        local tbn="${targ##*/}"
        local td="${targ%/*}"
        [ -n "$td" ] && cd $td
        targ="$PWD/$tbn"
        cd $owd
        ;;
    esac
  fi

  echo "$targ"
}


sov__overlay_new_busybox() {
  # Will put a different busybox binary in place of the
  # existing one, and will refresh the symlinks
  # accordingly.

  local bb_bin=`type busybox`
  bb_bin=${bb_bin##busybox is }

  bb_ver=`$bb_bin | $bb_bin head -n 1 | \
    $bb_bin awk '{ print $2 }'`

  # TODO:
  # Move the new busybox to the same dir as the old one,
  # with a version-suffx appended if needed.
  #
  # Make vars for all of the cmds we'll need, using
  # direct calls to the new 'busybox' in its new location.
  #
  # Rename old 'busybox' if not a symlink.
  # Link 'busybox' to the new binary.
  #
  # Fix links to the 'busybox' symlink that aren't in the new version.
}


FIXME___sov__targ_is_jackpal() {
  case "$TARGDIR" in *jackpal*) return 0;; esac
  # else:
  return 1
}


FIXME___sov__targ_is_data_local() {
  case "$TARGDIR" in /data/local/*) return 0;; esac
  # else:
  return 1
}


FIXME___sov__usage() {
  local l

  for l in "$@"; do
    echo "$l"
  done
  unset l

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
  echo "  TARGDIR"
  echo "    The installation directory"
  echo "  OVERLAY_TARBALL"
  echo -n "    The location of the "
  echo "\"overlay-bundle.tar.bz2\""
  echo "    file [or whatever you might have renamed it"
  echo "    to]."
  echo "Use the first form of this script if you want to"
  echo "use these environment variables instead of"
  echo "passing arguments on the commandline.  You can"
  echo "also use the second form, but it overrides the"
  echo "value of 'TARGDIR'."
  echo ""
}


####################
# Main
####################


coward_msg="Cowardly refusing to continue."

if [ -z "$1" ]; do
  has_err=y
  sov__usage "No arguments specified!"
fi

# FIXME:  I want to be able to:
#   1.  Just set up for using a built-in 'bash' [or
#       other shell].
#   2.  Set up for 'kbox'.
#   3.  Set up for 'kbox2'.
#   4.  Set up for other locations.
#   5.  Update a 'busybox'.

while [ -n "$1" ]; do
  case "$1" in
    AndroidTerminal)
      TARGDIR=$JACKPAL_DATADIR
      ;;

    defaults)
      :
      ;;

    -t|--targ)
      if [ -z "$2" ]; then
        has_err=y
        sov__usage \
          "Option \"--targ\" requires an argument."
      else
        shift
        TARGDIR="$1"
      fi
      ;;

    --pkg)
      if [ -z "$2" ]; then
        has_err=y
        sov__usage \
          "Option \"--pkg\" requires an argument."
      else
        shift
        OVERLAY_TARBALL="$1"
      fi
      ;;

    *)
      has_err=y
      sov__usage "Unsupported arg:  \"$1\""
      ;;
  esac

  shift
done


# Test that we're not installing to vfat.

case "$TARGDIR" in /sdcard/*)
  has_err=y

  echo "ERROR:  The target directory cannot be on a"
  echo "DOS/Windows filesystem.  And /sdcard/ is"
  echo "definitely a DOS/Windows filesystem."
  echo ""
  echo "$coward_msg"
  ;;
esac

# Check that we're running as root depending on the
# $TARGDIR

if [ -z "$has_err" -a "$UID" != 0 ]; then
  if sov__targ_is_jackpal; then
    :
  else
    has_err=y

    echo "You cannot use \"$TARGDIR\""
    echo "as the target directory unless you are root."
    echo ""
    echo "If you haven't rooted your phone,"
    echo "1. Install the $MSG_JACKPAL_APP_NAME app [if"
    echo "   you haven't already]."
    echo "2. Rerun this script like so:"
    echo "$MSG_RUN_ROOTLESS"
  fi
fi

# Perform setup steps.

if [ -z "$has_err" ]; then
  if sov__init; then

    if sov__targ_is_data_local; then
      sov__cfg_data_local || has_err=y
    else
      echo "Setting mode of the \$TARGDIR so that any"
      echo "terminal emulator app can use it."
      chmod 0755 $TARGDIR || has_err=y
    fi

    if [ -n "$has_err" ]; then
      echo "Failed!"
      echo ""
      echo "$coward_msg"
    fi

  else
    has_err=y

    echo ""
    echo "Please fix the errors and rerun this script."
    echo ""
    echo "$coward_msg"
  fi
fi

# Now, if everything else worked, install.

if [ -z "$has_err" ]; then
  if sov__extract; then

    echo ""
    echo "Installation succeeded!"

    echo ""
    echo "Now, configure $MSG_JACKPAL_APP_NAME"
    echo "so that the 'Initial Command' preference"
    echo "contains:"

    if sov__targ_is_data_local; then
      echo "  '. /data/local/bin/andterm-star.sh'"
      echo "or:"
    fi
    echo "  '. /sdcard/home/andterm-star.sh'"

    echo "[removing the surrounding '', of course]."

    echo ""
    echo "If you're using some other terminal emulator"
    echo "app, find the equivalent setting and modify"
    echo "as described above.  If there is no such"
    echo "setting, you'll have to perform the above"
    echo "command manually each time you open a terminal."

  else

    echo ""
    echo "Extraction Failed!"
    echo ""
    echo "Please determine the cause of the problem,"
    echo "try to correct it, then rerun this script."

  fi
fi


####################
# Cleanup:
# Because we're sourcing this file, we need to unset all
# of the variables that it uses.
unset TARGDIR JACKPAL_DATADIR
unset TMP_BIN_BASE TMP_BIN TAR_BIN SED_BIN
unset MSG_RUN_ROOTLESS
unset d has_err coward_msg
unset sov__usage sov__init sov__extract
unset sov__targ_is_jackpal
unset sov__targ_is_data_local sov__cfg_data_local
unset sov__find_binary sov__tweak_andterm_start sov__overlay_new_busybox
