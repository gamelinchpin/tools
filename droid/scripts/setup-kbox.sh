# -*- mode: sh; fill-column: 56 -*-
#
# This file should be sourced.


####################
# Config Envvars
####################


JACKPAL_DATADIR=/data/data/jackpal.androidterm
TARGDIR=${TARGDIR:-$JACKPAL_DATADIR}
KBOX_TARBALL=${KBOX_TARBALL:-$PWD/kbox-bundle.tar.bz2}


####################
# Other Envvars
####################


TAR_BIN=./tar
for d in xbin bin; do
 if [ -e /system/$d/tar ]; then
  TAR_BIN=/system/$d/tar
  USING_SYSTEM_TAR=y
  break
 fi
done

if [ -z "$UID" ]; then
 ls /root >/dev/null 2>&1 && UID=0 || UID=32768
fi

export TAR_BIN TARGDIR KBOX_TARBALL

MSG_RUN_ROOTLESS='    . setup-kbox.sh AndroidTerminal'
MSG_JACKPAL_APP_NAME="\"Android Terminal Emulator\""


####################
# Functions
####################


kbx__init() {
  if [ ! -d /sdcard/home ]; then
   echo "Creating:  /sdcard/home"
   mkdir /sdcard/home || return $?
  fi

  for d in shell root etc scripts; do
   if [ ! -d /sdcard/home/$d ]; then
    echo "Creating:  /sdcard/home/$d"
    mkdir /sdcard/home/$d || return $?
   fi
  done


  if [ -e /sdcard/home/andterm-start.sh ]; then
   echo -n "Already exists: "
   echo "/sdcard/home/andterm-start.sh"
   echo "Not overwriting.  If you want to use the newer"
   echo "file, execute the following:"
   echo "  cp $PWD/cp andterm-start.sh /sdcard/home/"
  elif [ -e andterm-start.sh ]; then
    echo "cp:  andterm-start.sh -> /sdcard/home/"
    cp andterm-start.sh /sdcard/home/ || return $?
  else
   echo "Error:  Could not copy \"andterm-start.sh\""
   echo "to \"/sdcard/home/\"."
   echo "You will need to do this manually."
  fi

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

  case "$KBOX_TARBALL" in
    /*)
      :
      ;;
    *)
      echo "Converting \$KBOX_TARBALL to an absolute"
      echo "path."
      KBOX_TARBALL=$PWD/$KBOX_TARBALL
      ;;
  esac

  if [ ! -e $KBOX_TARBALL ]; then
   echo "Error:  Cannot find file:  \"$KBOX_TARBALL\""
   echo "Without this tar-file, this script can't"
   echo "do ANYTHING."
   echo ""
   echo "Make sure you're sourcing this script from the"
   echo "same directory containing \"$KBOX_TARBALL\"."
   echo "Alternatively, you can set the environment"
   echo "variable \$KBOX_TARBALL to point to the "
   echo "\"kbox-bundle.tar.bz2\" file [or whatever you"
   echo "might have renamed it to]."
   return 1
  fi
}


kbx__targ_is_jackpal() {
  case "$TARGDIR" in *jackpal*) return 0;; esac
  # else:
  return 1
}


kbx__targ_is_data_local() {
  case "$TARGDIR" in /data/local/*) return 0;; esac
  # else:
  return 1
}


kbx__cfg_data_local() {
  echo "Setting mode:  /data/local"
  chmod 0755 /data/local || return $?
  echo "Setting mode:  /data/local/bin"
  chmod 0775 /data/local/bin || return $?

  echo -n "cp:  /sdcard/home/andterm-star.sh "
  echo "-> /data/local/bin/"
  cp /sdcard/home/andterm-star.sh /data/local/bin/ \
      || return $?

  echo "Setting mode:  /data/local/bin/andterm-star.sh"
  chmod 0644 /data/local/bin/andterm-star.sh \
      || return $?
}


kbx__extract() {
  if [ -z "$USING_SYSTEM_TAR" ]; then
   if [ ! -e $TARGDIR/$TAR_BIN ]; then
    echo "Putting the package-provided \"tar\" binary"
    echo "someplace where we can execute it."
    cp $TAR_BIN $TARGDIR || return $?

    copied_tar=y
   fi
  fi

  echo "Changing to the target-directory."
  cd $TARGDIR || has_err=y

  if [ -z "$has_err" ]; then
   echo "Unarchiving KBOX..."
   tar -jxf $KBOX_TARBALL || has_err=y
   echo "...Done!"
  fi

  if [ -n "$copied_tar" ]; then
   rm $TARGDIR/$TAR_BIN
  fi

  # We don't know that the shell supports the 'local'
  # keyword, so we'll unset the vars we used.
  unset copied_tar

  # Unfortunately, we need to use the value of $has_err
  # to determine the return-value.  Which means that we
  # can't unset it from a central location.  :(
  if [ -n "$has_err" ]; then
   unset has_err
   return 1
  fi

  unset has_err
  return 0
}


kbx__usage() {
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
 echo " * \"<tarball>\" is the \"kbox-bundle.tar.bz2\""
 echo "   file [or whatever you might have renamed it"
 echo "   to]."
 echo ""
 echo "There are also environment variables that you"
 echo "can use to configure the installation:"
 echo "  TARGDIR"
 echo "    The installation directory"
 echo "  KBOX_TARBALL"
 echo "    The location of the \"kbox-bundle.tar.bz2\""
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
 kbx__usage "No arguments specified!"
fi

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
     kbx__usage \
       "Option \"--targ\" requires an argument."
    else
     shift
     TARGDIR="$1"
    fi
   ;;

  --pkg)
    if [ -z "$2" ]; then
     has_err=y
     kbx__usage \
       "Option \"--pkg\" requires an argument."
    else
     shift
     KBOX_TARBALL="$1"
    fi
   ;;

  *)
   has_err=y
   kbx__usage "Unsupported arg:  \"$1\""
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
 if kbx__targ_is_jackpal; then
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
 if kbx__init; then

  if kbx__targ_is_data_local; then
   kbx__cfg_data_local || has_err=y
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
 if kbx__extract; then

  echo ""
  echo "Installation succeeded!"

  echo ""
  echo "Now, configure $MSG_JACKPAL_APP_NAME"
  echo "so that the 'Initial Command' preference"
  echo "contains:"

  if kbx__targ_is_data_local; then
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
unset TARGDIR TAR_BIN USING_SYSTEM_TAR JACKPAL_DATADIR
unset MSG_RUN_ROOTLESS
unset d has_err coward_msg
unset kbx__usage kbx__init kbx__extract
unset kbx__targ_is_jackpal
unset kbx__targ_is_data_local kbx__cfg_data_local
