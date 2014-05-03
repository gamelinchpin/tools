# -*- mode: sh; fill-column: 56; sh-basic-offset: 2 -*-
#
# This file should be sourced.


###
### Config
###


###          Uncomment the type of shell you want to
###          use.  Note:  'bash' falls back to
###          'kbox_shell' when the full, standalone
###          version of bash isn't installed.
###
###          If you want this script to find your
###          non-'busybox'-'bash', consider renaming or
###          symlinking it to the filename
###          'bash-standalone'.

#SHELL0=bash
#SHELL0=bash-standalone
#SHELL0=kbox_shell
#SHELL0=ash
#SHELL0=###FOR::MOD::DURING::INSTALL###


###          Default 'etc' Dir.
###          Leave commented out if you want to scan
###          for it instead.

#ENV_DIR=/data/local/etc
#ENV_DIR=###FOR::MOD::DURING::INSTALL###


###          Where the '$SHELL0' is located.
###          Uncomment and set if you have the same
###          shell in several locations, but want to use
###          a particular one in a specific directory.
###          Leave commented out if you want to scan for
###          it instead.

#SHELL_DIR=/data/sdext2/this/is/an/example
#SHELL_DIR=###FOR::MOD::DURING::INSTALL###


###          Some extra directories to include in the
###          scans for 'etc/profile' and '$SHELL0'.
###
###          The scan *always* starts with
###          '/data/local/kbox' and '/data/local',
###          followed by the "usual mountpoints" for the
###          sdcard's second partition.  The paths you
###          provide here are checked next.  Lastly,
###          '/data/data/jackpal.androidterm/kbox' and
###          '/data/data/jackpal.androidterm' are
###          checked.
###
###          Note:  The directories in this envvar don't
###          need to exist.  That's checked for you, and
###          nonexistent directory names omitted.
###
###          Modify to taste, or comment out.  [However, it's safe
###          to leave it.]

SCAN_DIRS="/sdcard/ext /data/sdext2/base.dir"


############


if [ -d /data/local/bin ]; then
  PATH=/data/local/bin:$PATH
  export PATH
fi


# Include the user's preferred location.
for d in /data/local /sd-ext /system/sd /data/sdext2 \
  $SCAN_DIRS /data/data/jackpal.androidterm
do
  if [ -d $searchDirs ]; then
    searchDirs="$searchDirs $d"
  fi
  if [ -d $searchDirs/kbox ]; then
    searchDirs="$searchDirs $d/kbox"
  fi
  if [ -d $searchDirs/kbox2 ]; then
    searchDirs="$searchDirs $d/kbox2"
  fi
done


# Look in '$ENV_DIR' first, then continue the scan if
# that fails.
if [ -e $ENV_DIR/profile ]; then
  etc_profile=$ENV_DIR/profile
else
  for d in $ENV_DIR $searchDirs; do
    # Make sure we don't end up looking in
    # "${foo}/etc/etc":
    d=${d%/etc}

    if [ -e $d/etc/profile ]; then
      etc_profile=$d/etc/profile
      break
    fi
  done
fi

if [ -n "$etc_profile" ]; then
  ENV="$etc_profile"
  export ENV
fi


# Make sure 'SHELL0' is set to *something*:
[ -z "$SHELL0" ] && SHELL0='not_a_real_shell'

# Find the directory containing the requested shell.
if [ -z "$SHELL_DIR" ]; then
  # ...but only if the user didn't specify a valid
  # location for one in the config-section.

  for d in $searchDirs; do
    if [ -e $d/bin/$SHELL0 ]; then
      SHELL_DIR=$d/bin
      break
    fi
  done

elif [ ! -e $SHELL_DIR/$SHELL0 ]
  # The configured shell directory doesn't contain the
  # desired shell.  Force-trigger one of the fallback
  # scans.
  SHELL_DIR=''

fi

# Fallback:  Check for any built-in version of 'bash' & 'ash'.
if [ -z "$SHELL_DIR" -a "$SHELL0" != "kbox_shell" ]; then
  for d in /system/xbin /system/bin; do
    if [ -e $d/$SHELL0 ]; then
      SHELL_DIR=$d
      break
    fi
  done
fi


# Want to run bash?
if [ "$SHELL0" = "bash" ]; then

  # Did we *find* a 'bash' executable?
  if [ -n "$SHELL_DIR" ]; then

    # Well, let's see if it's standalone or busybox-based.
    if [ -L $SHELL_DIR/bash ]; then
      bash_is_bb=`ls -l $SHELL_DIR/bash 2>&1 | grep busybox`
    fi

    if [ -n "$bash_is_bb" ]; then
      # We found 'bash', but it's a symlink to busybox.
      # Which means that it's part of a KBOX
      # installation, not a standalone 'bash'
      # executable.  So, force the fallback-scan [which
      # looks for 'kbox_shell' first].
      SHELL_DIR=''
    else
      # Set this envvar, which we'll use when we start the shell
      BASH_ENV=$ENV
      export BASH_ENV
    fi
  fi

elif [ "$SHELL0" = "bash-standalone" ]; then
  # Non-busybox-based 'bash'.

  # Set this envvar, which we'll use when we start the shell
  BASH_ENV=$ENV
  export BASH_ENV
fi


if [ -z "$SHELL_DIR" ]; then
  # We didn't find the preferred shell.
  # Perform a fallback-scan to find *some* shell:

  for f in kbox_shell ash; do
    for d in $searchDirs; do
      if [ -e $d/bin/$f ]; then
        SHELL_DIR=$d/bin
        SHELL0=$f
        break
      fi
    done

    if [ -n "$SHELL_DIR" ]; then
      break
    fi
  done
fi


# Replace the current shell with the one we found [*if*
# we found one]:
if [ -n "$SHELL_DIR" ]; then
  SHELL=$SHELL_DIR/$SHELL0
  export SHELL

  exec $SHELL ${BASH_ENV:+--rcfile} ${BASH_ENV}
fi

# else:
# Fall of the end of the file and keep the startup-shell
# that sourced this file.
