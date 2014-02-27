# -*- mode: sh; fill-column: 56 -*-
#
# This file should be sourced.


### Config:  Uncomment the type of shell you want to
###          use.  Note:  'bash' falls back to
###          'kbox_shell' when the full, standalone
###          version of bash isn't installed.

SHELL0=bash
#SHELL0=kbox_shell
#SHELL0=ash


############


PATH=/data/local/bin:$PATH
export PATH


for d in /sdcard/ext /data/local/kbox /sd-ext \
  /data/sdext2/base.dir \
  /data/data/jackpal.androidterm/kbox
do
 if [ -d $d ]; then
  dList="$dList $d"
 fi
done

for d in $dList; do
 if [ -e $d/etc/profile ]; then
  ENV=/data/local/kbox/etc/profile
  export ENV
  break
 fi
done


# Find the directory containing the requested shell
for d in $dList; do
 if [ -e $d/bin/$SHELL0 ]; then
  SHELL_DIR=$d/bin
  break
 fi
done


# Want to run bash?
if [ "$SHELL0" = "bash" ]; then
 if [ -n "$SHELL_DIR" ]; then
  if [ -L $SHELL_DIR/bash ]; then
   bash_is_bb=`ls -l $SHELL_DIR/bin/bash 2>&1 | grep busybox`
  fi

  if [ -n "$bash_is_bb" ]; then
   # We found 'bash', but it's a symlink to busybox.
   # Perform the fallback-scan.
   SHELL_DIR=''
  fi
 fi
fi


if [ -z "$SHELL_DIR" ]; then
 # Perform a fallback-scan:

 for f in kbox_shell ash; do
  for d in $dList; do
   if [ -e $d/bin/$f ]; then
    SHELL_DIR=$d/bin
    SHELL0=$f
    break
   fi
  done
 done
fi

if [ -n "$SHELL_DIR" ]; then
 exec $SHELL_DIR/$SHELL0
fi

# else:
# Fall of the end of the file and keep the startup-shell
# that sourced this file.
