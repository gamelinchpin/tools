#!/bin/sh
#
# Copyright (C) 2004-2010 by John P. Weiss
#
# This package is free software; you can redistribute it and/or modify
# it under the terms of the Artistic License, included as the file
# "LICENSE" in the source code archive.
#
# This package is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# You should have received a copy of the file "LICENSE", containing
# the License John Weiss originally placed this program under.
#
# $Id$
############


############
#
# Configuration Variables
#
############


# Comment this out if you don't have the GNU version of 'mv' on this machine.
GNUmvOpts='--backup=numbered --suffix=,'

# Comment this out if you don't have the GNU version of 'find'.  (The ','
# operator may be specific to GNU find.)
GNUfindPrint=", -print"

# Use the -f option with all varieties of 'mv'
mvOpts="-f${GNUmvOpts:+ }${GNUmvOpts}"

purgedir="/tmp/purged"


############
#
# Includes & Other Global Variables
#
############


twiddles='-name *~ -o -name .*~'
hashes='-name #*# -o -name .*#'
stashes='-name .saves* -o -name *.lyx.bak'

# Disable globbing while building these variables.  This will ensure that the
# shell always quotes the * and ~ chars in the strings below.
set -f
namelst="( ${twiddles} -o ${hashes} -o ${stashes} )"
skippurgedir="( -path ${purgedir} -prune ) -o"
set +f


FIND=/usr/bin/find


############
#
# Functions
#
############


cleanup()
{
    if [ "${PWD}" = "${purgedir}" ]; then
	    echo "Can't purge the purge directory!  Do this by hand."
	    exit 1
    fi

    depthopt="-maxdepth 1"
    if [ "$1" = "-r" -o "$1" = "-R" ]; then
        depthopt=''
    fi

    # Alas, we need to disable globbing here, too.  (See comment with
    # "namelst" for details.)
    echo -n "Purging files: "
    set -f
    $FIND . ${depthopt} ${skippurgedir} \
        '(' \! -type d -a ${namelst} ')' \
        '(' -exec mv ${mvOpts} '{}' ${purgedir} ';' ${GNUfindPrint} ')'
    set +f

    # Use the non-builtin, which lets us use the -n and -e options.
    /bin/echo -ne "\r"
    /bin/echo -n "                                        "
    /bin/echo -n "                                        "
    /bin/echo -ne "\r"
    echo ""
}

safe_rm()
{
    if [ -n "$GNUmvOpts" ]; then
        mv -v $mvOpts "$@" $purgedir
        return $?
    fi
    # else:
    # Do the 'mv' one at a time, printing out what it's doing afterwards if
    # the 'mv' succeeds
    for f in "$@"; do
        mv $mvOpts "$f" $purgedir && echo "'$f' -> $purgedir"
    done
}


############
#
# Main
#
############


if [ ! -d ${purgedir} ]; then
	echo "Purge directory does not exist."
	mkdir ${purgedir}
	chmod a+rwxt ${purgedir}
	echo "Created: ${purgedir}"
fi


myName="$0"
myName="${myName##*/}"
case "$myName" in
    safe-rm|safe-rm.sh|purge.sh)
        safe_rm "$@"
        ;;
    *)
        cleanup "$@"
        ;;
esac


#################
#
#  End
