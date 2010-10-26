##################    SOURCE THIS FILE   #################
#
# !/bin/bash
# !/bin/ksh
#
# Copyright (C) 2002 by John P. Weiss
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


############
#
# Includes & Other Global Variables
#
############


#. some.include.sh

#GREP=grep
#SED=sed
#AWK=awk
#LS=ls


############
#
# Functions
#
############


add_path() {
    # Set some defaults
    do_GetPathvarVal="echo \${${PATHVAR}}"
    do_Modify='${PATHVAR}=${pathvarContents}:${dirs_added}'
    PATHVAR="PATH"

    # Parse function args.
    dirs=""
    while [ -n "$1" ]; do
        case "$1" in
            -a|--app*)
                do_Modify='${PATHVAR}=${pathvarContents}:${dirs_added}'
                ;;
            -p|--pre*)
                do_Modify='${PATHVAR}=${dirs_added}:${pathvarContents}'
                ;;
            -P)
                PATHVAR="PATH"
                ;;
            -[mM])
                PATHVAR="MANPATH"
                ;;
            -[lL])
                PATHVAR="LD_LIBRARY_PATH"
                ;;
            -[cCjJ])
                PATHVAR="CLASSPATH"
                ;;
            -v|--var*)
                shift
                PATHVAR="$1"
                ;;
            -h|--help)
                echo "add_path [-a|-p|-P|-m|-l|-c] [-v <envvar>] <dirs...>"
                ;;
            -*)
                # Ignore any other options.
                ;;
            *)
                dirs="${dirs}:${1%\/}"
                ;;
        esac
        shift
    done
    # Trim leading ':' off of $dirs.
    dirs_added="${dirs#:}"

    # The "do nothing" case:
    if [ -z "${dirs_added}" ]; then
        return 1
    fi

    # Delete any paths that are already present.
    delete_path --eval "PATHVAR=\"${PATHVAR}\"; dirs=\"${dirs_added}\";"

    # Get the existing contents of the pathvar
    pathvarContents=`eval "${do_GetPathvarVal}"`

    # Handle the empty PATHVAR case.
    if [ -z "${pathvarContents}" ]; then
        do_Modify='${PATHVAR}=${dirs_added}'
    fi

    # Okay, so this is messy.  Here's what it's doing:
    # - The inner 'eval' expands the contents of $do_Modify.  The "echo"
    #   merely gives the 'eval' a command to execute.
    # - The outer 'eval' expands the variables that were embedded inside of
    # $do_Modify before executing the modificaiton command.
    eval `eval echo ${do_Modify}`
    export "${PATHVAR}"
}


delete_path() {
    # Set some defaults
    do_GetPathvarVal="echo \${${PATHVAR}}"
    PATHVAR="PATH"

    # Parse function args.
    newpath=""
    dirs=""
    while [ -n "$1" ]; do
        case "$1" in
            --eval)
                shift
                eval "$@"
                # Eat the commandline
                set --
                ;;
            -P)
                PATHVAR="PATH"
                ;;
            -[mM])
                PATHVAR="MANPATH"
                ;;
            -[lL])
                PATHVAR="LD_LIBRARY_PATH"
                ;;
            -[cCjJ])
                PATHVAR="CLASSPATH"
                ;;
            -v|--var*)
                shift
                PATHVAR="$1"
                ;;
            -h|--help)
                echo "delete_path [-a|-P|-m|-l|-c] [-v <envvar>] <dirs...>"
                ;;
            -*)
                # Ignore any other options.
                ;;
            *)
                dirs="${dirs}:${1%\/}"
                ;;
        esac
        shift
    done
    # Trim leading ':' off of $dirs.
    dirs="${dirs#:}"

    # The "do nothing" case:
    if [ -z "${dirs}" ]; then
        return 1
    fi

    # Get the contents of the path as it stands now.  Also reset the IFS.
    pathvarContents=`eval "${do_GetPathvarVal}"`
    oldIFS="${IFS}"
    IFS=":"
    # Construct the new path.  Yes, it's O(n^2)...
    for d in ${pathvarContents}; do
        keepdir=1
        for delD in ${dirs}; do
            # Nullify the old path element under examination if it matches one
            # of the deletion targets.
            if [ "${d}" == "${delD}" ]; then
                keepdir=0
                break;
            fi
        done
        if [ ${keepdir} -eq 1 ]; then
            newpath="${newpath}:${d}"
        fi
    done
    IFS="${oldIFS}"
    unset oldIFS pathvarContents

    # Trim leading ':' off of $newpath.
    newpath="${newpath#:}"

    # Trim leading ' ' off of $dirs.
    eval "${PATHVAR}=${newpath}"
    export "${PATHVAR}"
}


dedup_path() {
    # The "do nothing" case:
    if [ -z "$1" ]; then
        return 1
    fi

    # Set some defaults
    do_GetPathvarVal="echo \${${PATHVAR}}"
    PATHVAR="PATH"

    # Parse function args.
    case "$1" in
        -P)
            PATHVAR="PATH"
            ;;
        -[mM])
            PATHVAR="MANPATH"
            ;;
        -[lL])
            PATHVAR="LD_LIBRARY_PATH"
            ;;
        -[cCjJ])
            PATHVAR="CLASSPATH"
            ;;
        *)
            PATHVAR="$1"
            ;;
    esac

    # Get the contents of the path as it stands now.  Quit if there's nothing
    # there.
    pathvarContents=`eval "${do_GetPathvarVal}"`
    if [ -z "${pathvarContents}" ]; then
        return 0
    fi

    # Reset the IFS and set the positional parameters to the reverse-order of
    # $PATHVAR
    oldIFS="${IFS}"
    IFS=":"
    newpath=""
    for d in ${pathvarContents}; do
        newpath="${d}:${newpath}"
    done
    set -- ${newpath%\:}
    newpath=""

    # Construct the new path, eliminating duplicates.  Yes, it's O(n^2)...
    while [ -n "$1" ]; do
        d="$1"
        keepdir=1
        shift
        remainingDirs="$*"
        for delD in ${remainingDirs}; do
            # Nullify the present path element if it matches something else in
            # the path.  [Remember, ${remainingDirs} is in the reverse-order
            # of the PATHVAR.]
            if [ "${d}" == "${delD}" ]; then
                keepdir=0
                break;
            fi
        done
        if [ ${keepdir} -eq 1 ]; then
            newpath="${d}:${newpath}"
        fi
    done
    IFS="${oldIFS}"
    unset oldIFS pathvarContents

    # Trim trailing ':' off of $newpath.
    newpath="${newpath%:}"

    # Trim leading ' ' off of $dirs.
    eval "${PATHVAR}=${newpath}"
    export "${PATHVAR}"
}


############
#
# Main
#
############




#################
#
#  End
