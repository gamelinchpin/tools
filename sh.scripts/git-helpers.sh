#!/bin/bash
#
# Copyright (C) 2014 by John P. Weiss
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


GitHub_BaseURL=https://github.com/jpweiss
GIT_TESTER_DIR=./tmp-tester.git


############
#
# Includes & Other Global Variables
#
############


#. some.include.sh

GREP=grep
SED=sed
AWK=awk
LS=ls


############
#
# Functions
#
############


github_git() {
    local op="$1"
    shift

    local gitStat=127
    case "$op" in
        clone)
            local repos="$1"
            git clone ${GitHub_BaseURL}/$repos
            gitStat=$?
            ;;
        *)
            echo "Supported: git clone"
            ;;
    esac

    return $gitStat
}


git_tester() {
    if [[ ! -d $GIT_TESTER_DIR ]]; then
        mkdir -v $GIT_TESTER_DIR || return $?
    fi

    if [[ ! -d $GIT_TESTER_DIR/.git ]]; then
        git init $GIT_TESTER_DIR || return $?
    fi

    pushd $GIT_TESTER_DIR

    if [[ ! -e README.tmp ]]; then
        echo "This git repo is for testing git commands." >README.tmp
        echo "" >>README.tmp
        echo "" >>README.tmp
        echo "And this README file is merely a placeholder." >>README.tmp
        echo "" >>README.tmp
        git add -- README.tmp
        git commit -m "Creating test repos" -q -- README.tmp
    fi
}


utl_notaDirArg() {
    local path="$1"
    shift
    local optName="$1"
    shift

    # Returns 'true' on *error*, 'false' if $path is OK.
    local status=1

    if [ -z "$path" ] || [[ ! -d $path ]]; then
        status=0
        if [ -n "$optName" ]; then
            echo "Error:  not a directory:  \"$path\""
            echo "\"$optName\" requires a valid directory."
            echo ""
        fi
    fi

    return $status
}


utl_git_noisy_checkout() {
    local checkoutOpts
    local nuBranch
    local xtraArgs
    local errmsgOp="change to"
    while [ -n "$1" -a "$1" != "---" ]; do
        case "$1" in
            -b)
                checkoutOpts="$checkoutOpts $1"
                errmsgOp="create new"
                ;;
            --)
                xtraArgs="$xtraArgs $1"
                ;;
            -*)
                checkoutOpts="$checkoutOpts $1"
                ;;
            *)
                if [ -z "$nuBranch" ]; then
                    nuBranch="$1"
                else
                    xtraArgs="$xtraArgs $1"
                fi
                ;;
        esac

        shift
    done
    [ "$1" = "---" ] && shift

    git checkout $checkoutOpts $nuBranch $xtraArgs || hasErrs='y'

    if [ -n "$hasErrs" ]; then
        echo ""
        echo ">> Could not $errmsgOp branch:  \"$nuBranch\""
        local msg
        for msg in "$@"; do
            echo -e "$msg"
        done
        return 1
    fi

    return 0
}


utl_git_noisy_merge() {
    local mergeArgs
    while [ -n "$1" -a "$1" != "---" ]; do
        mergeArgs="$mergeArgs $1"
        shift
    done
    [ "$1" = "---" ] && shift

    git merge $mergeArgs || hasErrs='y'

    if [ -n "$hasErrs" ]; then
        echo ""
        echo ">> Could not complete merge $1"
        shift
        local msg
        for msg in "$@"; do
            echo -e "$msg"
        done
        return 1
    fi

    return 0
}


git_unique_tag() {
    local theTag showUsage_retval

    # Scan the args to find the tag and validate the options.
    local o consumeNextNonOption
    for o in "$@"; do
        if [ -n "$consumeNextNonOption" ]; then
            consumeNextNonOption=''
            continue
        fi # else:

        case "$o" in
            help)
                # Handle immediately.
                git help tag
                return 0
                ;;
            -[dlnv]|--list|--delete|--verify)
                # Only tag-adding supported.
                if [ "$o" = "-n" ]; then
                    # '-n' only used with '-l' option.
                    o='-l'
                fi
                echo "\"$o\" option not supported."
                echo ""
                showUsage_retval=1
                break
                ;;
            -h|--help)
                showUsage_retval=0
                ;;
            -[umF])
                # These options require an argument.
                consumeNextNonOption=y
                ;;
            -*)
                :
                ;;
            *)
                theTag="$o"
                break
                ;;
        esac
    done

    # Usage
    if [ -n "$showUsage_retval" ]; then
        echo "usage:  git_unique_tag {help|<git-tag-adding-options>}"
        echo ""
        echo "You can use the 'help' keyword to get the help for 'git tag'"
        echo "itself."
        echo "This helper-function only supports tag *adding*.  Trying"
        echo "to use it to list, delete, or verify tags is an error."
        echo ""
        echo "If you try to create a tag that already exists, this function"
        echo "silently does nothing.  ['git tag' will fail with an error"
        echo "message.]"

        return $showUsage_retval
    fi

    # Check if the tag exists.
    tagExists="$(git tag -l $theTag)"
    if [ -n "$tagExists" ]; then
        return 0
    fi
    # else:

    # Create the new tag, with the specified args as passed.
    git tag "$@"
}


git_patchpull() {
    local invokedAs="git_patchpull $@"

    local srcRepos destRepos destDir startFrom tagSrcRepos reusePatch
    local performMerge=y
    local showUsage_retval
    while [ -n "$1" -a -z "$showUsage_retval" ]; do
        case "$1" in
            -s|--src|--srcRepos|--src[-_]repos)
                shift
                if utl_notaDirArg "$1" "--srcRepos"; then
                    showUsage_retval=1
                else
                    srcRepos="$1"
                fi
                ;;

            -d|--dest|--destRepos|--dest[-_]repos)
                shift
                if utl_notaDirArg "$1" "--destRepos"; then
                    showUsage_retval=1
                else
                    destRepos="$1"
                fi
                ;;

            --destDir|--dest[-_]dir)
                shift
                if [ -z "$1" ]; then
                    echo "Option \"--destDir\" requires a value."
                    showUsage_retval=1
                else
                    destDir="$1"
                fi
                ;;

            --from|--startFrom|--start[-_]from)
                shift
                if [ -z "$1" ]; then
                    echo "Option \"--startFrom\" requires a revision."
                    showUsage_retval=1
                else
                    startFrom="$1"
                fi
                ;;

            --noTag|--no[-_]tag)
                tagSrcRepos=''
                ;;

            --reusePatch|--reuse[-_]patch)
                reusePatch=y
                ;;

            --skipMerge|--noMerge|--no[-_]merge)
                performMerge=''
                ;;

            -h|--help)
                showUsage_retval=0
                ;;

            *)
                echo "Unknown option:  \"$1\""
                showUsage_retval=1
                ;;
        esac
        shift
    done

    # Validatation
    if [ -z "showUsage_retval" ]; then
        if [ -z "$srcRepos" ]; then
            echo "\"--srcRepos\" missing!"
            showUsage_retval=1
        fi
        if [ -z "$destRepos" ]; then
            echo "\"--destRepos\" missing!"
            showUsage_retval=1
        fi
        if [ "$srcRepos" = "." -o "$srcRepos" = ".." ]; then
            echo "\"--srcRepos\" cannot be \".\" or \"..\"!"
            showUsage_retval=1
        fi
    fi

    #
    # Usage:
    #

    if [ -n "$showUsage_retval" ]; then
        [ $showUsage_retval -ne 0 ] && echo ""

        echo "usage:  git_patchpull {-s|--srcRepos} <gitReposDir> \\"
        echo "    {-d|--destRepos} <gitReposDir> \\"
        echo "    [--destDir <name>] [--startFrom <revision|tag>] \\"
        echo "    [--noTag] [--skipMerge|--noMerge] [--reusePatch]"
        echo ""
        echo -n "The \"--destDir\" defaults to the base-pathname of "
        echo "the \"--srcRepos\" directory."
        echo ""
        echo "The \"--noTag\" option skips tagging of the \"--srcRepos\"."
        echo "Useful if you're using the same source repository multiple"
        echo "times."
        echo ""
        echo "The \"--noMerge\" option skips the 'git merge' onto the master"
        echo "branch."
        echo ""
        echo "If the patch file already exists, it's normally recreated.  Use"
        echo "the  \"--reusePatch\" option use the old file.  [The "
        echo "\"--noTag\" option will also be activated.]"

        return $showUsage_retval
    fi

    #
    # The actual steps
    #

    local srcModule="${srcRepos##*/}"
    if [ -z "$destDir" ]; then
        destDir="$srcModule"
    fi

    local today=$(date +%Y%m%d)
    local tagName="newXfer${today}"
    if [ -n "$startFrom" ]; then
        tagName="sync${today}"
    else
        startFrom=--root
    fi

    local hasErrs
    local branchName="project.subrepos__$srcModule"
    local patchFile="$PWD/${srcModule}-patch-$tagName.mbox"

    local cowardErrmsg=">> Cowardly refusing to continue."


    # Let's start by generating that patch:

    if [[ -e $patchFile ]]; then
        if [ -z "$reusePatch" ]; then
            rm -f $patchFile >/dev/null 2>&1
        else
            echo ">> Reusing existing patch file:  \"$patchFile\""
        fi
    fi

    # Generate only if needed.
    if [[ ! -e $patchFile ]]; then
        pushd $srcRepos
        echo ""
        git format-patch --stdout $startFrom >$patchFile
        if [[ ! -s $patchFile ]]; then
            popd
            echo ""
            echo ">> Patch creation failed.  $cowardErrmsg"
            return 4
        fi

        # Before leaving, tag the revision that we just exported:
        if [ -n "$tagSrcRepos" ]; then
            git_unique_tag -m "Tagging \"patchpull\" to:" -m "$destDir" \
                $tagName
        fi
        popd
        echo ""
    fi

    # Now we'll set up the branch to import the patch into:
    pushd $destRepos >/dev/null 2>&1
    echo ""

    local common_errmsg="\nConsider doing a 'git reset'."
    common_errmsg="${common_errmsg}\n>>\n>>Remaining in"
    common_errmsg="$common_errmsg directory \"$destRepos\".\n"

    utl_git_noisy_checkout -b $branchName --- \
        "$common_errmsg" || hasErrs='y'
    [ -n "$hasErrs" ] && return 5

    # Now we import:
    git am --directory="${destDir}" $patchFile || hasErrs='y'
    if [ -n "$hasErrs" ]; then
        echo ""
        echo ">> Patch failed!!!"
        echo ">> [Examine patch file \"$patchFile\" for clues to the cause.]"
        echo ">>"
        echo "$cowardErrmsg"
        return 6
    fi

    # Add info to the "last sync" file and commit.
    local syncInf="$destDir/.sync2git_info"
    echo "Ran: '$invokedAs'" >>$syncInf
    echo "   -> Patch file:  \"$patchFile\"" >>$syncInf
    echo "   -> Source tagged with:  \"$tagName\"" >>$syncInf
    echo "" >>$syncInf

    git add $syncInf
    if [ $? -eq 0 ]; then
        git commit -m "Updated for git_patchpull '$tagName'" $syncInf
    fi

    # Finally, merge onto the head.

    utl_git_noisy_checkout master --- "$cowardErrmsg" || hasErrs='y'
    [ -n "$hasErrs" ] && return 5

    if [ -n "$performMerge" ]; then
        utl_git_noisy_merge -e $branchName --- \
            "from \"$branchName\" to \"master\"" || hasErrs='y'
        if [ -n "$hasErrs" ]; then
            return 7
        fi
    fi

    popd
    echo ""
    return 0
}


git_helpers_help() {
    cat <<EOF | less
    Functions:
    ----------

    github_git - Wrapper around "git" that runs commands against
                 a repository on GitHub.  The repository must be under the
                 account specified in the envvar, \$GitHub_BaseURL.

                 Used only with commands that require a remote URL.
                 Attempting to use it with any others is a no-op.

    git_patchpull - Works around limitations of "git pull" when joining
                    together multiple remote repositories into a new, single
                    repository.

                    Suppose you have to export several modules from another
                    SCM (Subversion, CVS, whatever) to a single git
                    repository.  Plus, you want to preserve the histories from
                    the modules in the 3rd-party SCMs.
                    Obviously, you'll first need to export each module to a
                    "staging" git-repository.  But then what?

                    You cannot "git pull" a remote repository into a
                    subdirectory of the destination repository.  You *could*
                    treat the "staging" git-repositories as "git submodules".
                    But then the target machine that you're going to "git
                    push" to has to be able to see the machine with the
                    staging areas.  And even then, users will need to go
                    through extra steps to get the submodules' files.

                    Copying the files won't let you preserve history.  But
                    creating a patch does.  Plus, "git am" lets you specify a
                    target-directory for your applied patch.

                    So that's what this function does.  It also uses some
                    "sane defaults" for directory names.  Oh ... and every
                    patch-import is tagged both at the "staging"
                    git-repository and the destination repository.

    git_tester - Create a testing-repos in \$GIT_TESTER_DIR and "pushd" into
                 it.
                 [Skips any of the repo-creation steps it doesn't need.]

    git_unique_tag - Silently do nothing if adding a non-unique tag name.

                     The 'git tag' command already requires you to use a
                     unique name when creating a new tag.  It will fail
                     with an error message otherwise.  This helper-function
                     does the opposite.


    Aliases:
    --------

    None so far.


    Envvars:
    --------

    \$GitHub_BaseURL - The URL to the GitHub account used by "github_git".
    \$GIT_TESTER_DIR - The name of the temporary test-repository used by
                       "git_tester".  Can also be a path name.
EOF
}


############
#
# Main
#
############


case "$0" in
    *bash)
        echo "Run \"git_helpers_help\" for a list of the tools created by"
        echo "this file."

        ;;
    *)
        # Was run as a script.
        echo "# $0: You must source this script."
        ;;
esac


#################
#
#  End
