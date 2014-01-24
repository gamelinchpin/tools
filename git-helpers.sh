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
            shift
            git clone ${GitHub_BaseURL}/$repos
            gitStat=$?
            ;;
        *)
            echo "Supported: git clone <repos>"
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


utl_isaRemoteBranch() {
    local remoteId="$1"
    shift
    local branchName="$1"
    shift

    [ -z "$branchName" ] && branchName="master"

    local actualRemoteBranch="$(git branch -r | \
                                grep $remoteId | \
                                sed -e 's/^  *//' -e 's/  *$//')"

    if [ "$branchName" = "--" ]; then
        # Special case:  Just check if there was a match to the $remoteId.
        [ -n "$actualRemoteBranch" ] && return 0
        # else:
        return 1
    fi
    # else:
    # Check for an exact match.

    [ "$actualRemoteBranch" = "$remoteId/$branchName" ] && return 0
    # else:
    return 1
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
        echo "Error:  not a directory:  \"$path\""
        if [ -n "$optName" ]; then
            echo "\"$optName\" requires a valid directory."
            echo ""
        fi
    fi

    return $status
}


utl_notaGitRepo() {
    local path="$1"
    shift
    local optName="$1"
    shift

    # Returns 'true' on *error*, 'false' if $path is OK.
    utl_notaDirArg "$path" "$optName" && return 0

    local status=1
    if [[ ! -d $path/.git ]] || [[ ! -e $path/.git/config ]]; then
        status=0
        echo "Error:  not a git-repository:  \"$path\""
        if [ -n "$optName" ]; then
            echo "\"$optName\" requires a git-repo dir."
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
    local branchPrefix
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

            --destBranch|--dest[-_]branch)
                shift
                if [ -z "$1" ]; then
                    echo -n "Option \"--destBranch\" requires a branch-name "
                    echo "prefix."
                    showUsage_retval=1
                else
                    branchPrefix="$1"
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
    if [ -z "$showUsage_retval" ]; then
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
        echo "    {--destBranch} <branchPrefix> \\"
        echo "    [--noTag] [--skipMerge|--noMerge] [--reusePatch]"
        echo ""
        echo -n "The \"--destDir\" defaults to the base-pathname of "
        echo "the \"--srcRepos\" directory."
        echo ""
        echo "The \"--noTag\" option skips tagging of the \"--srcRepos\"."
        echo "Useful if you're using the same source repository multiple"
        echo "times."
        echo ""
        echo "If you want to pull the patch into a branch in the destination"
        echo "git repository, use the \"--destBranch\" option.  The name"
        echo -n "of the target-branch will be "
        echo "\"<branchPrefix>\"+'._'+\"<srcDir>\", "
        echo "where \"<srcDir>\" is the base-pathname of the directory passed"
        echo "to the \"--srcRepos\" option."
        echo ""
        echo "The \"--noMerge\" option skips the 'git merge' onto the master"
        echo "branch.  It's ignored unless the \"--destBranch\" option was"
        echo "also used."
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

    local branchName
    [ -n "${branchPrefix}" ] && branchName="${branchPrefix}._$srcModule"
    local patchFile="$PWD/${srcModule}-patch-$tagName.mbox"

    local hasErrs
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

    if [ -n "$branchName" ]; then
        # Only create the new branch if a remote version of it doesn't already
        # exist.
        local createBranch="-b"
        if utl_isaRemoteBranch $branchName --; then
        # (Remote) Branch exists; don't create.
            createBranch=''
        fi

        utl_git_noisy_checkout $createBranch $branchName --- \
            "$common_errmsg" || hasErrs='y'
        [ -n "$hasErrs" ] && return 5
    fi

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


    if [ -n "$branchName" ]; then
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
    fi

    popd
    echo ""
    return 0
}


utl_git_subtree_viaMerge_sync() {
    local targBranch="master"

    if [ -z "$1" ]; then
        echo ">> At least one source git 'tree-ish' must be specified."
        echo ">> "
        return 127
    fi

    if utl_notaGitRepo "$PWD"; then
        echo ">> This command must be run from inside of a git-repo."
        echo ">> "
        return 127
    fi

    local remote gitStat ready2sync
    while [ -n "$1" ]; do
        ready2sync=y

        case "$1" in
            -b|--branch)
                shift
                targBranch="$1"
                ready2sync=''

                if [ -z "$targBranch" ]; then
                    echo -n ">> \"-b\" option requires the name of a target "
                    echo "branch."
                    echo ">>"
                    return 127
                fi

                if [ -z "$2" ]; then
                    echo -n ">> \"-b\" option can't be the last on the "
                    echo "commandline."
                    echo ">>"
                    return 127
                fi
                ;;

            -s|--source)
                shift
                remote="$1"
                ;;

            -*)
                echo ">> Unsupported option:  \"$1\""
                echo ">>"
                return 127
                ;;

            *)
                remote="$1"
                ;;
        esac
        shift

        # Perform the sync if we're ready.
        #
        if [ -n "$ready2sync" ]; then
            if [ -z "$remote" ]; then
                echo ">> The source git 'tree-ish' cannot be \"\"!"
                echo ">>"
                return 127
            elif utl_isaRemoteBranch $remote; then
                :
            else
                echo ">> This git-repo contains no remote called \"$remote\""
                echo ">>"
                return 127
            fi

            echo ">> Syncing \"$remote\" to \"$targBranch\"."
            git pull -v -s subtree "$remote" "$targBranch"
            gitStat=$?

            if [ $gitStat -ne 0 ]; then
                return $gitStat
            fi

            git add --all .subtrees && \
                git commit -m "Updating subtree info for \"$remote\"" .subtrees
            gitStat=$?
            if [ $gitStat -ne 0 ]; then
                return $gitStat
            fi
        fi
    done

    return $gitStat
}


utl_git_subtree_modifyRemotePullURLs() {
    local cmd="$1"
    shift

    local errmsgPre errmsgPost
    case "$cmd" in
        ro)
            errmsgPre=">> Error while resetting the pull-URL of \""
            errmsgPost="\"."
            ;;

        reset)
            errmsgPre=">> Error while making \""
            errmsgPost="\" non-pullable."
            ;;

        *)
            echo "!!! Internal Error in 'git_subtree'!!!"
            echo "!!! Aborting !!!"
            return 11
            ;;
    esac

    local cantContinue=">> Cannot continue."
    local remote hasErr
    for remote in "$@"; do
        if utl_isaRemoteBranch $remote; then
            :
        else
            echo ">> This git-repo contains no remote called \"$remote\""
            echo ">> Ignoring..."
            continue
        fi

        # For cmd=='reset', the following is sufficient.
        git remote set-url --delete --push "$remote" '..*' || hasErr=y
        if [ -n "$hasErr" ]; then
            echo "$errmsgPre$remote$errmsgPost"
            echo "$cantContinue"
            return 1
        fi

        # For cmd=='ro', we'll modify the now lone-pull-source to point to
        # /dev/null:
        if [ "$cmd" = "ro" ]; then
            git remote set-url --push "$remote" /-push-not-permitted- \
                || hasErr=y
            if [ -n "$hasErr" ]; then
                echo "$errmsgPre$remote$errmsgPost  [Final step failed.]"
                echo "$cantContinue"
                return 1
            fi
        fi
    done
}


utl_git_subtree_viaMerge_add() {
    local srcRepos="${1%/}"
    shift
    local destRepos="${1%/}"
    shift
    local subtreeDir="${1%/}"
    shift

    local srOpt=${calledAsSubroutine:+--srcRepos}
    if utl_notaGitRepo "$srcRepos" $srOpt; then
        return 127
    fi

    local drOpt=${calledAsSubroutine:+--destRepos}
    if utl_notaGitRepo "$destRepos" $drOpt; then
        return 127
    fi

    #
    # The actual code
    #

    local srcModule="${srcRepos##*/}"
    if [ -z "$subtreeDir" ]; then
        subtreeDir="$srcModule"
    else
        # Make sure $subtreeDir is a relative path.
        case "$subtreeDir" in
            [/~]*|../*|*/../*)
                echo "Invalid subtree dir:  \"$subtreeDir\"."
                echo "\"<subtreeDir>\" must be a relative path and cannot"
                echo "contain any '..' upreferences."
                echo ""
                return 127
                ;;
        esac
    fi

    local remoteBranch="subtree._$srcModule"

    # Make sure $srcRepos an absolute-path before changing to the $destRepos.
    case "$srcRepos" in
        [^/~]*)
            srcRepos="$PWD/$srcRepos"
            ;;
    esac
    pushd $destRepos >/dev/null 2>&1

    local hasErrs
    local cowardErrmsg=">>\n>> Cowardly refusing to continue."

    git remote add -f $remoteBranch $srcRepos || hasErrs=y
    if [ -n "$hasErrs" ]; then
        echo ">>"
        echo ">> Failed to add \"$srcRepos\" as a remote-branch!"
        echo -e "$cowardErrmsg"
        return 3
    fi

    git merge -s ours --no-commit $remoteBranch/master || hasErrs=y
    if [ -n "$hasErrs" ]; then
        echo ">>"
        echo -n ">> Failed to prepare for the merge/read-tree of the "
        echo "remote-branch"
        echo ">> \"$remoteBranch/master\"."
        echo -e "$cowardErrmsg"
        return 4
    fi

    git read-tree --prefix=$subtreeDir/ -u $remoteBranch/master || hasErrs=y
    if [ -n "$hasErrs" ]; then
        echo ">>"
        echo ">> Failed to read the remote-branch \"$remoteBranch/master\""
        echo ">> into the subdirectory, \"$subtreeDir\"."
        echo -e "$cowardErrmsg"
        return 5
    fi

    local commitMesg="Merged \"$srcRepos\" as a new subtree"
    commitMesg="$commitMesg in the \"$subtreeDir\" subdirectory."
    commitMesg="$commitMesg  [Subtree source is \"$remoteBranch\".]"
    git commit -m "$commitMesg" || hasErrs=y
    if [ -n "$hasErrs" ]; then
        echo ">>"
        echo ">> Could not commit the new subtree in \"$subtreeDir\"!"
        echo -e "$cowardErrmsg"
        return 6
    fi

    utl_git_subtree_saveState "$remoteBranch" || hasErrs=y

    popd >/dev/null 2>&1
    [ -n "$hasErrs" ] && return 1
    #else
    return 0
}


utl_git_subtree_saveState() {
    local remoteBranch="$1"
    shift

    # FIXME:  This whole thing needs better error handling.

    # Error Handling
    if utl_notaGitRepo "."; then
        return 127
    fi
    if [ -z "$remoteBranch" ]; then
        return 127
    fi

    # If there's no subtree state at all, set up the common files.
    if [[ ! -d .subtrees ]]; then
        mkdir -p .subtrees || return $?
        git add .subtrees
    fi

    pushd .subtrees >/dev/null 2>&1

    # Store configuration info about our subtrees.  Do it every time so that
    # we capture any changes.
    git config --get-regexp 'subtree\._' | \
        perl -p -e 's/^([^\s]+)\s(.+)$/\x27$1\x27 \x27$2\x27/;' \
        >config || return $?

    # Stash the refs for this remote.
    local refsDir="refs/remotes/$remoteBranch"
    if [[ ! -d $refsDir ]]; then
        mkdir -vp $refsDir || return $?
    fi

    local fatalErrA="!!! FATAL ERROR!  Not a file:  "
    local fatalErrB="!!! This should never occur.  "
    fatalErr="${fatalErrB}There's a bug in the tool.\n"
    fatalErr="${fatalErrB}!!!"

    local f bf

    pushd $refsDir >/dev/null 2>&1
    for f in ../../../../.git/$refsDir/*; do
        if [[ ! -e $f ]]; then
            echo "${fatalErrA}\"$f\""
            echo -e "$fatalErrB"
            return 11
        fi

        bf=${f##*/}
        [[ -e $bf ]] || ln -v $f .
    done
    popd >/dev/null 2>&1

    # Now stash the logs/refs for this remote.
    if [[ ! -d logs/$refsDir ]]; then
        mkdir -vp logs/$refsDir || return $?
    fi
    pushd logs/$refsDir >/dev/null 2>&1
    for f in ../../../../../.git/logs/$refsDir/*; do
        if [[ ! -e $f ]]; then
            echo "${fatalErrA}\"$f\""
            echo -e "$fatalErrB"
        fi

        bf=${f##*/}
        [[ -e $bf ]] || ln -v $f .
    done
    popd >/dev/null 2>&1

    # pop out of '.subtrees'
    popd >/dev/null 2>&1

    git add --all .subtrees && \
        git commit -m "Saving subtree  for \"$remoteBranch\"" .subtrees
    # FIXME:  Add Error Handling
}


utl_git_subtree_restoreState() {
    # FIXME:  This is just a bare-prototype.

    perl -p -e 's/^/git config --add/;' .subtrees/config \
        >restore-subtrees-config.sh

    . restore-subtrees-config.sh


}


git_subtree_viaMerge() {
    local cmd="$1"
    shift

    local v showUsage_retval showFullUsage
    case "$cmd" in
        sync)
            utl_git_subtree_viaMerge_sync "$@"
            local retval=$?
            if [ $retval -eq 127 ]; then
                showUsage_retval=1
            else
                return $retval
            fi
            ;;

        readonly|read[-_]only)
            utl_git_subtree_modifyRemotePullURLs ro "$@"
            local retval=$?
            if [ $retval -eq 127 ]; then
                showUsage_retval=1
            else
                return $retval
            fi
            ;;

        reset[-_]push)
            utl_git_subtree_modifyRemotePullURLs reset "$@"
            local retval=$?
            if [ $retval -eq 127 ]; then
                showUsage_retval=1
            else
                return $retval
            fi
            ;;

        add)
            if [ -z "$*" ]; then
                echo "Nothing to add!"
                echo ""
                showUsage_retval=1
            fi
            ;;

        -h|--help|help)
            showUsage_retval=0
            showFullUsage=y
            ;;

        *)
            echo "Unknown command:  \"$cmd\""
            echo ""
            showUsage_retval=9
            ;;
    esac

    if [ -n "$showUsage_retval" ]; then
        echo "usage: git_subtree_viaMerge {-h|--help}"
        echo ""

        echo -n "       git_subtree_viaMerge add  {-s|--srcRepos} "
        echo "<gitReposDir> \\"
        echo -n "            {-d|--destRepos} <gitReposDir> "
        echo "[--subtreeDir <name>] \\"
        echo "            [{--srcRepos|--subtreeDir} ...]"
        echo ""

        echo -n "       git_subtree_viaMerge sync [-b <targBranch>] "
        echo "<sourceName> \\"
        echo "            [<sourceName> ...]"
        echo ""

        echo -n "       git_subtree_viaMerge readonly <subtree_remoteName>"
        echo " [<subtree_remoteName> ...]"
        echo -n "       git_subtree reset-push <subtree_remoteName>"
        echo " [<subtree_remoteName> ...]"
        echo ""

        if [ -z "$showFullUsage" ]; then
            # Stop now

            echo "Use the \"--help\" option to see the full usage message."
            return $showUsage_retval
        fi
        # else:
        # Show the full usage message.

        echo "There are 4 supported subcommands to 'git_subtree'."
        echo ""
        echo ""

        echo "The 'sync' subcommand must be run from *inside* of a"
        echo "'git' repos directory that you previously used"
        echo "'git_subtree add' on."
        echo ""
        echo "  \"-b <targBranch>\""
        echo "        By default, each source is pulled onto the "
        echo "        \"master\" branch. This option lets you specify a"
        echo "        different branch to merge onto."
        echo "        You MUST pass this option *first*, or it "
        echo "        won't work correctly."
        echo ""
        echo "  \"<sourceName>\""
        echo "        One or more git-\"tree-ish\"es to perform"
        echo "        the subtree-pull on."
        echo ""
        echo ""

        echo "The 'add' subcommand connects one or more remote 'git'-repos to"
        echo "subtree(s) in a target 'git'-repo.  [See"
        echo "http://help.github.com/articles/working-with-subtree-merge"
        echo "for the detailed steps.]"
        echo "The options can be passed to this subcommand in any order, but"
        echo "there are some restrictions.  [E.g.:  until you pass the"
        echo "\"--destRepos\", nothing will happen.]"
        echo ""

        echo "  \"--destRepos <dest_gitReposDir>\""
        echo "        This option specifies the target 'git'-repo."
        echo "        \"<dest_gitReposDir>\" must be a directory containing"
        echo "        a 'git' repository."
        echo ""
        echo "        You must specify this option *once*.  Repeat instances"
        echo "        will be ignored."
        echo "        *Until* you specify this option, nothing will happen."
        echo "        If you don't pass this parameter first, then as soon"
        echo "        as this function sees the \"--destRepos\", it uses"
        echo "        either the next \"--srcRepos\" and/or \"--subtreeDir\""
        echo "        seen [whichever wasn't passed earlier], or the"
        echo "        *most-receont* one(s)."
        echo ""

        echo "  \"--srcRepos <src_gitReposDir>\""
        echo "        This option specifies each remote 'git'-repo to add"
        echo "        to the \"--destRepos\" as a subtree."
        echo ""
        echo "        You can specify this option more than once ... but all"
        echo "        but the first must follow the \"--destRepos\" option"
        echo "        on the commandline."
        echo ""

        echo "  \"--subtreeDir <subdirName>\""
        echo "        This is an optional parameter.  It lets you specify the"
        echo "        \"<dest_gitReposDir>\" subdirectory that a source"
        echo "        git-repo is pulled into."
        echo "        \"<subdirName>\" cannot be ''."
        echo ""
        echo "        You can specify one of these for each \"--srcRepos\" and"
        echo "        can come either before or after it on the commandline"
        echo "        [though you probably want to specify the \"--srcRepos\""
        echo "        then its \"--subtreeDir\", as this reads better]."
        echo "        Each \"--subtreeDir\" on the commandline is used only"
        echo "        *once*.  After each subtree-add, the previous"
        echo "        \"-subtreeDir\" is cleared back to the default."
        echo ""
        echo "        The default destination subdirectory is the "
        echo "        base-pathname of the \"<src_gitReposDir>\"."
        echo ""

        echo -n "The first group of [\"--destRepos\", \"--srcRepos\", "
        echo "\"--subtreeDir\"]"
        echo "options fires off the subtree-add.  The order doesn't matter,"
        echo "and the \"--subtreeDir\" can be omitted.  Each subsequent "
        echo "[\"--srcRepos\", \"--subtreeDir\"] pair or lone \"--srcRepos\""
        echo "forms a subtree-add.  Again, you can swap the order of the"
        echo "[\"--srcRepos\", \"--subtreeDir\"] pair.  The clustering is"
        echo "what matters."
        echo ""
        echo ""

        echo "The subcommands 'readonly' and 'reset-push' each modify the"
        echo "target-URL used by a 'git push <subtree_remoteName>'."
        echo ""
        echo "If you want to only pull changes *into* a subtree *from* its"
        echo "source, use the 'readonly' subcommand.  It will erase all of"
        echo "the pull-URLs of '<subtree_remoteName>', setting the lone"
        echo "pull-URL to a bogus local path.  [Note:  You cannot completely"
        echo "remove all of the push-URLs from a remote.  There must be at"
        echo "least one.]"
        echo ""
        echo "To re-enable 'git push' for a '<subtree_remoteName>', use the"
        echo "'reset-push' subcommand.  It restores the default push-URL"
        echo "[which is the same as the pull-URL]."
        echo ""
        echo "Remember:  'readonly' erases ALL of your push-URLs.  If you had"
        echo "any custom push-URLs, you'll lose them.  'reset-push' can't"
        echo "restore them."
        echo ""
        echo "You can specify more than one '<subtree_remoteName>' to these"
        echo "two subcommands to perform them in-bulk."

        echo ""
        echo ""

        return $showUsage_retval
    fi

    #
    # We only reach here if we're doing an 'add'
    #

    local srcRepos destRepos subtreeDir
    local srcDestReady ready2add
    while [ -n "$1" ]; do
        if [ -n "$srcRepos" -a -n "$destRepos" ]; then
            srcDestReady=y
        else
            srcDestReady=''
        fi

        case "$1" in
            -d|--dest|--destRepos|--dest[-_]repos)
                shift
                if [ -z "$destRepos" ]; then
                    destRepos="$1"
                else
                    echo -n ">> Cannot override \"--destRepos\".  Ignoring "
                    echo "(attempted) new value:"
                    echo ">> \"$1\""
                fi
                shift
                ;;

            -s|--src|--srcRepos|--src[-_]repos)
                if [ -n "$srcDestReady" ]; then
                    # There's a new value of '--srcRepos' at the ready.
                    # Ignore it for now and trigger a
                    # 'utl_git_subtree_viaMerge_add' with the args we have
                    # now.
                    ready2add=y
                else
                    shift
                    if [ -n "$srcRepos" ]; then
                        echo -n ">> No \"--destRepos\" specified yet.  "
                        echo "Tossing old \"--srcRepos\":"
                        echo ">> \"$srcRepos\""
                    fi

                    srcRepos="$1"
                    shift
                fi
                ;;

            --subtreeDir|--subtree[-_]dir)
                if [ -z "$subtreeDir" -o -z "$srcDestReady" ]; then
                    shift
                    if [ -z "$1" ]; then
                        echo "Option \"--subtreeDir\" requires a value."
                        echo ""
                        git_subtree --help
                        return 1
                    else
                        subtreeDir="$1"
                    fi
                    shift
                fi

                if [ -n "$srcDestReady" ]; then
                    # If we have a source and destination repository, an
                    # explicit '--subtreeDir' triggers a
                    # 'utl_git_subtree_viaMerge_add' using that '--subtreeDir'
                    # arg.
                    ready2add=y
                fi
                ;;

            -*)
                echo "Unknown option:  \"$1\""
                echo ""
                git_subtree --help
                return 1
                ;;

            *)
                echo "Mystery Bare Argument:  \"$1\""
                echo "[Missing something?]"
                echo ""
                git_subtree --help
                return 1
                ;;
        esac

        # Now that we've processed the next argument, we need to check if
        # we've consumed all of the args.  That, too, must trigger a
        # 'utl_git_subtree_viaMerge_add' using whatever parameters we have.
        if [ -z "$1" -a -z "$ready2add" ]; then
            local missing dangling

            if [ -n "$srcRepos" -a -n "$destRepos" ]; then
                ready2add=y
            else
                if [ -z "$srcRepos" ]; then
                    missing="$missing \"--srcRepos\""
                else
                    dangling="$dangling\n>>\t--srcRepos=\"$srcRepos\""
                fi
                if [ -z "$destRepos" ]; then
                    missing="$missing \"--destRepos\""
                else
                    dangling="$dangling\n>>\t--destRepos=\"$destRepos\""
                fi
                if [ -n "$subtreeDir" ]; then
                    dangling="$dangling\n>>\t--subtreeDir=\"$subtreeDir\""
                fi

                echo -n ">> Error!  Ran out of args before we could do "
                echo "the [next]"
                echo -n ">> add.  Missing: $missing"
                echo -e ">> Leftover parameters:$dangling"

                git_subtree --help
                retval=3
            fi
        fi

        # Now perform the 'utl_git_subtree_viaMerge_add':
        if [ -n "$ready2add" ]; then
            utl_git_subtree_viaMerge_add \
                "$srcRepos" "$destRepos" "$subtreeDir"
            local retval=$?

            if [ $retval -ne 0 ]; then
                if [ $retval -eq 127 ]; then
                    git_subtree --help
                    retval=1
                else
                    echo ">> "
                    echo ">> Subtree add failed for:"
                    echo ">>     srcRepos=\"$srcRepos\""
                    echo ">>     destRepos=\"$destRepos\""
                    [ -n "$subtreeDir" ] && \
                        echo ">>     subtreeDir=\"$subtreeDir\""
                    echo ">> Cowardly refusing to continue."
                fi

                return $retval
            fi

            # Clear all of the control flags and transient values.
            ready2add=''
            srcRepos=''
            subtreeDir=''
        fi

        # N.B.:  No 'shift' done here.  That has to take place inside of the
        # 'case'-statements, since we'll sometimes skip option processing
        # altogether.
    done
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

    git_patchpull - Works around limitations of "git" that prevent you from
                    transferring files *and* history across multiple
                    repositories.
                    Creating a patch lets you perform this type of transfer.
                    The 'git am' command, which applies exported-patches,
                    lets you specify a target-directory for your applied
                    patch.
                    This tool will tag the source git-repository after
                    performing the 'git format-patch'.  It can also optionally
                    'git am' the patch into a branch in the target
                    git-repos.

    git_subtree - This is probably what you want to use instead of
                  'git_patchpull'.  It connects multiple external git
                  repositories to subtrees in a target git-repos.

                  You can use it to sync an existing subtree as well as add a
                  new one.  See the help-message for details.

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
