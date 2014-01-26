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


#
# General-Purpose Utilities
#


utl_get_gitBranch_r() {
    local regex="$1"
    shift

    if [ -z "$regex" ]; then
        git branch -r | awk '{ print $1 }'
        return
    fi
    # else

    git branch -r | awk '{ print $1 }' | grep "${regex}"
}


utl_isaRemoteBranch() {
    local verbose
    if [ "$1" = "-v" ]; then
        verbose="$1"
        shift
    fi
    local remoteId="$1"
    shift
    local branchName="$1"
    shift

    [ -z "$branchName" ] && branchName="master"


    local actualRemoteBranch="$(utl_get_gitBranch_r "^${remoteId}/")"

    local noRemoteErr=">> No such remote:"

    if [ "$branchName" = "--" ]; then
        # Special case:  Just check if there was a match to the $remoteId.
        [ -n "$actualRemoteBranch" ] && return 0
        # else:

        if [ -n "$verbose" ]; then
            echo "$noRemoteErr  \"$remoteId\""
        fi
        return 2
    fi
    # else:
    # Check for an exact match.

    [ "$actualRemoteBranch" = "$remoteId/$branchName" ] && return 0
    # else:

    if [ -n "$verbose" ]; then
        if [ -n "$actualRemoteBranch" ]; then
            echo ">> Remote \"$remoteId\" isn't associated with any"
            echo ">> branch named \"$branchName\"."
        else
            echo "$noRemoteErr  \"$remoteId\""
        fi
    fi
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
        echo ">> Error:  not a git-repository:  \"$path\""
        if [ -n "$optName" ]; then
            echo ">> \"$optName\" requires a git-repo dir."
            echo ">>"
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


#
# "Standalone" Functions
# [i.e. not composed of multiple functions; those have their own sections.]
#


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


#
# 'git_patchpull'
# [It's a large function]
#


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


#
# 'git_subtree_*' Functions
#


utl_gitSubtree_verifyPrefix() {
    local prefix="$1"
    shift

    if [[ ! -d $prefix ]]; then
        echo -n ">> Prefix doesn't match any existing subdirectory:  "
        echo "\"$prefix\""
        return 2
    fi

    return 0
}


utl_gitSubtree_prefix2remote() {
    local prefix="$1"
    shift

    local remote="subtree._${prefix//\/._}"
    if [ "$1" = "--verify" ]; then
        utl_gitSubtree_verifyPrefix "$prefix" 1>&2 \
            || return 2
        # else

        if utl_isaRemoteBranch "$remote" --; then
            :
        else
            echo -n ">> Prefix doesn't match any known " 1>&2
            echo "subtree remote:  \"$prefix\"" 1>&2
            echo ">> Cannot convert to a valid remote." 1>&2
            return 3
        fi
        # else
    fi

    echo "$remote"
}


utl_gitSubtree_remote2prefix() {
    local remote="$1"
    shift

    local pseudoPrefix="${remote#subtree._}"
    local prefix="${pseudoPrefix//._/\/}"
    if [ "$1" = "--verify" ]; then
        if utl_isaRemoteBranch "$remote" --; then
            :
        else
            echo ">> No such remote:  \"$remote\"" 1>&2
            echo ">> Cannot convert to a valid prefix." 1>&2
            return 3
        fi
        # else

        utl_gitSubtree_verifyPrefix "$prefix"
        if [ $? -ne 0 ]; then
            echo ">> Remote \"$remote\" doesn't convert to a valid prefix." \
                1>&2
            return 2
        fi
        # else
    fi

    echo "$prefix"
}


utl_gitSubtree_saveState() {
    local prefix="$1"
    shift
    local addCmdArgs="$1"
    shift

    # Error Handling
    if utl_notaGitRepo "."; then
        echo ">> Error:  This command must be run from inside of a git repo."
        return 127
    fi
    if [ -z "$prefix" ]; then
        echo ">> Internal Error:  No subtree prefix specified."
        return 11
    fi

    # If there's no subtree state at all, set up the common files.
    if [[ ! -d .subtrees ]]; then
        mkdir -p .subtrees || return $?
        git add .subtrees
    fi

    local gitStat

    # Store configuration info about our subtrees.  Do it every time so that
    # we capture any changes.
    git config --get-regexp 'subtree\._' | \
        perl -p -e 's/^([^\s]+)\s(.+)$/git config \x27$1\x27 \x27$2\x27/;' \
        >.subtrees/remotes
    gitStat=$?
    if [ $gitStat -ne 0 ]; then
        echo ">>"
        echo ">> Failed to save subtree-remotes."
        return $gitStat
    fi

    if [ -n "$addCmdArgs" ]; then
        local addCmdFile=.subtrees/${prefix}-addCmd
        echo "# Subtree \"$prefix\" added with:" >${addCmdFile}
        echo "    git subtree add --prefix=$prefix $addCmdArgs" \
            >>${addCmdFile}
    fi

    git add --all .subtrees
    gitStat=$?
    if [ $gitStat -eq 0 ]; then
        git commit -m "Saving subtree info for \"$prefix\"" .subtrees
        gitStat=$?
    fi
    if [ $gitStat -ne 0 ]; then
        echo ">>"
        echo ">> Failed to commit subtree information."
        return $gitStat
    fi

    return 0
}


git_subtree_restoreState() {
    local noPull showUsage_retval
    case "$1" in
        --no[-_][pP]ull)
            noPull=y
            ;;
        --help|-h)
            showUsage_retval=0
            ;;
        *)
            if [ -n "$1" ]; then
                echo "Unknown option:  \"$1\""
                showUsage_retval=1
            fi
            ;;
    esac
    shift
    if [ -n "$*" ]; then
        echo "Extra args: $@"
        showUsage_retval=1
    fi

    #
    # Usage:
    #

    if [ -n "$showUsage_retval" ]; then
        [ $showUsage_retval -ne 0 ] && echo ""

        echo "usage:  git_subtree_restoreState [--no-pull]"
        echo ""
        echo "The \"--no-pull\" option disables the 'git subtree pull' stage."
        echo "Normally, 'git_subtree_restoreState' does a"
        echo "'git_subtree_sync --all' after restoring all of the subtree"
        echo "remotes."

        return $showUsage_retval
    fi

    #
    # The actual steps
    #

    . .subtrees/remotes
    [ -n "$noPull" ] && return 0

    git_subtree_sync --all
}


git_subtree_list_remotes() {
    if [ "$1" != "--" ]; then
        echo "# Run 'git branch -r' for the full list of remote branches."
        echo "#"
    fi

    local r
    for r in $(utl_get_gitBranch_r '^subtree\._'); do
        echo ${r%/*}
    done
}


git_subtree_modifyRemotes() {
    local cmd="$1"
    shift

    #
    # Option Processing
    #

    local errmsgPre errmsgPost showUsage_retval
    case "$cmd" in
        ro|readonly|read[-_]only)
            errmsgPre=">> Error while resetting the pull-URL of \""
            errmsgPost="\"."
            # Force this:
            cmd=ro
            ;;

        reset|reset[-_]push)
            errmsgPre=">> Error while making \""
            errmsgPost="\" non-pullable."
            ;;

        help|--help|-h)
            showUsage_retval=0
            ;;

        *)
            if [ -z "$cmd" ]; then
                echo "No command specified!"
            else
                echo "Unknown/Unsupported Command:  \"$cmd\""
            fi
            showUsage_retval=11
            ;;
    esac

    local isPrefix=y
    case "$1" in
        -r)
            shift
            isPrefix=''
            ;;
        --help|-h)
            showUsage_retval=0
            ;;
    esac
    if [ -z "$1" ]; then
        echo "At least one \"<prefixOrRemote>\" must be specified."
        showUsage_retval=1
    fi

    #
    # Usage:
    #

    if [ -n "$showUsage_retval" ]; then
        [ $showUsage_retval -ne 0 ] && echo ""

        local myName="${UTL_FN_USG_NAME:-git_subtree_modifyRemotes <cmd>}"
        echo "usage: ${myName} [-r] <prefixOrRemote> \\"
        echo "              [<prefixOrRemote>...]"
        echo ""

        if [ -z "$UTL_FN_USG_NAME" ]; then
            echo "<cmd> can be:"
            echo "    readonly"
            echo "    read-only"
            echo "    reset-push"
            echo "    help"
            echo ""
        fi

        if [ $showUsage_retval -ne 0 ]; then
            echo "Rerun with \"--help\" for the full documentation."
            return 0
        fi

        echo "-r"
        echo "    Normally, the \"<prefixOrRemote>\" arg(s) are the prefixes"
        echo "    you specified to 'git subtree add -P <prefix>'."
        echo "    This option changes how the \"<prefixOrRemote>\" arg(s) are"
        echo "    handled.  When present, the \"<prefixOrRemote>\" become"
        echo "    the names of the remote sources [as seen in a"
        echo "    'git branch -r']."
        echo ""
        echo "    This option must appear on the commandline in the position"
        echo "    shown.  You typically won't need it."
        echo ""

        echo "Modifies the target-URL used by a 'git push <remoteName>' or"
        echo "'git subtree push -P <prefix>', making \"<remoteName\" [or"
        echo "\"<prefix>\"] effectively read-only."
        echo ""
        echo "If you want to only pull changes *into* a subtree *from* its"
        echo "source, use the 'readonly' command [or the relevant alias]."
        echo "It will erase all ofthe pull-URLs of \"<prefixOrRemote>\","
        echo "setting the lonepull-URL to a bogus local path.  [Note:  You"
        echo "cannot completely remove all of the push-URLs from a remote."
        echo "There must be at least one.]"
        echo ""
        echo "To re-enable 'git push <remoteName>' [and "
        echo "'git subtree push -P <prefix>'] use the 'reset-push' command"
        echo "[or the relevant alias].  It restores the default push-URL"
        echo "[which is the same as the pull-URL]."
        echo ""
        echo "Remember:  'readonly' erases ALL of your push-URLs.  If you had"
        echo "any custom push-URLs, you'll lose them.  'reset-push' can't"
        echo "restore them."
        echo ""
        echo "You can specify more than one \"<prefixOrRemote>\" to these"
        echo "two subcommands to perform them in-bulk."
        echo ""

        return $showUsage_retval
    fi

    #
    # The Code-Proper
    #

    local cantContinue=">> Cannot continue."
    local prefix remote hasErr
    for prefix in "$@"; do
        if [ -n "$isPrefix" ]; then
            remote="$(utl_gitSubtree_prefix2remote $prefix --verify)" 2>&1

        else
            remote="$prefix"

            if utl_isaRemoteBranch "$remote" --; then
                # Validate the prefix:
                prefix="$(utl_gitSubtree_remote2prefix $prefix)" \
                    2>&1

                if [[ ! -d $prefix ]]; then
                    echo -n ">> The remote \"$remote\" doesn't match any "
                    echo "subtree in this git-repo."
                    remote=''
                fi
            else
                echo ">> This git-repo contains no remote called \"$remote\""
                remote=''
            fi
        fi

        if [ -z "$remote" ]; then
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
g_s_mR='UTL_FN_USG_NAME=git_subtree_readonly git_subtree_modifyRemotes'
alias git_subtree_readonly="$g_s_mR readonly"
g_s_mR='UTL_FN_USG_NAME=git_subtree_reset_push git_subtree_modifyRemotes'
alias git_subtree_reset_push="$g_s_mR reset-push"
unset g_s_mR


git_subtree_sync() {
    #
    # Arg processing
    #

    local targBranch="HEAD"
    local remote prefix showUsage_retval optname prbList
    while [ -n "$1" -a -z "$showUsage_retval" ]; do
        case "$1" in
            -b|--branch)
                shift
                targBranch="$1"
                if [ -z "$targBranch" ]; then
                    echo "Option \"-b\" requires the name of a target branch."
                    showUsage_retval=1
                fi
                ;;

            -r|--remote)
                shift
                remote="$1"
                if [ -z "$remote" ]; then
                    echo "Option \"-r\" requires a value."
                    showUsage_retval=1
                else
                    prefix="$(utl_gitSubtree_remote2prefix $remote)"
                fi
                ;;

            --all)
                prbList=''
                for remote in $(git_subtree_list_remotes --); do
                    prefix="$(utl_gitSubtree_remote2prefix $remote --verify)" \
                        2>&1

                    if [ -z "$prefix" ]; then
                        echo ">>"
                        echo ">> Ignoring..."
                        continue
                    fi

                    prbList="${prbList}${prbList:+|}${prefix}|${remote}|HEAD"
                done

                break
                ;;

            -h|--help)
                showUsage_retval=0
                ;;

            -P|--prefix|[^-]*)
                # Get the prefix, saving how it was passed.
                # We'll use the latter info for constructing error mesgs.
                optname="$1"
                [ "$optname" = '--prefix' ] && optname='-P'
                if [ "$optname" = '-P' ]; then
                    shift
                    prefix="$1"
                else
                    prefix="$optname"
                    optname=''
                fi

                # Process
                if [ -z "$prefix" ]; then
                    if [ -n "$optname" ]; then
                        echo "Option \"$optname\" requires a value."
                    else
                        echo "Cannot pass \"\" as a prefix."
                    fi
                    showUsage_retval=1
                else
                    remote="$(utl_gitSubtree_prefix2remote $prefix)"
                fi
                ;;

            -*)
                echo "Unknown option:  \"$1\""
                showUsage_retval=1
                ;;
        esac
        shift

        if [ -n "$remote" -a -n "$prefix" ]; then
            prbList="${prbList}${prbList:+|}${prefix}|${remote}|${targBranch}"

            remote=''
            prefix=''
        fi
    done

    #
    # Usage:
    #

    if [ -n "$showUsage_retval" ]; then
        [ $showUsage_retval -ne 0 ] && echo ""

        echo "usage: git_subtree_sync --all"
        echo -n "       git_subtree_sync [--branch <targBranch>] "
        echo "{subtreeSpec} \\"
        echo "           [{subtreeSpec}|--branch <targBranch>} ...]"
        echo ""

        echo "Options and their short-forms:"
        echo "    --remote -r"
        echo "    --prefix -P"
        echo "    --branch -b"
        echo ""

        echo "{subtreeSpec} is one of the following:"
        echo "    --remote <remote>"
        echo "    --prefix <prefix>"
        echo "    <prefix>"

        if [ $showUsage_retval -ne 0 ]; then
            echo ""
            echo "Rerun with \"--help\" for the full documentation."
            return 0
        fi

        echo "Note that the \"--prefix\" [or \"-P\"] flag is optional."
        echo "You can specify either the subtree prefix, or the remote ID"
        echo "that the subtree is attached to.  'git_subtree_sync' will"
        echo "use whichever you specify to look up the other."
        echo ""

        echo "Normally, you will sync from the 'HEAD' branch of each"
        echo "subtree's source repo.  If, however, you need to sync against"
        echo "a different branch, pass a \"--branch\" option on the"
        echo "commandline.  HOWEVER:  note that the \"<targBranch>\" you"
        echo "specify remains active FOR EVERY {subtreeSpec} THEREAFTER!"
        echo ""

        echo "The special option \"--all\" syncs all of the subtrees of the"
        echo "current git repo from the 'HEAD' branch of their sources.  The"
        echo "subtrees are gleaned from a 'git branch -r'."

        return $showUsage_retval
    fi

    #
    # The Code-Proper
    #

    # Check that we're in a git-repo ... but only after we've handled the
    # '--help' option.
    if utl_notaGitRepo "$PWD"; then
        echo ">> This command must be run from inside of a git-repo."
        echo ">> "
        return 1
    fi

    # Load our triplets into the arglist.
    local oIFS="$IFS"
    IFS='|'
    set -- $prbList
    IFS="$oIFS"

    if [ $# -lt 3 ]; then
        echo ">> At least one subtree must be specified."
        echo ">>"

        git_subtree_sync --help
        return 1
    fi


    local cowardErrmsg=">> Cowardly refusing to continue."
    local hasErrs
    while [ $# -ge 3 ]; do
        prefix="$1"
        shift
        remote="$1"
        shift
        targBranch="$1"
        shift

        # Validate.  Abort if there's any problems.

        utl_gitSubtree_verifyPrefix "$prefix"
        if [ $? -ne 0 ]; then
            echo ">>"
            echo "$cowardErrmsg"
            return 1
        fi

        if [ "$targBranch" = "HEAD" ]; then
            utl_isaRemoteBranch -v $remote -- || hasErrs=y
        else
            utl_isaRemoteBranch -v $remote $targBranch || hasErrs=y
        fi

        if [ -n "$hasErrs" ]; then
            echo ">>"
            echo "$cowardErrmsg"
            return 1
        fi

        # Perform the sync steps.

        echo ">> Syncing subtree \"$prefix\" from \"$remote/$targBranch\"."
        git subtree pull -P "$prefix" "$remote" "$targBranch" || hasErrs=y
        if [ -n "$hasErrs" ]; then
            echo ">>"
            echo ">> Pull failed.  Cannot continue."
            return 1
        fi

        git add --all .subtrees || hasErrs=y
        if [ -n "$hasErrs" ]; then
            echo ">>"
            echo "$cowardErrmsg"
            return 1
        fi

        git commit -m "Updating subtree info for \"$remote\"" .subtrees \
            || hasErrs=y
        if [ -n "$hasErrs" ]; then
            echo ">>"
            echo ">> Commit failed.  Any remaining syncs won't be performed."
            return 1
        fi
        # else

        echo ">>"
        echo ">> Subtree \"$prefix\" successfully synced."
        echo ">>"
    done

    return $gitStat
}


utl_gitSubtree_single_add() {
    local prefix="${1%/}"
    shift
    local srcRepos="${1%/}"
    shift

    if utl_notaGitRepo "$srcRepos" --srcRepos; then
        return 127
    fi

    if [[ -d $prefix ]]; then
        echo ">> Invalid prefix: \"$prefix\""
        echo ">> Directory already exists!  Cannot re-add a subtree!"
        echo ">>"
        return 127
    fi

    # Make sure $subtreeDir is a relative path.
    case "$prefix" in
        [/~]*|../*|*/../*|./*|*/./*)
            echo ">> Invalid prefix:  \"$prefix\"."
            echo ">> Cannot be an absolute path or contain any '.' or '..'"
            echo ">> components."
            echo ">>"
            return 127
            ;;
    esac

    # Convert the $srcRepos to an absolute path.
    case "$srcRepos" in
        [/~]*|../*|*/../*|./*|*/./*)
            pushd $srcRepos >/dev/null 2>&1
            srcRepos="$PWD"
            popd >/dev/null 2>&1
            ;;
    esac

    # Add it:

    local remoteId="$(utl_gitSubtree_prefix2remote $prefix)" 2>&1
    local hasErrs
    local cowardErrmsg=">>\n>> Cowardly refusing to continue."

    git remote add -f $remoteId $srcRepos || hasErrs=y
    echo ">>"
    if [ -n "$hasErrs" ]; then
        echo ">> Failed to add \"$srcRepos\" as a remote-branch!"
        echo -e "$cowardErrmsg"
        return 3
    else
        echo ">> Added new remote:  \"$remoteId\""
        echo ">> Use this as the remote-id for the \"prefix\" subtree's"
        echo ">> source git-repo."
        echo ">>"
    fi

    git subtree add --prefix="$prefix" "$remoteId" master || hasErrs=y
    if [ -n "$hasErrs" ]; then
        echo ">>"
        echo ">> Failed to add the new subtree \"$prefix\""
        echo ">> from the remote repository \"$remoteId\"."
        return 4
    fi

    utl_gitSubtree_saveState "$prefix" "\"$remoteId\" master" || hasErrs=y

    [ -n "$hasErrs" ] && return 1
    #else

    return 0
}


git_subtree_add() {
    local showUsage_retval
    local destRepos prefix srcRepos psRList
    while [ -n "$1" -a -z "$showUsage_retval" ]; do
        case "$1" in
            -d|--destRepos|--dest[-_]repos)
                shift
                if [ -z "$destRepos" ]; then
                    destRepos="$1"
                else
                    echo -n ">> Cannot override \"--destRepos\".  Ignoring "
                    echo "(attempted) new value:"
                    echo ">> \"$1\""
                    echo ""
                fi

                # Check the new value.
                if [ -z "$destRepos" ]; then
                    echo "Option \"--destRepos\" requires a value."
                    showUsage_retval=4
                fi
                ;;

            -s|--srcRepos|--src[-_]repos)
                shift
                if [ -n "$srcRepos" ]; then
                    echo "Error:  back-to-back  \"--srcRepos\" options."
                    showUsage_retval=3
                fi

                srcRepos="$1"
                # Check the new value.
                if [ -z "$srcRepos" ]; then
                    echo "Option \"--srcRepos\" requires a value."
                    showUsage_retval=4
                fi
                ;;

            -[pP]|--prefix)
                shift
                if [ -n "$prefix" ]; then
                    echo "Error:  back-to-back  \"--prefix\" options."
                    showUsage_retval=3
                fi

                prefix="$1"
                # Check the new value.
                if [ -z "$prefix" ]; then
                    echo "Option \"--prefix\" requires a value."
                    showUsage_retval=4
                fi
                ;;

            -h|--help)
                showUsage_retval=0
                ;;

            -*)
                echo "Unknown option:  \"$1\""
                showUsage_retval=1
                ;;

            *)
                echo "Mystery Bare Argument:  \"$1\""
                echo "[Missing something?]"
                showUsage_retval=1
                ;;
        esac
        shift

        if [ -n "$prefix" -a -n "$srcRepos" ]; then
            psRList="${psRList}${psRList:+|}${prefix}|${srcRepos}"

            remote=''
            srcRepos=''
        fi
    done

    # Argument Validation:
    # [Only check this if we're not printing the usage already.]
    if [ -z "$showUsage_retval" ]; then
        if [ -z "$destRepos" ]; then
            echo "Missing \"--destRepos\" option!"
            showUsage_retval=1
        elif utl_notaGitRepo "$destRepos" --destRepos; then
            showUsage_retval=2
        fi
    fi

    #
    # Usage:
    #

    if [ -n "$showUsage_retval" ]; then
        [ $showUsage_retval -ne 0 ] && echo ""

        echo "usage: git_subtree_add <Options>"

        echo "Add one or more subtree(s) to a target 'git'-repo."
        echo "You can pass the options in any order [within reason; see"
        echo "below]."
        echo ""

        echo "<Options>:"
        echo ""
        echo "  \"-d <dest_gitReposDir>\""
        echo "  \"--destRepos <dest_gitReposDir>\""
        echo "        This option specifies the target 'git'-repo."
        echo "        \"<dest_gitReposDir>\" must be a directory containing"
        echo "        a 'git' repository."
        echo "        If you're already in your target 'git'-repo, just pass"
        echo "        \".\" as the \"<dest_gitReposDir>\""
        echo ""
        echo "        You must specify this option *once*.  Repeat instances"
        echo "        will be ignored."
        echo ""

        echo "  \"-s <src_gitReposDir>\""
        echo "  \"--srcRepos <src_gitReposDir>\""
        echo "        This option specifies each remote 'git'-repo to add"
        echo "        to the \"--destRepos\" as a subtree."
        echo ""
        echo "        You must specify this together with a \"--prefix\", in"
        echo "        pairs.  The order within a pair doesn't matter."
        echo ""

        echo "  \"-P <prefix>\""
        echo "  \"-p <prefix>\""
        echo "  \"--prefix <prefix>\""
        echo "        This option specifies the \"prefix\" of the new"
        echo "        subtree.  to the \"--destRepos\" as a subtree."
        echo ""
        echo "        You must specify this together with a \"--srcRepos\","
        echo "        in pairs.  The order within a pair doesn't matter."
        echo ""

        echo "Be careful when specifying the \"--prefix\" and \"--srcRepos\""
        echo "that you pass them *in* *pairs*!  Accidently passing one of"
        echo "these \"back-to-back\" is an error."
        echo ""

        return $showUsage_retval
    fi

    #
    # The Code-Proper
    #

    # Load our triplets into the arglist.
    local oIFS="$IFS"
    IFS='|'
    set -- $psRList
    IFS="$oIFS"

    if [ $# -lt 2 ]; then
        echo "At least one (<prefix>, <srcRepo>) pair must be specified."
        echo ""

        git_subtree_add --help
        return 1
    fi

    [ "$destRepos" != "." ] && pushd $destRepos >/dev/null 2>&1

    local cowardErrmsg=">> Cowardly refusing to continue."
    local retval
    while [ $# -ge 2 ]; do
        prefix="$1"
        shift
        srcRepos="$1"
        shift

        utl_gitSubtree_single_add "$prefix" "$srcRepos"
        retval=$?

        if [ $retval -ne 0 ]; then
            if [ $retval -eq 127 ]; then
                git_subtree_add --help
                retval=1
            else
                echo ">> "
                echo ">> Subtree add failed for:"
                echo ">>     prefix=\"$prefix\""
                echo ">>     srcRepos=\"$srcRepos\""
                echo ">>     destRepos=\"$destRepos\""
                echo ">> "
                echo ">> Cowardly refusing to continue."
            fi

            return $retval
        fi
    done

    [ "$destRepos" != "." ] && popd >/dev/null 2>&1
    return 0
}


git_subtree() {
    local cmd="$1"
    shift

    local showUsage_retval cmd_retval
    case "$cmd" in
        sync)
            git_subtree_sync "$@"
            cmd_retval=$?
            ;;

        add)
            git_subtree_add "$@"
            cmd_retval=$?
            ;;

        readonly|read[-_]only)
            UTL_FN_USG_NAME="git_subtree_readonly" \
                git_subtree_modifyRemotes readonly "$@"
            cmd_retval=$?
            ;;

        reset[-_]push)
            UTL_FN_USG_NAME="git_subtree_reset_push" \
                git_subtree_modifyRemotes reset-push "$@"
            cmd_retval=$?
            ;;

        restore[-_][sS]tate)
            git_subtree_restoreState "$@"
            cmd_retval=$?
            ;;

        list|list[-_][rR]emotes)
            git_subtree_list_remotes
            cmd_retval=$?
            ;;

        -h|--help|help)
            showUsage_retval=0
            showFullUsage=y
            ;;

        *)
            if [ -z "$cmd" ]; then
                echo "No command specified!"
            else
                echo "Unknown command:  \"$cmd\""
            fi
            echo ""
            showUsage_retval=9
            ;;
    esac

    [ -z "$showUsage_retval" ] && return $cmd_retval
    # else:

    echo "usage: git_subtree {-h|--help}"
    echo "       git_subtree add {--help|<otherOpts>}"
    echo "       git_subtree sync {--help|<otherOpts>}"
    echo "       git_subtree readonly {--help|<otherOpts>}"
    echo "       git_subtree reset-push {--help|<otherOpts>}"
    echo "       git_subtree restore-state {--help|<otherOpts>}"
    echo "       git_subtree {list|list-remotes}"
    echo ""

    echo "This function is a simple wrapper around a group of related tools."
    echo "Run the appropriate command with the \"--help\" option for the"
    echo "usage statement of that command."

    echo "[The 'list-remotes' command has no usage message, as it is"
    echo " self-explanatory.]"

    return $showUsage_retval
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

    git_tester - Create a testing-repos in \$GIT_TESTER_DIR and "pushd" into
                 it.
                 [Skips any of the repo-creation steps it doesn't need.]

    git_unique_tag - Silently do nothing if adding a non-unique tag name.

                     The 'git tag' command already requires you to use a
                     unique name when creating a new tag.  It will fail
                     with an error message otherwise.  This helper-function
                     does the opposite.

    git_subtree_modifyRemotes - Lets you make a remote associated with a
                                subtree read-only, or "pushable".
                                You usually won't call this function, but one
                                of its aliases instead.

    git_subtree_restoreState - Restore subtree info from a new clone of a
                               GitHub repo.
                               Some of the subtree info, such as the remotes
                               created by 'git_subtree_add' aren't pushed to
                               GitHub.  So we need to save metadata about
                               them in committable files for later
                               restoration.

    git_subtree_add - Adds a new subtree to a repository, saving subtree
                      metadata for future use.
                      Also creates 'git remote's for each subtree, using a
                      remote name constructed from the subtree prefix.
                      Can add multiple subtrees at once.

    git_subtree_sync - Does a 'git subtree pull' with some standard args,
                       using either a prefix or a remote-name.
                       Can update multiple subtrees at once.

    git_subtree - Convenience-wrapper around the primary 'git_subtree_*'
                  functions.


    Aliases:
    --------

    git_subtree_readonly - Calls 'git_subtree_modifyRemotes readonly'.

    git_subtree_reset_push - Calls 'git_subtree_modifyRemotes reset-push'.


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
