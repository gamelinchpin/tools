#!/bin/bash
#
# Copyright (C) 2008-2012 by John P. Weiss
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


FUSEFS_MOUNT_BASE=${FUSEFS_MOUNT_BASE:-~/mnt}
FUSEFSTAB=${FUSEFSTAB:-$FUSEFS_MOUNT_BASE/fusefstab}


############
#
# Includes & Other Global Variables
#
############


ssh_common_opts="reconnect,follow_symlinks,ServerAliveInterval=120"
ssh_rw_opts="$ssh_common_opts,idmap=user"
ssh_ro_opts="$ssh_common_opts,idmap=none"

ftp_common_opts="transform_symlinks"


############
#
# Functions
#
############


cleanup_mountpoint() {
    local mountpoint="$1"
    shift

    mountpoint="${mountpoint%/}"
    case "$mountpoint" in
        ~/*)
            mountpoint="${HOME}/${mountpoint#~/}"
            ;;
        /*)
            :
            ;;
        *)
            mountpoint="${FUSEFS_MOUNT_BASE}/${mountpoint#./}"
            ;;
    esac
    if [ ! -d $mountpoint ]; then
        mkdir -p $mountpoint
    fi

    echo $mountpoint
}


mount_sshfs() {
    local accessType="$1"
    shift
    local login="$1"
    shift
    local mountpoint="$1"
    shift
    local alias="$1"
    shift

    local accessOptions="$ssh_rw_opts"
    case "$accessType" in
        *ro)
            accessOptions="$ssh_ro_opts"
            ;;
    esac

    case "$login" in
        *:*)
            # Do nothing.
            :
            ;;
        *)
            # Must append a ':' to the login to ensure successful mount.
            login="$login:"
            ;;
    esac

    mountpoint="$(cleanup_mountpoint $mountpoint)"

    echo "Mounting \"$login\" at \"$mountpoint\"..."
    sshfs -o $accessOptions $login $mountpoint
    return $?
}


mount_ftpfs() {
    local accessType="$1"
    shift
    local login="$1"
    shift
    local mountpoint="$1"
    shift
    local alias="$1"
    shift

    local accessOption=""
    case "$accessType" in
        *ro)
            accessOptions="-r"
            ;;
    esac

    local ftp_opts="$ftp_common_opts"
    local auth="${login%%@*}"
    case "$auth" in
        *:*)
            ftp_opts="${ftp_opts},user=${auth}"
            ;;
        *)
            local passwd
            read -s -p "Enter password for ${login%%:*}: " passwd
            ftp_opts="${ftp_opts},user=${auth}:${passwd}"
            echo ""
            ;;
    esac

    local url="ftp://${login##*@}/"
    case "$url" in
        *:[0-9]*[0-9]:*)
            local port="${url%:*}"
            port="${port##*:}"
            url="${url/:${port}:/:${port}}"
            echo "Connecting on port $port"
            ;;
    esac

    mountpoint="$(cleanup_mountpoint $mountpoint)"

    echo "Mounting \"$url\" at \"$mountpoint\"..."
    curlftpfs $accessOptions -o $ftp_opts $url $mountpoint
    return $?
}


parse_fusefstab() {
    perl -e '
        BEGIN {
            my $matchStr=join("|", sort(split(/\s/,"'"$*"'")));
            $match_re="";
            if ($matchStr ne "") { $match_re="(?:$matchStr)"; }
        }
        ' -n -e '
        unless (m/^[#]/ || m/^\s*$/) {
            if (($match_re eq "") || m/$match_re/) {
                chomp;
                @fields=split;
                if ($#fields > 0) {
                    if (($#fields < 3) || ($fields[3] eq "auto")) {
                        $fields[3] = $fields[2];
                        $fields[3] =~ s|.*/([^/]+)|$1|;
                    }
                    print(join(",", @fields), "\n");
                }
            }
        }' $FUSEFSTAB
}


mount_by_type() {
    local accessType="$1"
    shift
    local mountType="$1"
    shift

    case "$mountType" in
        --ssh)
            mount_sshfs "$accessType" "$@"
            ;;
        --ftp)
            mount_ftpfs "$accessType" "$@"
            ;;
    esac
}


batch_mount() {
    local accessType="$1"
    shift

    if [ "$1" = "--all" ]; then
        set -- `parse_fusefstab`
    else
        set -- `parse_fusefstab "$@"`
    fi

    local mntArgs
    for mntArgs in "$@"; do
        mount_by_type "$accessType" ${mntArgs//,/ }
    done
}


print_usage() {
    local myname="`basename $0`"
    echo "usage:  $myname [-h|--help|--all|-a|--auto|-rw|-ro]"
    echo "        $myname [-rw|-ro] <login>|<mountpoint>|<alias> \\"
    echo "            [<login>|<mountpoint>|<alias> ...]"
    echo "        $myname <typeopt> [-rw|-ro] -m <loginInfo> <mountpoint>"
    cat - <<-EOF

	Environment Variables:

	FUSEFS_MOUNT_BASE - Absolute path containing all of your "sshfs"
	                    mountpoints.  Any newly-created mountpoints go here,
	                    too.
	                    Default is "~/mnt/".
	FUSEFSTAB - Name and location of an "fstab"-like file used by "$myname"
	            to match an "ssh" login with a mountpoint.  Defaults to
	            "\$FUSEFS_MOUNT_BASE/fusefstab".

	Options:
	--------
	-h
	--help
	    This message.

	--all
	    Mount every last entry found in the \$FUSEFSTAB.
	    Mutually-exclusive with the "--auto" option.

	-a
	--auto
	    Mount only those \$FUSEFSTAB entries containing the "auto" keyword.
	    Mutually-exclusive with the "--all" option.

	-m <loginInfo> <mountpoint>
	    Like calling "sshfs", but using $myname's default "-o" options.
	    Will also create <mountpoint> if it doesn't exist.
	    <loginInfo> and <mountpoint> are as described below.

	-rw
	    Mount everything in read-write mode.  Can be used with any other
	    option or way of calling $myname.  Will override any earlier "-ro"
	    on the commandline; the last one specified is the one that "wins".
	    This is (presently) the default behavior.

	-ro
	    Mount everything read-only.  Overrides any earlier "-rw" option on
	    the commandline.  (The last "-ro" or "-rw" on the commandline is
	    the one used.)  Can be used with any other options or any of the
	    ways of calling $myname.

	<typeopt>
	    An option specifying the type of FUSE filesystem being mounted,
	    required when using the "-m" option.  Must be one of the following:

	    --ssh        Mount over 'sshfs'.
	    --ftp        Mount using 'curlftpfs'.

	Positional Parameters:
	----------------------
	When run without any options, each of the positional parameters is
	treated as either ssh login information or the name of a directory to
	use as a mountpoint.  It's looked up in the \$FUSEFSTAB and mounted
	accordingly.

	The specific syntax expected by $myname is as follows:
	  <loginInfo>
	    Anything containing the '@' or ':' character is treated as an "ssh"
	    login.  Partial login information will match *everything* in
	    \$FUSEFSTAB with that login.  All matching entries are mounted.

	  <mountpoint>
      <alias>
	    Anything else is treated as an expression to match against the
	    mountpoints or aliases in \$FUSEFSTAB.  Every line in \$FUSEFSTAB
	    that has a mountpoint *or* alias matching the expression is mounted.
	    Missing mountpoints in \$FUSEFSTAB are created.

	Any other positional parameters are treated as bogus and ignored.

	The \$FUSEFSTAB File:
	--------------------
	Its syntax is rather straightforward:

	    <typeopt> <loginInfo> <mountpoint> [<alias>|auto]

	The 4 elements can be separated by any amount of whitespace.  Lines
	beginning with a '#' are ignored.
	  The <typeopt> is one of the options documented above that are needed
	  on the commandline when using the "-m" option.

	  The <loginInfo> is as you'd specify on the commandline.

	  The <mountpoint> can be either an absolute path or a relative one.
	  Relative directories are treated as relative to \$FUSEFS_MOUNT_BASE
	  ... NOT relative to the current directory.  You can use a leading '~'
	  or a trailing '/' (useful for relative mountpoints); they're handled
	  correctly.

	  The <alias> should be a distinct name for this
	  <typeopt>+<loginInfo>+<mountpoint> combination.  Try to make your
	  aliases "non-substring"; i.e. avoid making an alias that is a
	  substring of several others.  If you don't, you'll end up mounting
	  the whole set of 'em when you ask to mount the "substring-alias".  On
	  the other hand, you can use this quirk to your advantage:  make
	  <alias> a comma-separated list of tags.  You can then use any one of
	  those tags to mount that entry.

	  There is a special <alias>, the string 'auto'.  It's used with the
	  "--auto" commandline option.
EOF
}


usage() {
    local exitcode=1
    if [ -n "$1" ]; then
        exitcode=$1
    fi

    print_usage |& less

    exit $exitcode
}


############
#
# Main
#
############


mountType=""
case "$0" in
    *mount[-_]sshfs*)
        mountType="--ssh"
        ;;
    *mount[-_]ftpfs*)
        mountType="--ftp"
        ;;
esac

accessType=""
toMount=""
loginInfo=""
mountPt=""
while [ -n "$1" ]; do
    case "$1" in
        -h|--help)
            usage 0
            ;;
        -r[ow]|--r[ow])
            accessType="$1"
            ;;
        --ssh|--ftp)
            mountType="$1"
            ;;
        --all)
            batch_mount "$accessType" --all
            exit 0
            ;;
        --auto|-a)
            batch_mount "$accessType" auto
            exit 0
            ;;
        -m)
            shift
            loginInfo="$1"
            shift
            mountPt="$1"
            if [ -z "$login" ]; then
                echo "Missing required argument:  <loginInfo>"
                usage 1
            elif [ -z "$mountPt" ]; then
                echo "Missing required argument:  <mountpoint>"
                usage 1
            fi
            ;;
        *)
            toMount="$toMount $1"
            ;;
    esac
    shift
done

if [ -n "$loginInfo" ]; then
    mount_by_type "$accessType" "$mountType" "$loginInfo" "$mountPt"
elif [ -z "$toMount" ]; then
    usage 1
else
    batch_mount "$accessType" $toMount
fi


#################
#
#  End
