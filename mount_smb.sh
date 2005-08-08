#!/bin/bash
#
# Copyright (C) 2004 by John P. Weiss
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
# RCS $Id$
############


############
#
# Configuration Variables
#
############


SMB_IP=${SMB_IP:-127.0.0.1}
WIN_DOMAIN=${WIN_DOMAIN:-myWinDomain}
SMB_UID=${USER}

MOUNTPT_BASE="/mnt"


############
#
# Includes & Other Global Variables
#
############


#. some.include.sh


############
#
# Functions
#
############


start() {
    options="ro,ip=${SMB_IP},username=${SMB_UID},workgroup=${WIN_DOMAIN}"
    share="$1"
    shift
    mountPt="$1"
    shift
    mypasswd="$1"
    shift

    echo "Trying to mount \"${share}\" on \"${mountPt}\""
    PASSWD="${mypasswd}" \
        sudo smbmount "${share}" "${mountPt}" -o "${options}"
}


stop() {
    mountPt="$1"
    shift

    echo "Unmounting \"${mountPt}\""
    sudo smbumount "${mountPt}"
}


svc_is_mountPt() {
    nmbName="$1"
    shift
    mountPt_pre="$1"
    shift
    action="$1"
    shift
    passwd="$1"
    shift

    for svcNm in "$@"; do
        mountPt="${mountPt_pre}/${svcNm}"
        service="//${nmbName}/${svcNm}"
        if [ ! -d ${mountPt} ]; then
            echo "Directory \"${mountPt}\" does not exist."
            echo "Cannot mount \"${service}\"."
            continue
        fi
        case $action in
            start|mount)
                start "${service}" "${mountPt}" \
                    "${passwd}"
                ;;
            stop|umount|unmount)
                stop "${mountPt}"
                ;;
            *)
                echo "Would mount: ${service} on ${mountPt}"
                ;;
        esac
    done
}


############
#
# Main
#
############


action="$1"
shift

myPasswd='..none..'
case $action in
    start|mount)
        IFS="" read -p "Enter Windows Login Password: " -s -r myPasswd
        echo ""
        ;;
esac
echo "The next password request is from \"sudo\"."
echo "(It simply runs \"sudo smbumount >/dev/null\".)"
sudo smbmount >/dev/null
echo "(If you saw nothing, it means your last \"sudo\" is still valid.)"
echo ""
echo "Any subsequent password prompts are from \"smbmount\" (or should be)."

svc_is_mountPt NMB_NAME ${MOUNTPT_BASE}/lc_nmb_name ${action} "${myPasswd}" \
    svc1 svc2


#################
#
#  End
