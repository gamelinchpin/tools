if [ -d /mnt/sdcard ]; then
    cd /mnt/sdcard
fi

find . \! -type d |\
    grep -v '^./.FW.Recovery' | grep -v '^./clockworkmod' |\
    grep -v '^./Backups' | grep -v '^./tmp' | grep -v '^./Music' |\
    grep -v '^./.quickoffice/temp' |\
    grep -v '^./.zooniverse/files/gz-stockpile' |\
    sort
