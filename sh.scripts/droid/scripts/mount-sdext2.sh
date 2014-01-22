l2sd_mpt=/data/sdext2/base.dir
sd_mpt=/sdcard/ext
p2dev=/dev/block/mmcblk1p2
mtopts=errors=remount-ro

if [[ -d $l2sd_mpt ]]; then
  mount -o bind,$mtopts $l2sd_mpt $sd_mpt
else
  #/system/bin/mount
  mount -t ext3 -o $mtopts $p2dev $sd_mpt
fi

unset l2sd_mpt sd_mpt p2dev mtopts
