if [ -d /data/sdext2/base.dir ]; then
  mount -o bind /data/sdext2/base.dir /sdcard/ext
else
  #/system/bin/mount
  mount -t ext3 /dev/block/mmcblk1p2 /sdcard/ext
fi
