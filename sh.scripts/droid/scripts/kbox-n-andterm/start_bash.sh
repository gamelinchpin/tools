ENV=$KBOX/etc/profile
export ENV

busybox_bin=$KBOX/bin/busybox
if [ -x $KBOX/bin/busybox-v1.18.4 ]; then
 busybox_bin=$KBOX/bin/busybox-v1.18.4
fi
has_bash=`$busybox_bin | grep bash`

if [ -n "$has_bash" ]; then
 exec $busybox_bin bash
fi
# else
exec ash


