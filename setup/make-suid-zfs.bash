#!/bin/bash
#
# Create suid btrfs for use by chute in current directory

# as root...
if [ $(id -u) != 0 ]; then
    echo Insufficient privileges to create setuid link
    exit 1
fi

if [ -O btrfs ]; then
    echo btrfs already exists
    exit 1
fi

ZFS=/sbin/zfs

if [ ! -x "$ZFS" ]; then
    echo no zfs executable found at "$ZFS".  Need a zfs, like FreeBSD-11/zfs or ubuntu/zfs.
    exit 1
fi

target="$PWD/zfs"
ln_type="-s"

ln "$ln_type" "$ZFS" "$target"  && chmod u+s "$target" && echo Created setuid at "$target" && exit 0


