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

BTRFS=/sbin/btrfs

if [ ! -x "$BTRFS" ]; then
    echo no btrfs executable found at "$BTRFS".  Install btrfs-tools under Linux.
    exit 1
fi

target="$PWD/btrfs"
ln_type="-s"

ln "$ln_type" "$BTRFS" "$target"  && chmod u+s "$target" && echo Created setuid at "$target" && exit 0


