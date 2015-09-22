#!/bin/bash

# as root...
if [ $(id -u) != 0 ]; then
    echo Insufficient privileges to create setuid link
    exit 1
fi

if [ -O btrfs ]; then
    echo btrfs already exists
    exit 1
fi

ln /sbin/btrfs btrfs
chmod u+s btrfs
echo Created setuid at  $PWD/btrfs 

