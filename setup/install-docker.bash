#!/bin/bash

tmp=/var/tmp
base=/opt/cloudfleet/app/parachute
chute="$base/chute"

ccl_arm_uri=ftp://ftp.clozure.com/pub/release/1.11/ccl-1.11-linuxarm.tar.gz
ccl_x86_uri=ftp://ftp.clozure.com/pub/release/1.11/ccl-1.11-linuxx86.tar.gz

ccl_uri=""

config_arm(){
    ccl_uri=${ccl_arm_uri}
    ccl_bin="armcl"
}

config_not_arm(){
    ccl_uri=${ccl_x86_uri}
    ccl_bin="lx86c64"
}

if hash dpkg 2>/dev/null; then # if dpkg available
    if [ "`dpkg --print-architecture`" = "armhf" ]; then
        config_arm
    else
        config_not_arm
    fi
else
    config_not_arm
fi

# Install CCL 
echo ccl_uri $ccl_uri
cd "$tmp" && wget --continue $ccl_uri
file=$( basename $ccl_uri )
mkdir -p $base
cd "$base" && tar xvzf "$tmp/$file"

# Install quicklisp
cd "$HOME" && wget https://beta.quicklisp.org/quicklisp.lisp

ccl="$base/ccl/$ccl_bin"

# Populate ASDF registry
asdf_conf="${HOME}/.config/common-lisp/source-registry.conf.d"
mkdir -p "$asdf_conf"
cp chute/chute.conf "$asdf_conf"

ccl --no-init --load "$chute/install-quicklisp.lisp"





