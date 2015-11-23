#!/bin/bash
. "/opt/cloudfleet/apps/parachute/etc/cf-vars.sh"

tmp=${CF_TMP}
base=${CF_APPS}/parachute
chute="$base/chute"

ccl_arm_uri=ftp://ftp.clozure.com/pub/release/1.11/ccl-1.11-linuxarm.tar.gz
ccl_x86_uri=ftp://ftp.clozure.com/pub/release/1.11/ccl-1.11-linuxx86.tar.gz

ccl_uri=""

config_arm(){
    ccl_uri=${ccl_arm_uri}
    ccl_bin="armcl"
}

config_not_arm(){
    if [ $( uname -s ) =  "Darwin" ]; then
        ccl_uri=""
        ccl_bin=ccl64
    else 
        ccl_uri=${ccl_x86_uri}
        ccl_bin="lx86c64"
    fi
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
(cd "$tmp" && wget --continue $ccl_uri)
file=$( basename $ccl_uri )
mkdir -p $base
(cd "${CF_TMP} && tar xvzf "$tmp/$file")

# Install quicklisp
if [ ! -r "$HOME/quicklisp.lisp" ]; then
  (cd "$HOME" && wget --continue https://beta.quicklisp.org/quicklisp.lisp)
fi

ccl="$base/ccl/$ccl_bin"
#ccl=ccl64

# Populate ASDF registry
asdf_conf="${HOME}/.config/common-lisp/source-registry.conf.d"
mkdir -p "$asdf_conf"
cp chute/chute.conf "$asdf_conf"

if [ ! -d "$HOME/quicklisp" ]; then 
    $ccl --no-init --load "$chute/install-quicklisp.lisp"
else
    $ccl --eval "(progn (load \"$chute/quicklisp-setup.lisp\") (quit))"
fi





