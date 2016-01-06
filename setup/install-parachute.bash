#!/bin/bash
set -x
. "/opt/cloudfleet/apps/parachute/etc/cf-vars.sh" 
if [[ -z "${CF}" ]]; then
   echo "Failed to source configuration" && exit 1
fi


tmp=${CF_TMP}
base=${CF_APPS}/parachute
chute="$base/chute"

ccl_arm_uri=ftp://ftp.clozure.com/pub/release/1.11/ccl-1.11-linuxarm.tar.gz
ccl_x86_uri=ftp://ftp.clozure.com/pub/release/1.11/ccl-1.11-linuxx86.tar.gz

ccl_uri=""

config_arm(){
    ccl_uri=${ccl_arm_uri}
    ccl_bin="armcl"
    ccl="$CF_APPS/ccl/$ccl_bin"
}

config_not_arm(){
    ccl_uri=${ccl_x86_uri}
    ccl_bin="lx86cl64"
    ccl="$CF_APPS/ccl/$ccl_bin"
}

if hash dpkg 2>/dev/null; then # if dpkg available
    if [ "`dpkg --print-architecture`" = "armhf" ]; then
        echo Configuring for Linux ARM
        config_arm
    else
        echo Configuring for Linux x64
        config_not_arm
    fi
else
    if [ $( uname -s ) =  "Darwin" ]; then
        echo Configuring for OS X
        ccl_uri=""
        ccl_bin=ccl64
        ccl=ccl64
    fi
fi

mkdir -p $base

# Install CCL
if [[ ! -z $ccl_uri ]]; then
    echo Installing CCL from _uri $ccl_uri
    (cd "$tmp" && wget --continue $ccl_uri)
    file=$( basename $ccl_uri )
    if [ ! -d "${CF_APPS}/ccl" ]; then
        (cd "${CF_APPS}" && tar xvzf "$tmp/$file")
    fi

fi 

# Download Quicklisp install shim
if [ ! -r "$HOME/quicklisp.lisp" ]; then
  (cd "$HOME" && wget --continue https://beta.quicklisp.org/quicklisp.lisp)
fi

# Populate ASDF registry
asdf_conf_d="${HOME}/.config/common-lisp/source-registry.conf.d"
mkdir -p "${asdf_conf_d}" 
rsync -avzP ${CF_APPS}/parachute/chute/chute.conf ${asdf_conf_d}/

if [ ! -d $HOME/quicklisp ]; then 
    $ccl --no-init --load "${CF_APPS}/parachute/chute/install-quicklisp.lisp"
fi

$ccl --eval '(progn (require :asdf) (load (asdf:system-relative-pathname (asdf:find-system :chute) "quicklisp-setup.lisp")) (quit))'






