#!/bin/bash

set -x

. "/opt/cloudfleet/apps/parachute/etc/cf-vars.sh" 
if [[ -z "${CF}" ]]; then
   echo "Failed to source configuration" && exit 1
fi

tmp=${CF_TMP}
base=${CF_APPS}/parachute
chute="$base/chute"

ccl_uri_base=ftp://ftp.clozure.com/pub/release/1.11/

case `uname -s` in
    Darwin)
        ccl_uri=${ccl_uri_base}/ccl-1.11-darwinx86.tar.gz
        ;;
    Linux)
        case `uname -m` in
            *86*)
                ccl_uri=${ccl_uri_base}/ccl-1.11-linuxx86.tar.gz
                ;;
            *arm*)
                ccl_uri=${ccl_uri_base}/ccl-1.11-linuxarm.tar.gz
                ;;
        esac
        ;;
    FreeBSD)
        ccl_uri=${ccl_uri_base}/ccl-1.11-freebsdx86.tar.gz
        ;;
    *)
        echo "Can't determine host OS."
        exit 1
        ;;
esac

mkdir -p $base
export CCL_DEFAULT_DIRECTORY=${CF_APPS}/ccl
if [[ $(uname -m) == "x86_64" ]]; then 
    CCL=${CCL_DEFAULT_DIRECTORY}/scripts/ccl64
else
    CCL=${CCL_DEFAULT_DIRECTORY}/scripts/ccl
fi

test_ccl() {
    $CCL --no-init --eval '(quit)'
}

recompile_ccl () {
    apt-get install -y make m4 
    # XXX remove the hard coding of Linux ARM recompilation
    (cd ${CF_APPS}/ccl/lisp-kernel/linuxarm && make clean && make)
}

# Install CCL
if [[ ! -z $ccl_uri ]]; then
    echo Installing CCL from uri $ccl_uri
    (cd "$tmp" && wget --continue $ccl_uri)
    file=$( basename $ccl_uri )
    if [ ! -d "${CF_APPS}/ccl" ]; then
        (cd "${CF_APPS}" && tar xvzf "$tmp/$file")
    fi
    test_ccl
    if [[ $? -ne 0 ]]; then
        recompile_ccl
        test_ccl
        if [[ $? -ne 0 ]]; then
            echo "Failed to recompile CCL."
            exit 1
        fi
    fi
fi 

# Download Quicklisp installation code
if [[ ! -r "$HOME/quicklisp.lisp" ]]; then
  (cd "$HOME" && wget --continue https://beta.quicklisp.org/quicklisp.lisp)
fi

# Populate ASDF registry
asdf_conf_d="${HOME}/.config/common-lisp/source-registry.conf.d"
mkdir -p "${asdf_conf_d}" 
rsync -avzP ${CF_APPS}/parachute/chute/chute.conf ${asdf_conf_d}/

# Install Quicklisp and the dependencies needed
$CCL --no-init --load "${CF_APPS}/parachute/chute/install-quicklisp.lisp"

