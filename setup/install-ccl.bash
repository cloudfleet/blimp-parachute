#!/usr/bin/env bash
ccl_uri_base=ftp://ftp.clozure.com/pub/release/1.11/

case `uname -s` in
    Darwin)
        ccl_uri=${ccl_uri_base}/ccl-1.11-darwinx86.tar.gz
        ;;
    Linux)
        apt-get install -y wget 
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

prefix=/usr/local
export CCL_DEFAULT_DIRECTORY=${prefix}/ccl
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
    (cd $prefix/ccl/lisp-kernel/linuxarm && make clean && make)
}

# Install CCL
install_ccl () {
    ccl_uri=$1
    dest=$2
    tmp=/tmp/
    if [[ ! -z $ccl_uri ]]; then
        echo Installing CCL from uri $ccl_uri
        (cd "$tmp" && wget --continue $ccl_uri)
        file=$( basename $ccl_uri )
        if [ ! -d "${dest}/ccl" ]; then
            (cd "$dest" && tar xvzf "$tmp/$file")
        fi
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
}



install_ccl $ccl_uri $prefix
