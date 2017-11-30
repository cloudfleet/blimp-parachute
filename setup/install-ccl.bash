#!/usr/bin/env bash
ccl_uri_base=ftp://ftp.clozure.com/pub/release/1.11/

case `uname -s` in
    Darwin)
        ccl_uri=${ccl_uri_base}/ccl-1.11-darwinx86.tar.gz
        ;;
    Linux)
	if [[ ! $(which wget) ]]; then
	    echo Attempting to install wget.
            apt-get install -y wget
	fi
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
    if [[ ! -w ${dest} ]]; then
	echo No write permissions under ${dest}.
	echo Try running as root or specify a different destination.
	exit 1
    fi
    echo Installing ${ccl_uri} under ${dest}
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

install_ccl_wrapper () {
    ccl_root=$1
    ccl_wrapper=$2
    echo "#!/usr/bin/env bash" > ${ccl_wrapper}
    cat <<EOF >> ${ccl_wrapper}
export CCL_DEFAULT_DIRECTORY=${ccl_root}
exec $CCL \$* 
EOF
    chmod a+x ${ccl_wrapper}
    echo Created invocation wrapper as ${ccl_wrapper}.
}

install_ccl "${ccl_uri}" "${prefix}"
echo ccl installed as ${CCL}
install_ccl_wrapper "${prefix}/ccl" /usr/local/bin/ccl


