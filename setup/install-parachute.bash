#!/usr/bin/env bash
DIR="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

cf_vars="/opt/cloudfleet/apps/parachute/etc/cf-vars.sh"

if [[ ! -f ${cf_vars} ]]; then
    cf_vars=${DIR}/../etc/cf-vars.sh
fi

. ${cf_vars}

if [[ -z "${CF}" ]]; then
   echo "Failed to source configuration from ${cf_vars}" && exit 1
fi

tmp=${CF_TMP}
base=${CF_APPS}/parachute
chute="$base/chute"

# Populate ASDF registry
asdf_symlinkfarm="~/common-lisp/"
mkdir -p "${asdf_symlinkfarm}"
pushd ${asdf_symlinkfarm} && ln -s {CF_APPS}/parachute

#CL=/usr/local/bin/sbcl
CL=abcl

# Install Quicklisp and the dependencies needed
$CL --no-init --batch "${CF_APPS}/parachute/chute/install-quicklisp.lisp"

