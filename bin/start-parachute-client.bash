#!/bin/bash
. "/opt/cloudfleet/apps/parachute/etc/cf-vars.sh"

export CCL_DEFAULT_DIRECTORY=${CF_APPS}/ccl
if [[ $(uname -m) == "x86_64" ]]; then 
    CCL=${CCL_DEFAULT_DIRECTORY}/scripts/ccl64
else
    CCL=${CCL_DEFAULT_DIRECTORY}/scripts/ccl
fi

lisp=$CCL
exec $lisp --load "$CF_APPS/parachute/chute/boot-client.lisp"
