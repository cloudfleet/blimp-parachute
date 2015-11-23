#!/bin/bash
. "/opt/cloudfleet/apps/parachute/etc/cf-vars.sh"
lisp=sbcl
exec $lisp --load "${CF_APPS}/parachute/chute/boot-server.lisp"

