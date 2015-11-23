#!/bin/bash
. "/opt/cloudfleet/apps/parachute/etc/cf-vars.sh"

lisp=ccl
exec $lisp --load "$CF_APPS/parachute/chute/boot-client.lisp"
