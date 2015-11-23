#!/bin/bash
. "/opt/cloudfleet/apps/parachute/etc/cf-vars.sh"
lisp=sbcl
screen $lisp --load "${CF_APPS}/parachute/chute/boot-server.lisp"

