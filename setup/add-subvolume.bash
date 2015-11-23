#!/bin/bash
#
# create btrfs subvolume, copy $path, remove $path
. "/opt/cloudfleet/apps/parachute/etc/cf-vars.sh"

path=$CF_DATA
path_copy=${path}.copy

if [ -e ${path_copy} ]; then 
   echo Location to backup cloudfleet data already exists at "${path_copy}"
   exit 1
fi

echo Stopping Cloudfleet services
/opt/cloudfleet/engineroom/bin/stop-containers.sh

mv "$path" "$path_copy"

btrfs subvolume create $CF_DATA
rsync -avzP "${path_copy}"/ "${path}"/
rm -rf "${path_copy}"

mkdir "${path}/.snapshot"

echo Starting Cloudfleet services
/opt/cloudfleet/engineroom/bin/start-containers.sh # XXX Does this script background itself? How can we tell?





