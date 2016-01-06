# cloudfleet parachute client
#
# <http://docs.docker.com/engine/reference/builder/>

FROM debian

LABEL "VERSION" "0.0.1"

# Install dependencies
RUN apt-get update && apt-get install -y screen wget btrfs-tools rsync

COPY    .   /opt/cloudfleet/apps/parachute
WORKDIR     /opt/cloudfleet/apps/parachute

RUN setup/install-parachute.bash

CMD bin/start-parachute-client.bash

