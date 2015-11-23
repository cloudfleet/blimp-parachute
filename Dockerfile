# cloudfleet parachute client
#
# <http://docs.docker.com/engine/reference/builder/>

LABEL "VERSION" "0.0.1"

FROM debian

# Install dependencies
RUN apt-get install -y screen wget btrfs-tools

COPY    .   /opt/cloudfleet/apps/parachute
WORKDIR     /opt/cloudfleet/apps/parachute

RUN setup/install-parachute.bash

CMD bin/start-parachute-client.bash

