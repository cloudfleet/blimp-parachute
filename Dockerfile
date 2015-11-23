# cloudfleet parachute client
#
# <http://docs.docker.com/engine/reference/builder/>

LABEL "VERSION" "0.0.1"

FROM debian

COPY    .   /opt/cloudfleet/apps/parachute
WORKDIR     /opt/cloudfleet/apps/parachute

RUN setup/install-parachute.bash

CMD bin/start-parachute-client.bash

