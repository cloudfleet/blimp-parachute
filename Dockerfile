# cloudfleet parachute 
#
# <http://docs.docker.com/engine/reference/builder/>

LABEL "VERSION" "0.0.1"

FROM debian

COPY . /opt/cloudfleet/app/parachute
WORKDIR  /opt/cloudfleet/app/parachute

RUN setup/install-docker.bash

CMD bin/start-parachute-client.bash

