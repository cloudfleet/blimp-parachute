# CloudFleet Parachute client

FROM debian:bookworm
LABEL "VERSION" "0.6.1"

# Provisioning 
RUN export DEBIAN_FRONTEND='noninteractive' && \
    apt-get update  && \
    apt-get install -y screen wget rsync binutils gcc \
     abcl ant-optional ant-contrib maven \
     sbcl cl-cffi 

COPY    .   /opt/cloudfleet/apps/parachute
WORKDIR     /opt/cloudfleet/apps/parachute

RUN setup/install-parachute.bash

# Service definition
CMD ["/bin/bash", "bin/start-parachute-client.bash"]


