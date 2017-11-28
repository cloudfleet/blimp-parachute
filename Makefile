DOCKER	:= docker

.PHONY: all 
all: 
	$(DOCKER) build -t easye/blimp-parachute .

