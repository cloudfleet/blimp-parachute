DOCKER	:= podman

.PHONY: all 
all: 
	$(DOCKER) build -t $(USER)/chute .

