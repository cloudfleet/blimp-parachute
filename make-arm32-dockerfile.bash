#!/bin/bash
target="Dockerfile-arm32"
if [ -r $target ]; then
    echo Regenerating $target
fi
cat Dockerfile | sed -e 's/^FROM debian/FROM registry.marina.io\/debian/' | tee $target


