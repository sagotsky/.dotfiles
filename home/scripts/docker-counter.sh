#!/usr/bin/env bash

while : ; do
    docker ps -q | wc -l
    sleep 30
done
