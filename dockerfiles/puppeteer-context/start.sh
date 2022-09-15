#!/bin/bash

echo "Trying to start daemon on $(hostname)"

kill -s SIGUSR2 $(/find_puppeteer.sh)

while [ ! -f /root/daemon-active ]; do sleep 1; done
