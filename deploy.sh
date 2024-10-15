#!/usr/bin/env bash

cd ~/repos/no-rush && rsync -av --exclude '.*' ../no-rush dar:~/work/selfhost/apps/
