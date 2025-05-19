#!/bin/sh


cabal install --overwrite-policy=always

/home/zaku/.cabal/bin/cms-exec -y ./cms-exec.yaml


