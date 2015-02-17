#!/bin/bash

cabal build && \
  ./dist/build/demo/demo && \
  convert -size 2x2 -depth 8 rgb:output.raw output.png && \
  open output.png
