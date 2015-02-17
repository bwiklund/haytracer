#!/bin/bash

cabal build && \
  ./dist/build/demo/demo && \
  convert -size 200x200 -depth 8 rgb:output.raw output.png && \
  open output.png
