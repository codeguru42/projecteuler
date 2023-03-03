#!/bin/bash
for file in problem*.hs; do
    ghc $file
done
