#!/bin/sh

set -e

cp $1 $1.hs
stack runghc $1.hs
