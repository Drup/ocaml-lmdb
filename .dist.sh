#!/bin/bash
#
# dist
# ----
# Copyright : (c) 2012, Jeremie Dimino <jeremie@dimino.org>
# Licence : BSD3
#
# Script to build the release

set -e

# Extract project parameters from _oasis
NAME=`opam show -f name . 2>/dev/null`
VERSION=`opam show -f version . 2>/dev/null`
PREFIX=$NAME-$VERSION
ARCHIVE=$(pwd)/$PREFIX.tar.gz

# Clean setup.data and other generated files.
dune clean

# Create a branch for the release
git checkout -b release-$VERSION

# Remove this script and dev-files
rm -f .dist.sh .travis*

# Commit
git add --all --force
git commit
git tag $VERSION

git checkout master

# Prepare publishing
