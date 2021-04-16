#!/bin/bash
set -x # echo on
set -eu

# Make sure this is called from root directory
if [[ "$(pwd)" != "$(git rev-parse --show-toplevel)" ]]; then
  echo "this must be called from mina's root folder"
  exit 1
fi

# Keep compile dirs to avoid recompiles
export OPAMKEEPBUILDDIR='true'
export OPAMREUSEBUILDDIR='true'
export OPAMYES=1

# Set term to xterm if not set
export TERM=${TERM:-xterm}

# init opam
if ! [[ -d ~/.opam ]]; then
  opam init
  eval $(opam config env)
fi

# create local switch
opam switch import src/opam.export --switch .
sudo chmod -R u+rw _opam
eval $(opam config env)

# add custom O(1) Labs opam repository to local switch
O1LABS_REPO='https://github.com/o1-labs/opam-repository.git'
opam repository add --yes --this-switch o1-labs "$O1LABS_REPO"

# All our ocaml packages
opam update
eval $(opam config env)

# show switch list at the end
echo "opam switch list"
opam switch list
