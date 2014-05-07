#!/usr/bin/env bash

source travis/util.sh

package=$1

step "Installing $package from github" <<EOF
  git clone "https://github.com/$package"
  cd "$package"
  cabal-\$CABALVER install -j
  cd ..
EOF
