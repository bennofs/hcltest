#!/usr/bin/env bash

source travis/util.sh

package=$1

tmp=`mktemp -d`  

step_suppress "Getting sources of $package" <<EOF
  git clone "https://github.com/$package" $tmp
EOF

step_suppress "Adding $package to sandbox" <<EOF
  cabal-\$CABALVER sandbox add-source $tmp
EOF
