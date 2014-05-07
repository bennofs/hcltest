#!/usr/bin/env bash

source travis/util.sh

step "Configuring project" << 'EOF'
  tmp=$(mktemp)
  cabal-$CABALVER configure --enable-tests --enable-benchmarks -v2 --ghc-options="-Wall -Werror" &> $tmp || (
    cat $tmp
    exit 1
  )
  echo "Using packages: "
  sed -nre 's/Dependency ([^ ]*) ==([^ :]*).*/\1 \2/p' $tmp | column -t | sed -e "s/^/  /"
EOF

step "Building project" << EOF
  cabal-$CABALVER build
EOF

step "Running tests" << EOF
  cabal-$CABALVER test
EOF

step "Creating source distribution" << EOF
  cabal-$CABALVER check
  cabal-$CABALVER sdist # tests that a source-distribution can be generated
EOF

step_suppress "Checking source distribution" << 'EOF'
  # The following scriptlet checks that the resulting source distribution can be built & installed
  SRC_TGZ=$(cabal-$CABALVER info . | awk '{print $2 ".tar.gz";exit}')
  cd dist/
  if [ -f "$SRC_TGZ" ]; then
    cabal-$CABALVER install "$SRC_TGZ"
  else
    echo "expected '$SRC_TGZ' not found"
    exit 1
  fi    
EOF
