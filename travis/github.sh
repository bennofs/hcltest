#!/usr/bin/env bash

source travis/util.sh

package=$1

echo -e "${green}Installing $package from github${nc}"

tmp=`mktemp -d`  
git clone "https://github.com/$package" $tmp

olddir=$PWD
cd $tmp
cabal-$CABALVER install -j

cd $olddir
rm -rf $tmp
