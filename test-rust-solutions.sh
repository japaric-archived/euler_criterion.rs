#!/bin/bash

export PATH="${PATH}:$(pwd)/rustic/target/release"

set -e
set -x

for path in $(find problems -name *.rs); do
  pushd $(dirname $path)
  file=$(basename $path)
  ans=$(cat ${file%.*}.ans)
  sol=$(rustic $file -a)
  [ "$sol" -eq "$ans" ]
  popd
done
