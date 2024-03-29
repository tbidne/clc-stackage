#!/usr/bin/env bash

set -e

export LANG="C.UTF-8"

cabal update

echo '*** Building clc-stackage'

# disable error due to timeout
set +e

# run for 10 min
timeout --signal=2 -- 600 cabal build clc-stackage --ghc-options -Werror

exit_code=$?

# restore errors
set -e

# Command should exit due to timeout
if [[ $exit_code -ne 124 ]]; then
  echo "*** Expected timeout, received: '$exit_code'"
  exit 1
fi

echo '*** Finished'
