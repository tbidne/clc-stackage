set -e

echo "*** Updating cabal ***"

cabal update

echo "*** Installing clc-stackage ***"

# --overwrite-policy=always and deleting output/ are unnecessary for CI since
# this script will only be run one time, but it's helpful when we are
# testing the script locally.
cabal install exe:clc-stackage --installdir=./bin --overwrite-policy=always

if [[ -d output ]]; then
  rm -r output
fi

echo "*** Building all ***"

set +e
./bin/clc-stackage --batch 200 --cabal-options="--semaphore"

ec=$?

.github/scripts/print_logs.sh

if [[ $ec != 0 ]]; then
  echo "*** clc-stackage failed ***"
  exit $ec
else
  echo "*** clc-stackage succeeded ***"
fi
