set +e

if command -v clc-stackage &> /dev/null; then
  echo "*** clc-stackage exists on PATH, not re-installing ***"

# -f not -x since downloaded exe may not have executable permissions.
elif [[ -f ./bin/clc-stackage ]]; then
  echo "*** ./bin/clc-stackage exists, not re-installing ***"

  # May need to add permissions, if this exe was downloaded
  chmod a+x ./bin/clc-stackage

  export PATH=./bin:$PATH

else
  echo "*** Updating cabal ***"
  cabal update

  # --overwrite-policy=always and deleting output/ are unnecessary for CI since
  # this script will only be run one time, but it's helpful when we are
  # testing the script locally.

  echo "*** Installing clc-stackage ***"
  cabal install exe:clc-stackage --installdir=./bin --overwrite-policy=always
  export PATH=./bin:$PATH
fi

set -e
