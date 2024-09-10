#ghcup install ghc 9.8.2 --set

#ghcup install cabal 3.12.1.0 --set

cabal update

echo '*** START ***'

cabal build sequential --ghc-options -Werror

echo '*** END ***'
