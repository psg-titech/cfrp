#!/bin/sh

cabal sandbox init

# check that happy is already available
if [ ! `which happy` ]; then
    cabal install happy
fi

# to avoid the dependency problems that may arise when installing
# the peggy package (thanks to Shohei Yasutake)
cabal install --only-dependencies \
      --allow-newer=ListLike,hashtables,template-haskell

cabal install

