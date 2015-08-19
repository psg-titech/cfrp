{-# LANGUAGE CPP #-}
module Language.CFrp.Compat (
    modifySTRef'
  ) where

import Control.Monad.ST (ST)
import qualified Data.STRef as ST

modifySTRef' :: ST.STRef s a -> (a -> a) -> ST s ()
#if MIN_VERSION_base(4,6,0)
modifySTRef' = ST.modifySTRef'
#else
modifySTRef' ref f = do
    x <- ST.readSTRef ref
    let x' = f x
    x' `seq` ST.writeSTRef ref x'
#endif
