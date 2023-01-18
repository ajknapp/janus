{-# LANGUAGE FunctionalDependencies #-}

module Janus.Command.TicToc where

import Data.Functor.Identity
import Data.Word
import Foreign.C

foreign import ccall janus_tic :: IO CLLong

foreign import ccall janus_toc :: IO CLLong

newtype Tic e = Tic { getTic :: e Word64 }

newtype Toc e = Toc { getToc :: e Word64 }

ticTocDelta :: Num (e Word64) => Tic e -> Toc e -> e Word64
ticTocDelta (Tic tic') (Toc toc') = toc'-tic'

class CmdTicToc m e | m -> e, e -> m where
  tic :: m (Tic e)
  toc :: m (Toc e)

instance CmdTicToc IO Identity where
  tic = Tic . Identity . fromIntegral <$> janus_tic
  toc = Toc . Identity . fromIntegral <$> janus_toc
