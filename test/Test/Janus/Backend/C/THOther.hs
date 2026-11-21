{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Janus.Backend.C.THOther
  ( th_other_fac,
    splicedOtherCube,
  )
where

import Data.Int (Int64)
import Janus.Backend.C (JanusC)
import Janus.Backend.C.TH
import Test.Janus.Backend.C.THDefs (facPoly)

$(declareJanusC Safe "th_other_fac" (facPoly @JanusC @Int64))

splicedOtherCube :: Int64 -> Int64
splicedOtherCube =
  $(spliceJanusC Unsafe "th_other_cube" (\(x :: JanusC Int64) -> x * x * x))
