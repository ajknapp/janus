{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Janus.Backend.C.THOther
  ( th_other_fac,
    th_other_fac_i32,
    splicedOtherCube,
  )
where

import Data.Int (Int32, Int64)
import Janus.Backend.C (JanusCCSE)
import Janus.Backend.C.TH
import Test.Janus.Backend.C.THDefs (facPoly)

$(declareJanusC Safe "th_other_fac" (facPoly @JanusCCSE @Int64))
$(declareJanusCAs Safe "th_other_fac" "th_other_fac_i32" (facPoly @JanusCCSE @Int32))

splicedOtherCube :: Int64 -> Int64
splicedOtherCube =
  $(spliceJanusC Unsafe "th_other_cube" (\(x :: JanusCCSE Int64) -> x * x * x))
