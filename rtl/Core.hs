-----------------------------------------------------------------------------
-- |
-- Module      :  Core
-- Maintainer  :  Felix Klein (felix@qbaylogic.com)
--
-- Clash core example
--
-----------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

module Core where

-----------------------------------------------------------------------------

import Clash.Explicit.Prelude

-----------------------------------------------------------------------------

createDomain vSystem{vName="SampleClock"}

type FpOffset = 2 :: Nat
type DataWidth = 16 :: Nat

fromMv :: Signed DataWidth -> Signed DataWidth
fromMv = (`shiftL` (natToNum @FpOffset :: Int))

-----------------------------------------------------------------------------

{-# ANN topEntity
  ( Synthesize
      { t_name = "core"
      , t_inputs =
          [ PortName "clk"
          , PortName "sample_clk"
          , PortName "sample_in0"
          , PortName "sample_in1"
          , PortName "sample_in2"
          , PortName "sample_in3"
          ]
      , t_output = PortProduct ""
          [ PortName "sample_out0"
          , PortName "sample_out1"
          , PortName "sample_out2"
          , PortName "sample_out3"
          ]
      }
  ) #-}

topEntity ::
  Clock System ->
  Clock SampleClock ->
  Signal SampleClock (Signed DataWidth) ->
  Signal SampleClock (Signed DataWidth) ->
  Signal SampleClock (Signed DataWidth) ->
  Signal SampleClock (Signed DataWidth) ->
  ( Signal SampleClock (Signed DataWidth)
  , Signal SampleClock (Signed DataWidth)
  , Signal SampleClock (Signed DataWidth)
  , Signal SampleClock (Signed DataWidth)
  )
topEntity _ clk in1 in2 _in3 _in4 = (out1, out2, out3, out4)
 where
  redge = isRising clk rst enableGen False ((> fromMv 2000) <$> in1)

  counter =
    register clk rst enableGen (0 :: Unsigned 3)
      $ mux redge (satSucc SatWrap <$> counter) counter

  rst = unsafeFromActiveHigh $ pure False

  -- mirror of the first input
  out1 = in1

  -- average of the first and the second input
  out2 = (\a b -> min a b + (abs (a - b) `div` 2))
    <$> in1
    <*> in2

  -- schmitt-trigger pulses
  out3 = mux ((== 2) <$> counter) outHi outLo
  out4 = mux ((== 7) <$> counter) outHi outLo

  outLo = pure $ fromMv 0
  outHi = pure $ fromMv 5000

-----------------------------------------------------------------------------