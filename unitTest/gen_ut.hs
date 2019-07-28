-- Copyright (c) 2019 Shawn Eary
--
-- Unit Test for Generator.hs
--
-- This Unit Test is Licenced via the MIT License
--
-- COMPILATION STEPS:
-- 1) Install GHC
-- 2) Install GNU Make
-- 3) Change to this directory and run the command
--    make
import Generators as Gen
import UnitTestHelpers



main :: IO ()
main = do
  -- ###############################################
  -- # Trivial 1 Hertz 16 Bit Test                 #
  -- ###############################################
  -- 16 Bit Sine Generator at 1 Hertz should
  -- yield 0 at time = 0.0
  verify "test1HZ-1" 0 (Gen.sine 1 0.0 16)

  -- 16 Bit Sine Generator at 1 Hertz should yield
  -- sixteenBitAmplitude at time = 0.25
  verify "test1HZ-2" Gen.sixteenBitAmplitude (Gen.sine 1 0.25 16)

  -- 16 Bit Sine Generator at 1 Hertz should yield
  -- 0 at time = 0.5
  verify "test1HZ-3" 0 (Gen.sine 1 0.5 16)

  -- 16 Bit Sine Generator at 1 Hertz should yield
  -- -sixteenBitAmplitude at time = 0.75
  verify "test1HZ-4" (0-sixteenBitAmplitude) (Gen.sine 1 0.75 16)



  -- ###############################################
  -- # Trivial 2 Hertz 16 Bit Test                 #
  -- ###############################################
  -- 16 Bit Sine Generator at 2 Hertz should
  -- yield 0 at time = 0.0
  verify "test2HZ-1" 0 (Gen.sine 2 0.0 16)

  -- 16 Bit Sine Generator at 2 Hertz should yield
  -- sixteenBitAmplitude at time = 0.125
  verify "test2HZ-2" Gen.sixteenBitAmplitude (Gen.sine 2 0.125 16)

  -- 16 Bit Sine Generator at 2 Hertz should yield
  -- 0 at time = 0.25
  verify "test2HZ-3" 0 (Gen.sine 2 0.25 16)

  -- 16 Bit Sine Generator at 1 Hertz should yield
  -- -sixteenBitAmplitude at time = 0.375
  verify "test2HZ-4" (0-sixteenBitAmplitude) (Gen.sine 2 0.375 16)



  -- ###############################################
  -- # Trivial 2000 Hertz 16 Bit Test              #
  -- ###############################################
  let testFreq = 2000
  let fullPeriod = 1.0 / fromIntegral(testFreq)
  let threeFourthsPeriod = fullPeriod * 3.0 / 4.0
  let halfPeriod = fullPeriod / 2.0
  let quarterPeriod = fullPeriod / 4.0
  
  -- 16 Bit Sine Generator should
  -- yield 0 at time = 0.0
  verify "test2000HZ-1" 0 (Gen.sine testFreq 0.0 16)

  -- 16 Bit Sine Generator at 2000 Hertz should yield
  -- sixteenBitAmplitude at time = quarterPeriod
  verify
     "test2000HZ-2"
     Gen.sixteenBitAmplitude
     (Gen.sine testFreq quarterPeriod 16)

  -- 16 Bit Sine Generator at 2000 Hertz should yield
  -- 0 at time = halfPeriod
  verify "test2000HZ-3" 0 (Gen.sine testFreq halfPeriod 16)

  -- 16 Bit Sine Generator at 2000 Hertz should yield
  -- -sixteenBitAmplitude at time = threeFourthsPeriod
  verify
     "test2000HZ-4"
     (0-sixteenBitAmplitude)
     (Gen.sine testFreq threeFourthsPeriod 16)
