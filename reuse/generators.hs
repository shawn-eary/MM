-- Copyright (c) 2019 Shawn Eary
--
-- Generators Module - Functions to generate wave/sound data
--
-- This Module is Licenced via the MIT License
--
-- COMPILATION STEPS:
-- 1) Install GHC
-- 2) Install GNU Make
-- 3) Change to this directory and run the command
--    make
-- [NOTE: In general, you shouldn't need to complile this module
--        directly.  If you compile an app that uses this module,
--        this this module should compile "automatically" ]
--
-- REFERENCES:
-- [ Please Refer to REFERENCES.txt in root Folder]

module Generators
  where

-- PURPOSE:
--    Gets the sample value of a sine wave of frequency freq
--    at time t
--
-- freq:
--    The frequncy (in hertz) of the desired sine wave
--
-- t:
--    The time at which to collect the sample
--
-- bitsPerSample:
--    The resolution of the sample.  At the moment, 8 and 16 are
--    the only supported values.
--
-- RETURNS:
--   An integer represented the sample of a sine wave of frequency freq
--   (in hertz) at time t
sine :: Int -> Double -> Int -> Int
sine freq t bitsPerSample = do
  -- I wasn't expecting this to work but apparently to convert
  -- from an Integer to Double can can just call fromIntegral
  -- I though that only worked when going from one kind of Int
  -- to another kind of Int...
  let cyclesCompleted = fromIntegral (freq) * t
  let radiansCompleted = 2.0 * pi * cyclesCompleted

  let curIntValue =
        if (bitsPerSample == 8) then
          -- Somehow I'm one off here [Should maybe be 255]
          -- 8 Bit Wave Samples are unsigned from 0-255 [2]
          127 * (sin radiansCompleted) + 127
        else
          -- 16 Bit Wave Samples are two's compliment signed
          -- from -32768 to 32767 [2].  I'm being lazy in
          -- multiplying by a slightly smaller numer 32760 to
          -- give myself some wiggle room in case I mess the
          -- calucation up a bit
          32760 * (sin radiansCompleted)
  truncate curIntValue
