-- Copyright (c) 2019 Shawn Eary
--
-- AudioFormat Module - Functions and definitions that are useful when
--                      dealing with audio formats such as
--                      the Microsoft/IBM Wave Standard [H] underlying
--                      respesentations such as Pules-code Modulation
--                      (PCM) [I]
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

module AudioFormat
  where

import qualified Data.ByteString as BStr
import qualified IntConversion as IntC
import qualified Generators as Gens
import qualified Data.Word as DW
import qualified Data.Int as DI
import qualified Data.ByteString.Lazy as BL
import Data.Binary



-- PURPOSE:
--   Uses [1] and [2] to geneate a wave file wrapper arond the supplied
--   PCM data
--
-- pcmData:
--   The PCM data to put into a WAV format
--
-- numChannels:
--   Normally this would be 1 for MONO or 2 for STEREO
--   I supposed it get could go higher for surround sound type
--   situations but I don't know
--   (WARNING: Right now only 1 channel is supported)
--
-- sampleRate:
--   The frequency of the PCM data in hertz
--   (WARNING: Right now I'm uncertain if sample rates other than
--    16000 will work)
--
-- bitsPerSample:
--   The number of bits in the sample.  This would normally be 8 or 16
--   but can be other options in some circumstances
--   (WARNING: Only 8 bits per sample is supported at this current
--    moment)
--
-- RETURNS:
--   A ByteString representing a WAV file containing pcmData
getWaveByteString :: BStr.ByteString -> Int -> Int -> Int -> BStr.ByteString
getWaveByteString pcmData numChannels sampleRate bitsPerSample = do
  -- Per [2],
  -- byteRate = sampleRate * numChannels * bitsPerSample / 8
  -- In addition to picking a resonable sample rate, you should
  -- probably also pick the product of sampleRate, numChannels
  -- and bitsPerSample so it is divisible by 8
  -- Also, in Haskell, Integer division is done via the quot
  -- function [7]
  let byteRate = quot (sampleRate * numChannels * bitsPerSample) 8

  -- Per [2] - "The default byte ordering assumed for WAVE"
  -- data files is little-endian.".  This is different when
  -- string constants such as RIFF and WAVE are used though
  -- In those cases big-endian is used

  -- Not sure what this is exactly but per *both* [1] and [2], it
  -- always seems to be 16.  [2] Seems to speifically indicate
  -- that it should be 16 for PCM which is what we will be using
  -- in this program
  let subChunkOneSize = [16, 0, 0, 0]

  -- See [2] for an examplation of this
  -- NumChannels * BitsPerSample / 8
  -- Integer division is done in Haskell via the quot
  -- function [7]
  let blockAlign = quot (numChannels * bitsPerSample) 8
  let blockAlignByteStr =
        IntC.toUnsignedLEndianByteS_Int16 blockAlign

  let subChunkTwoSize = BStr.length pcmData
  let subChunkTwoSizeByteS =
        IntC.toUnsignedLEndianByteS subChunkTwoSize

  -- TODO: FileSize needs to be computed
  -- Per [2] - "36 + SubChunk2Size"
  let fileSize = 36 + subChunkTwoSize
  let fileSizeByteS =
        IntC.toUnsignedLEndianByteS fileSize

  let byteRateByteStr =
        IntC.toUnsignedLEndianByteS byteRate

  let sampleRateByteStr =
        IntC.toUnsignedLEndianByteS sampleRate

  let theByteString = BStr.pack [82, 73, 70, 70]  -- Text RIFF
  let tbs2 = BStr.append theByteString fileSizeByteS
  -- Text WAVE
  let tbs3 = BStr.append tbs2 (BStr.pack [87, 65, 86, 69])
  -- Text "fmt " (Without Quotes)
  let tbs4 = BStr.append tbs3 (BStr.pack [102, 109, 116, 32])
  let tbs5 = BStr.append tbs4 (BStr.pack subChunkOneSize)
  -- Choose PCM
  let tbs6 = BStr.append tbs5 (BStr.pack [1, 0])
  -- Just one channel for now...
  let tbs7 = BStr.append tbs6 (BStr.pack [1, 0])
  let tbs8 = BStr.append tbs7 sampleRateByteStr
  let tbs9 = BStr.append tbs8 byteRateByteStr
  let tbs10 = BStr.append tbs9 blockAlignByteStr
  -- 8 bits per sample hardcoded
  let bitsPerSampleByteStr =
        IntC.toUnsignedLEndianByteS_Int16 bitsPerSample
  let tbs11 = BStr.append tbs10 bitsPerSampleByteStr
  -- Text data
  let tbs12 = BStr.append tbs11 (BStr.pack [100, 97, 116, 97])
  let tbs13 = BStr.append tbs12 subChunkTwoSizeByteS
  let tbs14 = BStr.append tbs13 pcmData
  tbs14



-- PURPOSE:
--    Return simple non compressed 16 Bit Mono PCM data representing a
--    sine wave at the frequency sineFreq
--
-- sampleRate:
--    The frequency of the sample rate.  Common values might be
--    8000, 16000, 24000 or 48000.  It needs to be twice the highest
--    frequency of the input signal.  See limitations section of [I]
--
-- sineFreq:
--    The frequncy (in hertz) of the desired sine wave
--
-- curTime:
--    The currentTime is "iterated" until the end and all samples have
--    been collected.  curTime is fed to the generator
--
-- bitsPerSample:
--    Right now, only 8 bit and 16 bit are supported
--
-- secondsToRun:
--    The number of seconds to get PCM Data for
--
-- RETURNS:
--   A ByteString representing Mono PCM "stream" of a
--   sine wave oscillating at the frequency sineFreq
getPCM :: Int -> Int -> Double -> Int -> Double -> BStr.ByteString
getPCM sampleRate sineFreq curTime bitsPerSample secondsToRun = do
   if curTime >= secondsToRun then
     -- We have meet the specified amount of time.  Quit.
     BStr.empty
   else do
     -- The time period or time between samples in the inverse of the
     -- frequency
     let secondsPerCycle = 1.0 / fromIntegral(sampleRate)

     let curVal = Gens.sine sineFreq curTime bitsPerSample

     -- I found out in [3] that you use the encodefunction
     -- to convert an Integer to a ByteString
     let curValByteString =
          if bitsPerSample == 8 then
            encode (fromIntegral(curVal) :: DW.Word8)
          else
            encode (fromIntegral(curVal) :: DI.Int16)

     -- Get the current value and append it to the rest
     -- of the values
     let restOfByteString =
            getPCM sampleRate sineFreq (curTime + secondsPerCycle) bitsPerSample secondsToRun
     -- I found out in [3] that you use the encodefunction
     -- to convert an Integer to a ByteString
     let curValByteString =
          if bitsPerSample == 8 then
            encode (fromIntegral(curVal) :: DW.Word8)
          else do
            let twosCompliment = IntC.toTwosComplimentInt16 curVal
            twosCompliment
     let curValByteStringStrict = BL.toStrict curValByteString
     let newByteString =
          BStr.append curValByteStringStrict restOfByteString
     newByteString
