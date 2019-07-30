-- Copyright (c) 2019 Shawn Eary
--
-- RefWave - Generates a WAV File of a reference sine at the specified
--           pitch in hertz
--
-- RefWave is Licenced via the MIT License
--
-- COMPILATION STEPS:
-- ghc refWave.hs
--
-- REFERENCES:
-- 1) Install GHC
-- 2) Install GNU Make
-- 3) Change to this directory and run the command
--    make
import System.Environment
import System.IO
import qualified Data.ByteString as BStr
import Data.Binary
import qualified Data.ByteString.Lazy as BL
import qualified Data.Word as DW
import qualified Data.Int as DI
import WaveFile
import qualified Generators as Gens

----------------------------------------------------------------------
-- GLOBALS                                                          --
----------------------------------------------------------------------
outputFile = "out.wav"

numChannels = 1         -- Mono for now
sampleRate = 48000      -- Sampling Rate in Hertz
                        -- 48000 is *LUDICROUS* overkill for a simple
                        -- sine wave within the knowable notes on a piano
                        -- keyboard but it is a common audio standard.  In
                        -- reality 8000 HZ would probably do just find since
                        -- a sine wave does not have artifacts, but 48000
                        -- is still used hre since it is a common sampling
                        -- rate for pro audio tools



-- Temporarily hard coded
secondsToRun = 5;

-- The time period or time between samples in the inverse of the
-- frequency
secondsPerCycle = 1.0 / fromIntegral(sampleRate);



-- PURPOSE:
--    Return simple non compressed 16 Bit Mono PCM data representing a
--    sine wave at the frequency sineFreq
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
-- RETURNS:
--   A ByteString representing Mono PCM "stream" of a
--   sine wave oscillating at the frequency sineFreq
getPCM :: Int -> Double -> Int -> BStr.ByteString
getPCM sineFreq curTime bitsPerSample = do
   let curVal = Gens.sine sineFreq curTime bitsPerSample

   -- I found out in [3] that you use the encodefunction
   -- to convert an Integer to a ByteString
   let curValByteString =
         if bitsPerSample == 8 then
           encode (fromIntegral(curVal) :: DW.Word8)
         else
           encode (fromIntegral(curVal) :: DI.Int16)
   if curTime > secondsToRun then do
     -- We have meet the specified amount of time.
     -- Return one last value and quit
     -- See [4] to convert Lazy ByteString to String ByteString
     let sByteString = BL.toStrict curValByteString
     sByteString
   else do
     -- Get the current value and append it to the rest
     -- of the values
     let restOfByteString =
            getPCM sineFreq (curTime + secondsPerCycle) bitsPerSample
     -- I found out in [3] that you use the encodefunction
     -- to convert an Integer to a ByteString
     let curValByteString =
          if bitsPerSample == 8 then
            encode (fromIntegral(curVal) :: DW.Word8)
          else
            encode (fromIntegral(curVal) :: DI.Int16)
     let curValByteStringStrict = BL.toStrict curValByteString
     let newByteString =
          BStr.append curValByteStringStrict restOfByteString
     newByteString



main :: IO ()
main = do
  theArgs <- getArgs
  let argsLength = length theArgs
  if argsLength > 2 then do
    putStrLn
       "Useage: refWave [freqBetween 110-8000 hertz] [bitsPerSample 8 or 16]"
    putStrLn ""
    putStrLn "Examples: "
    putStrLn "./refWave 440"
    putStrLn "./refWave 880 8"
    putStrLn "./refWave 7040 16"
  else do
    if argsLength < 1 then do
       hWaveFile <- openFile outputFile WriteMode
       let sineFreq = 440
       let bitsPerSample = 8
       let pcmData = getPCM sineFreq 0.0 bitsPerSample
       let waveWrapperByteString =
             getWaveByteString
             pcmData
             numChannels
             sampleRate
             bitsPerSample
       BStr.hPutStr hWaveFile waveWrapperByteString
       hClose hWaveFile
    else do
       let bitsPerSample =
            if argsLength < 2 then
               8
            else do
               -- [6] to read an integer from a string
               read (theArgs !! 1)
       hWaveFile <- openFile outputFile WriteMode
       let sineFreqText = head theArgs

       -- Weird that I have to use "read" to convert the
       -- text value of sineFreqText into an integer but
       -- see [6]
       let sineFreqInt = read sineFreqText

       let debugStr =
            "DEBUG: freq=" ++ (show sineFreqInt) ++
            " bits=" ++ (show bitsPerSample)
       putStrLn debugStr

       let pcmData = getPCM sineFreqInt 0.0 bitsPerSample
       let waveWrapperByteString =
             getWaveByteString
             pcmData
             numChannels
             sampleRate
             bitsPerSample
       BStr.hPutStr hWaveFile waveWrapperByteString
       hClose hWaveFile
