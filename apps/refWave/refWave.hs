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
import WaveFile

----------------------------------------------------------------------
-- GLOBALS                                                          --
----------------------------------------------------------------------
outputFile = "out.wav"

numChannels = 1         -- Mono for now
sampleRate = 16000      -- 16 KHZ for now
bitsPerSample = 8       -- One Byte Per Sample right now



-- Temporarily hard coded
secondsToRun = 5;

-- The time period or time between samples in the inverse of the
-- frequency
secondsPerCycle = 1.0 / fromIntegral(sampleRate);



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
-- RETURNS:
--   An integer represented the sample of a sine wave of frequency freq
--   (in hertz) at time t
generator :: Int -> Double -> Int
generator freq t = do
  -- I wasn't expecting this to work but apparently to convert
  -- from an Integer to Double can can just call fromIntegral
  -- I though that only worked when going from one kind of Int
  -- to another kind of Int...
  let cyclesCompleted = fromIntegral (freq) * t
  let radiansCompleted = 2.0 * pi * cyclesCompleted

  -- To keep things simple, we are only using 8 bit samples right now
  -- This means all values are between 0 255.
  -- I presume this means:
  -- 255 =~ 127
  -- 127 =~ 0
  -- 0 =~ -127
  --
  -- I will fix this to use 16 bit later but not right now...

  -- Create values sine values between 0 and 254
  -- Somehow I'm one off here [Should maybe be 255]
  let curIntValue = 127 * (sin radiansCompleted) + 127
  truncate curIntValue



-- PURPOSE:
--    Return simple non compressed 16 Bit Mono PCM data representing a
--    sine wave at the frequency sineFreq
--
-- sineFreq:
--    The frequncy (in hertz) of the desired sine wave
--
-- RETURNS:
--   A ByteString representing a sine 16 Bit Mono PCM "stream" of a
--   sine wave oscillating at the frequence sineFreq
getPCM :: Int -> Double -> BStr.ByteString
getPCM sineFreq curTime = do
   let curVal = generator sineFreq curTime
   -- Not going there right now...
   -- let curValByteString = toLittleEndianByteString curVal

   -- I found out in [3] that you use the encodefunction
   -- to convert an Integer to a ByteString
   let curValByteString = encode (fromIntegral(curVal) :: DW.Word8)
   if curTime > secondsToRun then do
     -- We have meet the specified amount of time.
     -- Return one last value and quit
     -- See [4] to convert Lazy ByteString to String ByteString
     let sByteString = BL.toStrict curValByteString
     sByteString
   else do
     -- Get the current value and append it to the rest
     -- of the values
     let restOfByteString = getPCM sineFreq (curTime + secondsPerCycle)
     let newByteString = BStr.cons (fromIntegral curVal) restOfByteString
     newByteString



main :: IO ()
main = do
  theArgs <- getArgs
  let argsLength = length theArgs
  if argsLength > 1 then do
    putStrLn "Useage: refWave [freqBetween 110-8000 hertz]"
    putStrLn ""
    putStrLn "Examples: "
    putStrLn "./refWave 440"
    putStrLn "./refWave 880"
    putStrLn "./refWave 7040"
  else do
    if argsLength < 1 then do
       hWaveFile <- openFile outputFile WriteMode
       let sineFreq = 440
       let pcmData = getPCM sineFreq 0.0
       let waveWrapperByteString =
             getWaveByteString
             pcmData
             numChannels
             sampleRate
             bitsPerSample
       BStr.hPutStr hWaveFile waveWrapperByteString
       hClose hWaveFile
    else do
       hWaveFile <- openFile outputFile WriteMode
       let sineFreqText = head theArgs

       -- Weird that I have to use "read" to convert the
       -- text value of sineFreqText into an integer but
       -- see [6]
       let sineFreqInt = read sineFreqText
       let pcmData = getPCM sineFreqInt 0.0
       let waveWrapperByteString =
             getWaveByteString
             pcmData
             numChannels
             sampleRate
             bitsPerSample
       BStr.hPutStr hWaveFile waveWrapperByteString
       hClose hWaveFile
