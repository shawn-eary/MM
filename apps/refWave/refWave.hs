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
import AudioFormat

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



-- Number of seconds to get PCM data for
-- This is hardcoded at the moment instead of retreived from the
-- command line
secondsToRun = 5.0;



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
       let pcmData =
            getPCM sampleRate sineFreq 0.0 bitsPerSample secondsToRun
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

       let pcmData =
            getPCM sampleRate sineFreqInt 0.0 bitsPerSample secondsToRun
       let waveWrapperByteString =
             getWaveByteString
             pcmData
             numChannels
             sampleRate
             bitsPerSample
       BStr.hPutStr hWaveFile waveWrapperByteString
       hClose hWaveFile
