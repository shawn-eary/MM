-- Copyright (c) 2019 Shawn Eary
--
-- RefWave - Generates a WAV File of a reference sine at the specified
--           pitch in hertz
--
-- RefWave is Licenced via the MIT License (See LICENSE File)

-- COMPILATION STEPS:
-- ghc refWave.hs
--
-- REFERENCES:
-- [ Please Refer to REFERENCES.txt ]

import System.Environment
import System.IO
import qualified Data.ByteString as BStr
import Data.Binary
import Data.Int
import qualified Data.ByteString.Lazy as BL
import qualified Data.Word as DW

----------------------------------------------------------------------
-- GLOBALS                                                          --
----------------------------------------------------------------------
outputFile = "out.wav"

-- 16 KHZ
sampleRate = 16000;
-- TODO: This needs to be computed from sampleRate but it is hard coded
--       right now
sampleRateW8 = [128, 62, 0, 0];

-- Temporarily hard coded
secondsToRun = 10;

-- The time period or time between samples in the inverse of the
-- frequency
secondsPerCycle = 1.0 / sampleRate;



toLittleEndianByteString :: Int -> BStr.ByteString
toLittleEndianByteString theInt = do
  -- Per [2] - "16-bit samples are stored as 2's-complement
  -- signed integers, ranging from -32768 to 32767" and
  -- "The default byte ordering assumed for WAVE"
  -- data files is little-endian."
  -- NOTE!!!! - I using 8 bit samples right now to get
  --            around the two's compliment requirement...

  -- I found out in [3] that you use the encodefunction
  -- to convert an Integer to a ByteString in Big Endian
  -- format
  let bigEndian32L = encode (fromIntegral(theInt) :: Int32)

  -- See [4] to convert Lazy BytString to Strict ByteString
  let bigEndian32S = BL.toStrict bigEndian32L
  let littleEndian32 = BStr.reverse bigEndian32S
  littleEndian32



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
   let genFreq = sineFreq
   
   -- BEGIN Just for Giggles
   -- Just for fun, I am attempting to apply frequency modulation to
   -- the generator
   let fmFreq = 200;   -- In hertz
   let fmScale = 2.0;  -- Scale of 1.0 means +/- one octave from
                       -- base frequence sineFreq
   let fmCyclesCompleted = fromIntegral (fmFreq) * curTime
   let fmRadiansCompleted = 2.0 * pi * fmCyclesCompleted
   let intGenFreq = genFreq + truncate(fmScale * (sin fmRadiansCompleted))
   -- END   Just for Giggles
   
   let curVal = generator intGenFreq curTime
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



-- PURPOSE:
--   Uses [1] and [2] to geneate a wave file wrapper arond the supplied
--   PCM data
--
-- pcmData:
--   The 16 Bit Integer PCM data to put into a WAV format
--
-- RETURNS:
--   A ByteString representing a WAV file containing pcmData
getWaveWrapper :: BStr.ByteString -> BStr.ByteString
getWaveWrapper pcmData = do
  -- Per [2] - "The default byte ordering assumed for WAVE"
  -- data files is little-endian.".  This is different when
  -- string constants such as RIFF and WAVE are used though
  -- In those cases big-endian is used

  -- Not sure what this is exactly but per *both* [1] and [2], it
  -- always seems to be 16.  [2] Seems to speifically indicate
  -- that it should be 16 for PCM which is what we will be using
  -- in this program
  let subChunkOneSize = [16, 0, 0, 0]

  -- See [2] for an examplation of this.
  -- NumChannels * BitsPerSample / 8
  -- For now, it is hardcoded
  let blockAlign = [1, 0]

  let subChunkTwoSize = BStr.length pcmData
  let subChunkTwoSizeByteS = toLittleEndianByteString subChunkTwoSize

  -- TODO: FileSize needs to be computed
  -- Per [2] - "36 + SubChunk2Size"
  let fileSize = 36 + subChunkTwoSize
  let fileSizeByteS = toLittleEndianByteString fileSize

  -- See [2] for an explanation of this, but it's hardcoded right now
  let byteRateW8 = [128, 62, 0, 0]

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
  let tbs8 = BStr.append tbs7 (BStr.pack sampleRateW8)
  let tbs9 = BStr.append tbs8 (BStr.pack byteRateW8)
  let tbs10 = BStr.append tbs9 (BStr.pack blockAlign)
  -- 8 bits per sample hardcoded
  let tbs11 = BStr.append tbs10 (BStr.pack [8,0])
  -- Text data
  let tbs12 = BStr.append tbs11 (BStr.pack [100, 97, 116, 97])
  let tbs13 = BStr.append tbs12 subChunkTwoSizeByteS
  let tbs14 = BStr.append tbs13 pcmData
  tbs14



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
       let waveWrapperByteString = getWaveWrapper pcmData
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
       let waveWrapperByteString = getWaveWrapper pcmData
       BStr.hPutStr hWaveFile waveWrapperByteString
       hClose hWaveFile
