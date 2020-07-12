-- Copyright (c) 2020 Shawn Eary
--
-- Cheese - My attempt to create a cheezy Baroque music music piece
--          using Haskell to generate the music
--
-- When this music piece is done, hopefully it won't give you too much
-- gas...  Remember, never eat more than 3x your maximum daily allowance
-- of Cheez-Its (R)...  No, I wasn't given permission to use the
-- Cheez-It trademark...
--
-- Cheese is Licenced via the MIT License
--
-- COMPILATION STEPS:
-- ghc cheese.hs
--
-- REFERENCES:
-- 1) Install GHC
-- 2) Install GNU Make
-- 3) Change to this directory and run the command
--    make
import Data.Word
import System.IO
import qualified Data.ByteString as BStr
import qualified Generators as Gens
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
genFunction = Gens.sine -- Keep it simple for now
bitsPerSample = 8       -- Keep it simple for now

beatsPerMinute=120.0


-- See [9] for a where these frequencies in Hertz came from
nF4 = 349
nE4 = 330
nD4 = 294
nC4 = 262  -- 261.6256
nG3 = 196  -- 195.9977
-- Hack for a REST
nRe  = 0

-- Tracker Style Notation
bassLine=
   [(nC4,1.0),          (nG3,1.0)          ,(nC4,1.0)          ] ++
   [(nD4,1.0),          (nG3,1.0)          ,(nD4,1.0)          ] ++
   [(nE4,1.0),          (nF4,0.5),(nE4,0.5),(nD4,0.5),(nC4,0.5)] ++
   [(nD4,1.0),          (nG3,1.0)          ,(nD4,1.0)          ] ++

   [(nC4,1.0),          (nG3,1.0)          ,(nC4,1.0)          ] ++
   [(nD4,1.0),          (nG3,1.0)          ,(nD4,1.0)          ] ++
   [(nD4,1.0),          (nG3,1.0)          ,(nD4,1.0)          ] ++
   [(nC4,3.0)                                                  ]

melody=
   [(nE4,0.5),(nRe,0.5),(nE4,0.5),(nRe,0.5),(nE4,0.5),(nRe,0.5)] ++
   [(nRe,3.0)                                                  ] ++
   [(nRe,3.0)                                                  ] ++
   [(nRe,3.0)                                                  ] ++

   [(nRe,3.0)                                                  ] ++
   [(nRe,3.0)                                                  ] ++
   [(nRe,3.0)                                                  ] ++
   [(nRe,3.0)                                                  ]



-- Broke: Not Implemneted
-- This is probably going to go over like a lead ballon.  "mix"
-- the signals by taking their "averge".  Sure man...
-- mix :: [BStr.ByteString] -> BStr.ByteString
-- mix theLines = do
--    let numLines = length theLines
--   (theLines !! 0)



getPCMLine :: [(Int, Double)] -> Double -> BStr.ByteString
getPCMLine [] _ =
   BStr.pack []
getPCMLine (l:ls) bpm = do
   let beats = snd l

--     beats       minute      60 seconds
-- ----------     -------      -----
--                  beats      minute

   let seconds = beats / bpm * 60.0;
   let freq = fst l

   let genFunction =
        if (freq == nRe) then
           Gens.nullGen
        else
           Gens.sine

   let pcmData =
        getPCM
        genFunction
        sampleRate
        freq
        0.0
        bitsPerSample
        seconds
   BStr.append pcmData (getPCMLine ls bpm)



avg :: Word8 -> Word8 -> Word8
avg l r = do
           let halfOfLeft = (div l 2)
           let halfOfRight = (div r 2)
           (halfOfLeft + halfOfRight)



main :: IO ()
main = do
    hWaveFile <- openFile outputFile WriteMode
    let pcmData1 =
         getPCMLine bassLine beatsPerMinute
    let pcmData2 =
         getPCMLine melody beatsPerMinute

    -- https://stackoverflow.com/questions/4776750/what-is-an-idiomatic-way-to-add-lists-in-haskell
    let poorlyMixedResult =
          BStr.pack (BStr.zipWith avg pcmData1 pcmData2)

    let waveWrapperByteString =
         getWaveByteString
         poorlyMixedResult
         numChannels
         sampleRate
         bitsPerSample
    BStr.hPutStr hWaveFile waveWrapperByteString
    hClose hWaveFile
