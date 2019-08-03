-- Copyright (c) 2019 Shawn Eary
--
-- IntConversion Module - Functions and definitions to help with
--                        converting Haskell integers to other
--                        formats
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

module IntConversion
  where

import Data.Binary
import Data.Int
import qualified Data.ByteString as BStr
import qualified Data.ByteString.Lazy as BL
import qualified Data.Int as DI



-- PURPOSE:
--    Take an unsigned integer and convert it to an Unsigned BtyeString
--    in Little Endian format
--
-- Int:
--    An unsigned integer
--
-- RETURNS:
--   A ByteString in Little Endian format representing the specifed
--   unsigned integer named theInt
toUnsignedLEndianByteS :: Int -> BStr.ByteString
toUnsignedLEndianByteS theInt = do
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

  -- See [4] to convert Lazy ByteString to Strict ByteString
  let bigEndian32S = BL.toStrict bigEndian32L
  let littleEndian32 = BStr.reverse bigEndian32S
  littleEndian32



-- PURPOSE:
--    Take an unsigned Int16 and convert it to an Unsigned ByteString
--    in Little Endian format
--    (Same as toUnsignedLEndianByteS only the value is converted to
--     16 Bit Format instead of 32 like the former)
--
-- Int:
--    An unsigned integer
--
-- RETURNS:
--   A ByteString in Little Endian 16 bit format representing the
--   specifed unsigned integer named theInt
toUnsignedLEndianByteS_Int16 :: Int -> BStr.ByteString
toUnsignedLEndianByteS_Int16 theInt = do
  -- Per [2] - "16-bit samples are stored as 2's-complement
  -- signed integers, ranging from -32768 to 32767" and
  -- "The default byte ordering assumed for WAVE"
  -- data files is little-endian."
  -- NOTE!!!! - I using 8 bit samples right now to get
  --            around the two's compliment requirement...

  -- I found out in [3] that you use the encodefunction
  -- to convert an Integer to a ByteString in Big Endian
  -- format
  let bigEndian16L = encode (fromIntegral(theInt) :: Int16)

  -- See [4] to convert Lazy ByteString to Strict ByteString
  let bigEndian16S = BL.toStrict bigEndian16L
  let littleEndian16 = BStr.reverse bigEndian16S
  littleEndian16



-- PURPOSE:
--    Take a signed Int16 and convert it to two's compliment form
--
-- theInt:
--    The integer to conver to two's complement form
--
-- RETURNS:
--   A ByteString of length two representing the supplied integer
--   in two's complement form
toTwosComplimentInt16 :: Int -> BL.ByteString
toTwosComplimentInt16 theInt = do
  -- I found out in [3] that you use the encodefunction
  -- to convert an Integer to a ByteString
  let retValue = encode (fromIntegral(theInt) :: DI.Int16)
  let tempRetValue = BL.reverse retValue
  tempRetValue


