-- Copyright (c) 2019 Shawn Eary
--
-- utHelpers Module - Helper functions for Unit Tests
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

module UnitTestHelpers
  where



verify :: String -> Int -> Int -> IO ()
verify label expected actual = do
  let initialOutStr = label ++ ": "
  putStr initialOutStr
  if expected == actual then
    putStrLn "Pass"
  else do
    let restOfOutString =
         "Fail - expected=" ++ show(expected) ++ " actual=" ++ show(actual)
    putStrLn restOfOutString
