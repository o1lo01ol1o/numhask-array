{-# OPTIONS_GHC -Wall #-}

module Main where

import NumHask.Prelude
import Test.DocTest

main :: IO ()
main = do
  putStrLn ("Array DocTest" :: Text)
  doctest ["src/NumHask/Array.hs"]
  putStrLn ("Example DocTest" :: Text)
  doctest ["src/NumHask/Array/Example.hs"]
  putStrLn ("Slicer DocTest" :: Text)
  doctest ["src/NumHask/Array/Slicer.hs"]
