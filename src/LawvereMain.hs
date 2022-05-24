-----------------------------------------------------------------------------
--
-- Module      :   a test  
------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main     where      -- must have Main (main) or Main where

 
-- import           Lib.DirTree
import  Lib.Page13
import Lib.UsingSets
-- import           Lib.OpenClass

main :: IO ()
main =  do  -- with tests in other modules
    page13
    using
--    openMain

