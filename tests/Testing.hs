-----------------------------------------------------------------------------
--
-- Module      :   for automatic test
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings     #-}

-- module Spec     where      -- must have Main (main) or Main where

import           Test.Framework
import  {-@ HTF_TESTS @-}         Lib.Lawvere_test

main :: IO 
main =  do
    putStrLn "Lib.Testing.hs for Lawvere_test:\n"
    r <- htfMain htf_importedTests
    putStrLn ("other tests t:\n" ++ show r)
    return ()


