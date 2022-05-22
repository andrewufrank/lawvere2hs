-----------------------------------------------------------------------------
--
-- Module      :   for automatic test
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings     #-}

-- module Spec     where      -- must have Main (main) or Main where

--import Uniform.Strings
import           Test.Framework
-- import Uniform.Strings.Conversion_test
    -- ( htf_Uniform_Strings_Conversion_test_thisModulesTests )
import  {-@ HTF_TESTS @-}         Lib.Lawvere_test
-- import  {-@ HTF_TESTS @-}         Uniform.Strings.Infix_test
-- import   {-@ HTF_TESTS @-}        Uniform.Strings.Utilities_test

main =  do
    putStrLn "Lib.Testing.hs for Lawvere_test:\n"
    -- r1 <- htfMain htf_thisModulesTests  -- if local tests
    -- putStrLn ("tree tagger test:\n" ++ show r1)
    r <- htfMain htf_importedTests
    putStrLn ("other tests t:\n" ++ show r)
    return ()


-- examples
