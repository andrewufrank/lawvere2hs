 -----------------------------------------------------------------------------
--
-- Module      :  Lawvere
-- Copyright   :
--
-- | the test for catergories

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings
    , RecordWildCards     #-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -w #-}

module Lib.Lawvere_test
    where

import Lib.Page13 

import UniformBase
 

import           Algebra.Laws             as Law
import           Test.Framework
import           Test.Invariant           as Rule  
import Test.QuickCheck --  (arbitraryBoundedEnum)

instance Arbitrary Set13 where
    arbitrary = arbitraryBoundedEnum
instance Arbitrary Set14 where
    arbitrary = arbitraryBoundedEnum


prop_id13:: Set13 -> Bool 
prop_id13 a = id13 a == a 

prop_id14:: Set14 -> Bool 
prop_id14 a = id14 a == a 

-- from Algebra.Laws:  the identity function has a different signature. 
-- why?
-- prop_id13 :: Set13 -> Set13 -> Bool 
-- prop_id13 a b = identity id13 a b
-- identity :: Eq a => (a -> a -> a) -> a -> a -> Bool
-- identity op x y  =  leftIdentity op x y &&  rightIdentity op x y

-- rightIdentity :: Eq a => (a -> b -> a) -> b -> a -> Bool
-- rightIdentity op y x  =  x `op` y == x