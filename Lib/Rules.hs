-----------------------------------------------------------------------------
--
-- Module      :   summary of the rules as code
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Rules where

import Data.List (nub)
import Data.Tuple (swap)
import UniformBase


injective :: (Bounded a, Enum a, Eq b, Enum b, Bounded b) => (a -> b) -> Bool
injective f = length image1 == length (nub (dots `asTypeOf` image1))
  where
    image1 = fmap f dots

surjective :: (Bounded a, Enum a, Eq b, Enum b, Bounded b) => (a -> b) -> Bool
surjective f = length codomain1 == length (nub image1)
  where
    image1 = fmap f dots
    codomain1 = fmap f dots `asTypeOf` image1

bijective :: (Bounded a, Enum a, Eq b, Enum b, Bounded b) => (a -> b) -> Bool
bijective f = injective f && surjective f


toPfeile :: (Bounded a, Enum a) => (a->b) -> [(a,b)]
-- the arrows in the internal diagram 
toPfeile f = map (toPfeil f) (dots)
    where
    toPfeil :: (a -> b) -> a -> (a, b)
    toPfeil f1 a = (a, f1 a)

dots :: (Bounded a, Enum a) => [a]
dots = [minBound .. maxBound]

invPfeil :: [(a, b)] -> [(b, a)]
invPfeil = map swap

fromPfeile :: (Eq a, Show a) => [(a, b)] -> a -> b
fromPfeile ((k, v) : kvs) k1 = if k == k1 then v else fromPfeile kvs k1
fromPfeile [] k = errorT ["not a function for", showT k, "- error in inversion?"]

invFunct :: (Eq a, Show a, Bounded p, Enum p) => (p -> a) -> a -> p
invFunct f = fromPfeile (invPfeil $ toPfeile f)

-- section - f must be surjective (epimorphism) 
--                  one of each (from stack)
-- retraction - f must be injective (monomorpism)

-- invFunct f = if bijective f then 
--                 else errorT ["not bijective"]

