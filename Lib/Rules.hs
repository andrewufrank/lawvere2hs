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

import Data.List (nub, group, sort)
import Data.List.Extra (groupSort )

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
    codomain1 =  dots `asTypeOf` image1

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

stacking :: (Ord v, Enum v, Bounded v, Bounded k, Enum k) => (k -> v) -> [(v, [k])]
stacking ff = if surjective ff then -- groupSort (invPfeil . toPfeile $ ff)
        groupSort $ map  (\a -> (ff a, a)) dots
            else []

naming :: (Bounded a, Enum a, Eq b) => (a -> b)-> [b]
naming ff = nub . map ff $ dots

countSections :: (Bounded a, Enum a, Ord b, Enum b, Bounded b) =>
             (a -> b) -> Int
countSections f -- = product . map length . map snd . stacking
        = if surjective f 
            then 
                product . map length .  stacking $ f
            else 0

    -- construct all sections - see page 93
allSections :: (Show v, Ord v, Enum v, Bounded v, Bounded k, Enum k) 
    => (k -> v) -> [(v -> k)]
allSections ff = if surjective ff 
        then map fromPfeile . seq1 $ ff
        else []
    where
    c1 :: (a, [b]) -> [(a, b)]
    c1 (a,[]) = []
    c1 (a, (b:bs)) = (a,b) : c1 (a,bs)

    -- - create stack
    sta1 :: (Ord v, Enum v, Bounded v, Bounded k, Enum k) => (k -> v) -> [(v, [k])]
    sta1 ff = groupSort $ map  (\a -> (ff a, a)) dots
    -- expand
    exp1 :: (Ord v, Enum v, Bounded v, Bounded k, Enum k) =>  (k -> v) -> [[(v,k)]]
    exp1 ff = map c1 (sta1 ff)
    -- sequence all allSections, gives the functions f which are sections to g 
    seq1 :: (Ord v, Enum v, Bounded v, Bounded k, Enum k) =>  (k -> v) -> [[(v,k)]]
    seq1 ff = sequence . exp1 $ ff 


-- testSection g . f = id
testSection :: (Ord v, Enum v, Bounded v, Bounded k, Enum k) =>
        (k -> v) -> (v -> k) -> Bool
testSection g f = and $ zipWith (==) (map (g.f) dots) dots


-- retraction - f must be injective (monomorphism)

-- invFunct f = if bijective f then 
--                 else errorT ["not bijective"]

