-----------------------------------------------------------------------------
--
-- Module      :   the code for aticle 1 page 13
-- page 13: sets, maps, composition
--------------------------------------------------------------------------- 
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Lib.UsingSets
     where

import UniformBase 
import Lib.Page13

-- instance Enum Set13 where   - is derived 
-- gives toEnum :: Int -> Set13 and 
-- fromEnum :: Set13 -> Int 

toEnum13:: Int -> Set13 
toEnum13 = toEnum

toEnum14:: Int -> Set14 
toEnum14 = toEnum

f15' :: Int -> Int 
f15' 0 = 0
f15' 1 = 3 
f15' 2 = 3 

f15'' :: Set13 -> Set14
f15'' = toEnum14 . f15' . fromEnum 

using:: IO () 
using = do 
    putIOwords ["fromEnum john mary", showT [fromEnum John, fromEnum Mary]]
    putIOwords ["toEnum 0,1", showT [toEnum13 0, toEnum13 1]]
    putIOwords ["f15'' ", showT (map f15'' [John, Mary, Sam])]

data I3 a = I3 a deriving (Eq, Ord, Show, Read)
unI3 (I3 a) = a 
toI3 :: Set13 -> I3 Int
toI3 = I3 . fromEnum 
fromI3 :: I3 Int -> Set13 
fromI3 (I3 a) = toEnum a 

instance Functor I3 where 
    fmap   f (I3 a) = I3 (f a) 


using2:: IO () 
using2 = do 
    putIOwords ["toI3", showT [map toI3 [John, Mary, Sam]]]
    putIOwords ["fromI3", showT [map fromI3[I3 0, I3 1, I3 2]]]
    putIOwords ["functor", showT (fmap id (I3 1))]