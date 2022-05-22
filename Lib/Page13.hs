-----------------------------------------------------------------------------
--
-- Module      :    a sub 
--------------------------------------------------------------------------- 
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE DeriveAnyClass     #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Lib.Page13   
     where

import UniformBase 
-- import Uniform.Strings
-- page 13: sets, maps, composition
import Test.QuickCheck --  (arbitraryBoundedEnum)

-- the objects
data Set13 = John | Mary | Sam deriving (Show, Eq, Bounded, Enum)
data Set14 = Eggs | Toast | Oatmeal | Coffee deriving (Show, Eq)
-- the maps 
f13 :: Set13 -> Set14
f13 (John) = Eggs
f13 (Mary) = Coffee
f13 (Sam) = Coffee

g13 :: Set13 -> Set13
g13(John) = Mary
g13(Sam) = Mary
g13(Mary) = John 

id13:: Set13 -> Set13 -- identity map 
id13 a = a 

fg13:: Set13 -> Set14 -- f  dot g
fg13 = f13 . g13



page13 :: IO ()
page13= do
    putStrLn "Lib.Page13 start"
    putIOwords ["fg13", showT (fg13 John)]
    putStrLn "Lib.Page13 done"
    return ()





