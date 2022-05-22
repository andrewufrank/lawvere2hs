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

module Lib.Page13   
     where

import UniformBase 

-- the objects
data Set13 = John | Mary | Sam deriving (Show, Eq, Bounded, Enum)
data Set14 = Eggs | Toast | Oatmeal | Coffee deriving (Show, Eq, Bounded, Enum)
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
id14:: Set14 -> Set14
id14 a = a 

fg13:: Set13 -> Set14 -- f  dot g
fg13 = f13 . g13

-- define a category with this example 

-- the objects Set13 and Set14 
-- the maps are f13 and g13
-- the identity maps: id13, id14

-- the identity law:   id13 . f = f 
-- the associate law: 

page13 :: IO ()
page13= do
    putStrLn "Lib.Page13 start"
    putIOwords ["fg13", showT (fg13 John)]
    putStrLn "Lib.Page13 done"
    return ()





