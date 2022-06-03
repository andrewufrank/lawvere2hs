-----------------------------------------------------------------------------
--
-- Module      :   the code for aticle II page 39
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

module Lib.Page39  
     where

import UniformBase 
import Lib.UsingSets
import Data.List.Extra
import Lib.Page13

-- the objects
data SetA = Mother | Father | Child deriving (Show, Eq, Bounded, Enum, Ord)
data SetB = Feather | Stone | Flower  deriving (Show, Eq, Ord, Bounded, Enum)
 
-- the maps 
f :: SetA -> SetB
f (Mother) = Feather
f (Father) = Stone
f (Child) = Flower

invOf f = fromList (invPfeil $ pfeile f)

-- section - f must be surjective (epimorphism)
-- retraction - f must be injective (monomorpism)

-- invf f = if bijective f then 
--                 else errorT ["not bijective"]

stackA = groupSort (pfeile f)
stacking f = groupSort (pfeile f)
sorting f = groupSort (invPfeil . pfeile $ f)

naming f = nub . map snd . pfeile $ f 

page39= do
    putIOwords ["fg13", showT (f Father)]
    putIOwords ["injective f", showT (injective f)]
    putIOwords ["surjective f", showT (surjective f)]
    putIOwords ["bijective f", showT (bijective f)]
    putIOwords ["f-1 feather", showT (invOf f Feather)]
    putIOwords ["groupSort pfeile f", showT . stacking $ f]
    putIOwords ["groupSort pfeile f-1", showT.stacking  $  f]
    putIOwords ["sorting f", showT.sorting  $  f]
    putIOwords ["sorting f13", showT.sorting  $  f13]
    putIOwords ["stacking f13", showT.stacking  $  f13]
    putIOwords ["naming f13", showT.naming  $  f13]


    -- putIOwords ["pag19 john . f13 prefers:", showT (f13 . john $ One)]
    -- putStrLn "Lib.Page13 done"
    return ()


-- g :: SetB -> SetA
-- g13(John) = Mary
-- g13(Sam) = Mary
-- g13(Mary) = John 

-- h15 :: Set14 -> Set15 
-- h15(Eggs) = D1 
-- h15(Coffee)= D2 
-- h15(Toast) = D4 
-- h15(Oatmeal) = D5

-- id13:: Set13 -> Set13 -- identity map 
-- id13 a = a 
-- id14:: Set14 -> Set14
-- id14 a = a 

-- fg13:: Set13 -> Set14 -- f  dot g
-- fg13 = f13 . g13

-- -- define a category with this example 

-- -- the objects Set13 and Set14 
-- -- the maps are f13 and g13
-- -- the identity maps: id13, id14

-- -- the identity law:   id13 . f = f 
-- -- the associate law: g13 f13 h15 

-- -- points 
-- data Set1 = One deriving (Show, Eq, Bounded, Enum)
-- john, mary, sam :: Set1 -> Set13
-- john One = John 
-- mary One = Mary
-- sam One = Sam 

page39:: IO ()





