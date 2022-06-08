-----------------------------------------------------------------------------
--
-- Module      :   the code for session  7 pag 86 and following
-- page 86  isomorphism and coordinates
-- page 91 pictures of maps 
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

module Lib.Page86  
     where

import UniformBase 
import Lib.UsingSets
import Data.List.Extra
import Lib.Page13
import Lib.Page39

-- the objects
type R = Float
    -- deriving (Show, Eq, Ord, Bounded, Enum)
data L = P | Q | R 
    deriving (Show, Eq, Ord, Bounded, Enum)

-- -- the maps 
plot :: R -> L 
plot = invOf coord
-- plot = fromList ([(0.0,P)])

coord :: L -> R
coord = fromList [(P,0.0),(Q,3.5),(R,-4.3)]
-- coord P = 0.0
-- coord R = -4.3
-- coord Q = 3.5


data SetA = A1 | A2 | A3 | A4 | A5 | A6 | A7 | A8 | A9 | A10 | A11 | A12 
    deriving  (Show, Eq, Bounded, Enum, Ord)
-- data SetB = Feather | Stone | Flower  deriving (Show, Eq, Ord, Bounded, Enum)
 
-- f :: SetA -> SetB
-- f (Mother) = Feather
-- f (Father) = Stone
-- f (Child) = Flower

f = fromList [(A1,A3), (A2,A3), (A3,A3), (A4,A6), (A5,A6), (A6,A6), (A7,A11),(A8,A11),(A9,A11), (A10,A11), (A11,A11), (A12,A12)]

-- section - f must be surjective (epimorphism)
-- retraction - f must be injective (monomorpism)

-- invf f = if bijective f then 
--                 else errorT ["not bijective"]

-- stackA = groupSort (pfeile f)
-- stacking f = groupSort (pfeile f)
-- sorting f = groupSort (invPfeil . pfeile $ f)

-- naming f = nub . map snd . pfeile $ f 

page86:: IO ()
page86= do
    putIOwords ["pfeile coord ", showT (pfeile coord)]
    -- putIOwords ["naming plot ", showT (naming plot)]
    putIOwords ["naming coord ", showT (naming coord)]
  
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






