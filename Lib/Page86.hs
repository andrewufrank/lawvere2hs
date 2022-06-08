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

f86 = fromList [(A1,A3), (A2,A3), (A3,A3), (A4,A6), (A5,A6), (A6,A6), (A7,A11),(A8,A11),(A9,A11), (A10,A11), (A11,A11), (A12,A12)]

-- section - f must be surjective (epimorphism)
-- retraction - f must be injective (monomorpism)

-- invf f = if bijective f then 
--                 else errorT ["not bijective"]

-- stackA = groupSort (pfeile f)
-- stacking f = groupSort (pfeile f)
-- sorting f = groupSort (invPfeil . pfeile $ f)

-- naming f = nub . map snd . pfeile $ f 


fixedPoints = map fst . filter fstEqsnd . pfeile  
fstEqsnd (a,b) = a == b

page86:: IO ()
page86= do
    putIOwords ["\npage 86"]
    putIOwords ["pfeile f ", showT (pfeile f86)]
    -- putIOwords ["naming plot ", showT (naming plot)]
    putIOwords ["naming f ", showT (naming f86)]
    putIOwords ["stacking f ", showT (stacking f86)]
    putIOwords ["fixedPoints f ", showT (fixedPoints f86)]
  
    return ()

