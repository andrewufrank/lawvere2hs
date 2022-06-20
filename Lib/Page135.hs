-----------------------------------------------------------------------------
--
-- Module      :   the code for artcke III pag 135 and following
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

module Lib.Page135
     where

-- change prelude for constrainded-categories
import Prelude ()
import Control.Category.Constrained.Prelude
import qualified Control.Category.Hask as Hask
import Control.Monad.Constrained
-- end 

import UniformBase 
import Lib.Rules
import Data.List.Extra
import Lib.Page13
import Lib.Page39

data SetX = X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8 | X9   
    deriving  (Show, Eq, Bounded, Enum, Ord)
    --  diagram page 137
f137 :: SetX -> SetX
f137 = fromPfeile [(X1,X2), (X2,X3), (X3,X4), (X4, X5), (X5,X3), (X6,X7), (X7,X5), (X8,X9),(X9,X9)]


data SetY a = SetY a 
    deriving  (Show, Eq, Bounded, Ord)  -- not Enum

instance Hask.Functor SetY  where
    fmap ff (SetY a) = SetY (ff a)
--     -- fmap . f137 = f137' . fmap

f137' :: SetY SetX -> SetY SetX
f137' = fmap f137 

fp137' :: (Enum (SetY SetX)) => [SetY SetX]
fp137' = fixedPoints f137'

page135 :: IO ()
page135 = do
    putIOwords ["\npage 135"]
    putIOwords ["toPfeile f ", showT (toPfeile f137)]
    putIOwords ["injective f", showT (injective f137)]
    putIOwords ["surjective f", showT (surjective f137)]
    putIOwords ["countSections f", showT (countSections f137)]
    putIOwords ["naming f ", showT (naming f137)]
    putIOwords ["stacking f ", showT (stacking f137)]
    putIOwords ["fixedPoints f ", showT (fixedPoints f137)]
    putIOwords ["sorting f ", showT (sorting f137)]
    -- -- the map f137 A -> A has a codomain of the fixed points and
    -- -- the sorting is then done with this codomain
    -- putIOwords ["fixedPoints f' ", showT (fp137')]
    -- return ()


