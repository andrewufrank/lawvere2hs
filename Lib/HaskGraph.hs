-----------------------------------------------------------------------------
--
-- Module      :   an attempt to construct a graph with the built in data.map from container 
{-  the data are values of type container;
errors are detected only at run time

two objects: Points and Coordinates (for now 1d)
two functions: coord and point
-- issue: not all coords are assigned
-}
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
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.HaskGraph
     where -- change prelude for constrainded-categories
import Prelude ()
import Control.Category.Constrained.Prelude
import qualified Control.Category.Hask as Hask
import Control.Monad.Constrained
-- for set example code
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Monoid
-- end 
import UniformBase 
import qualified Data.Map.Strict as Map
import Data.List (nub)


-- necessary because set has context Ord 
type Ranking = ConstrainedCategory (->) Ord
ordd :: (Ord a, Ord b) => (a -> b) -> Ranking a b
ordd = constrained
inOrd :: Ranking a b -> Ranking a b
inOrd = id
instance Functor Set Ranking Ranking where
  fmap = constrainedFmap Set.map

pointData :: Map.Map Text Float
pointData = Map.fromList $ zip ["a1", "a2"] [1.0, 2.0]

injectiveTest :: (Eq a, Eq b) => Map.Map a b -> Bool
injectiveTest mp = length ls == (length . nub $ ls)
    where ls = map snd. Map.toList $ mp


points :: Set Text
points = Set.fromList $ Map.keys pointData

ps :: [Text]
ps = Set.toList points

coords :: Set Float
coords = Set.fromList $ Map.elems pointData

cs :: [Float]
cs = Set.toList coords

p2c :: Map.Map Text Float -> Text -> Float
p2c pdb p = pdb Map.! p

c2p :: Map.Map Text Float -> Float -> Text
c2p pdb c = (Map.fromList . map swap . Map.toList $ pdb) Map.! c

pageHaskGraph :: IO ()
pageHaskGraph= do
    putIOwords ["\npageHaskGraph"]
    putIOwords ["points", showT points]
    putIOwords ["coords", showT coords]
    putIOwords ["p2c", showT . map (p2c pointData) $ ps]
    putIOwords ["c2p", showT . map (c2p pointData) $ cs]
    putIOwords ["injective pointData", showT . injectiveTest $ pointData]














    -- putIOwords ["y1", showT y1]
-- x :: Set Integer
-- x = Set.fromList [1 .. 9]
--     -- deriving  (Show, Eq, Bounded, Enum, Ord)
--     --  diagram page 137
-- y :: Set Integer
-- y = Set.fromList [11..19]

-- y1 :: Set Integer
-- y1 = fmap (ordd(+10)) $ x 


-- f137 :: SetX -> SetX
-- f137 = fromPfeile [(X1,X2), (X2,X3), (X3,X4), (X4, X5), (X5,X3), (X6,X7), (X7,X5), (X8,X9),(X9,X9)]


-- data SetY a = SetY a 
--     deriving  (Show, Eq, Bounded, Ord)  -- not Enum

-- instance Hask.Functor SetY  where
--     fmap ff (SetY a) = SetY (ff a)
-- --     -- fmap . f137 = f137' . fmap

-- f137' :: SetY SetX -> SetY SetX
-- f137' = fmap f137 

-- fp137' ::  [SetY SetX]
-- fp137' = fixedPoints f137'

-- page135hask :: IO ()
-- page135hask= do
--     putIOwords ["\npage 135hask"]
--     putStr "Functor:  " 
--     print $ fmap (ordd (^2)) $ Set.fromList [-3 .. 4]
--     putIOwords ["y", showT y]
--     putIOwords ["y1", showT y1]
     -- putIOwords ["toPfeile f ", showT (toPfeile f137)]
    -- putIOwords ["injective f", showT (injective f137)]
    -- putIOwords ["surjective f", showT (surjective f137)]
    -- putIOwords ["countSections f", showT (countSections f137)]
    -- putIOwords ["naming f ", showT (naming f137)]
    -- putIOwords ["stacking f ", showT (stacking f137)]
    -- putIOwords ["fixedPoints f ", showT (fixedPoints f137)]
    -- putIOwords ["sorting f ", showT (sorting f137)]
    -- -- -- the map f137 A -> A has a codomain of the fixed points and
    -- -- -- the sorting is then done with this codomain
    -- putIOwords ["fixedPoints f' ", showT (fp137')]
    -- return ()


