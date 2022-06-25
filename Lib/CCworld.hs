-----------------------------------------------------------------------------
--
-- Module      :   the core concept data 
{- describe a point in space time theme with a value

specialized (for simplicity) one D space, simple value Float
    trivial interpolation
-}
--------------------------------------------------------------------------- 
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.CCworld
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

pageCCworld :: IO ()
pageCCworld= do
    putIOwords ["\npageCCworld"]
    putIOwords ["field data f1", showT . Map.toList $ f1]
    -- putIOwords ["coords", showT coords]
    -- putIOwords ["p2c", showT . map (p2c pointData) $ ps]
    -- putIOwords ["c2p", showT . map (c2p pointData) $ cs]
    -- putIOwords ["injective pointData", showT . injectiveTest $ pointData]

data Point1d = Point1d {x1 :: Float}
    deriving (Eq, Ord, Read, Show, Num)
mkp = Point1d

data Theme = Theme {unTheme :: Text}
    deriving (Eq, Ord, Read, Show)
th0 = Theme "zero"

data Time1 = Time1 {unTime1:: Int}
    deriving (Eq, Ord, Read, Show)
mkt = Time1 
t0 = Time1 0

instance Functor Int Time1 Ord where
    fmap = fmap

--------------------- field

data WorldPoint = WP {space::Point1d, time::Time1, theme::Theme}
    deriving (Eq, Ord, Read, Show)
mkwp x11 t1 = WP (mkp x11) (mkt t1) (Theme "First")
wp1 = WP p1 t0 th0

p1 = Point1d 1.0
data Value = ValueInt Int
    deriving (Eq, Ord, Read, Show)
mkvi = ValueInt 

type Field = Map.Map WorldPoint Value

f1 = Map.fromList [(wp1, mkvi 0),  ( mkwp 1.2 2, mkvi 1)]

------------------ objects

data ObjProperty = OV {oid::Obj, otime::Time1, otheme::Theme}
-- genau wie WorldPoint
data Obj = Obj {unObj :: Int }

type Objects = Map.Map ObjProperty Value


-- data Field = Field Float Float
-- field1 = Field 1.0 0.1 

-- field :: Field -> Point -> Float
-- field (Field a b) p = a + x p * b 

-- -- necessary because set has context Ord 
-- type Ranking = ConstrainedCategory (->) Ord
-- ordd :: (Ord a, Ord b) => (a -> b) -> Ranking a b
-- ordd = constrained
-- inOrd :: Ranking a b -> Ranking a b
-- inOrd = id
-- instance Functor Set Ranking Ranking where
--   fmap = constrainedFmap Set.map

-- pointData :: Map.Map Text Float
-- pointData = Map.fromList $ zip ["a1", "a2"] [1.0, 2.0]
-- -- is automatically surjective (if no other def of domain)

-- injectiveTest :: (Eq a, Eq b) => Map.Map a b -> Bool
-- injectiveTest mp = length ls == (length . nub $ ls)
--     where ls = map snd. Map.toList $ mp


-- points :: Set Text
-- points = Set.fromList $ Map.keys pointData

-- ps :: [Text]
-- ps = Set.toList points

-- coords :: Set Float
-- coords = Set.fromList $ Map.elems pointData

-- cs :: [Float]
-- cs = Set.toList coords

-- p2c :: Map.Map Text Float -> Text -> Float
-- p2c pdb p = pdb Map.! p

-- c2p :: Map.Map Text Float -> Float -> Text
-- c2p pdb c = (Map.fromList . map swap . Map.toList $ pdb) Map.! c















--     -- putIOwords ["y1", showT y1]
-- -- x :: Set Integer
-- -- x = Set.fromList [1 .. 9]
-- --     -- deriving  (Show, Eq, Bounded, Enum, Ord)
-- --     --  diagram page 137
-- -- y :: Set Integer
-- -- y = Set.fromList [11..19]

-- -- y1 :: Set Integer
-- -- y1 = fmap (ordd(+10)) $ x 


-- -- f137 :: SetX -> SetX
-- -- f137 = fromPfeile [(X1,X2), (X2,X3), (X3,X4), (X4, X5), (X5,X3), (X6,X7), (X7,X5), (X8,X9),(X9,X9)]


-- -- data SetY a = SetY a 
-- --     deriving  (Show, Eq, Bounded, Ord)  -- not Enum

-- -- instance Hask.Functor SetY  where
-- --     fmap ff (SetY a) = SetY (ff a)
-- -- --     -- fmap . f137 = f137' . fmap

-- -- f137' :: SetY SetX -> SetY SetX
-- -- f137' = fmap f137 

-- -- fp137' ::  [SetY SetX]
-- -- fp137' = fixedPoints f137'

-- -- page135hask :: IO ()
-- -- page135hask= do
-- --     putIOwords ["\npage 135hask"]
-- --     putStr "Functor:  " 
-- --     print $ fmap (ordd (^2)) $ Set.fromList [-3 .. 4]
-- --     putIOwords ["y", showT y]
-- --     putIOwords ["y1", showT y1]
--      -- putIOwords ["toPfeile f ", showT (toPfeile f137)]
--     -- putIOwords ["injective f", showT (injective f137)]
--     -- putIOwords ["surjective f", showT (surjective f137)]
--     -- putIOwords ["countSections f", showT (countSections f137)]
--     -- putIOwords ["naming f ", showT (naming f137)]
--     -- putIOwords ["stacking f ", showT (stacking f137)]
--     -- putIOwords ["fixedPoints f ", showT (fixedPoints f137)]
--     -- putIOwords ["sorting f ", showT (sorting f137)]
--     -- -- -- the map f137 A -> A has a codomain of the fixed points and
--     -- -- -- the sorting is then done with this codomain
--     -- putIOwords ["fixedPoints f' ", showT (fp137')]
--     -- return ()


