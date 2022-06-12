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
import Lib.Rules
import Data.List (nub, group, sort)
import Data.List.Extra ( groupSort )
import Lib.Page13

-- the objects
data SetA = Mother | Father | Child deriving (Show, Eq, Bounded, Enum, Ord)
data SetB = Feather | Stone | Flower  deriving (Show, Eq, Ord, Bounded, Enum)
 
-- the maps 
f :: SetA -> SetB
f (Mother) = Feather
f (Father) = Stone
f (Child) = Flower

-- exercise 5 
data A5 = A5b | A5p | A5q | A5r | A5s deriving (Show, Eq, Bounded, Enum, Ord)
data B5 = B50 | B51 deriving (Show, Eq, Bounded, Enum, Ord)

g :: A5 -> B5
g = fromPfeile [(A5b,B50), (A5p,B50), (A5q,B50), (A5r,B51),
     (A5s,B51)]

-- invOf ff = fromPfeile (invPfeil $ toPfeile ff)

-- section - f must be surjective (epimorphism) 
--                  one of each (from stack)
-- retraction - f must be injective (monomorpism)

-- invFunct f = if bijective f then 
--                 else errorT ["not bijective"]

stackA :: [(SetA, [SetB])]
stackA = groupSort (toPfeile f)
-- sorting :: (Ord k, Bounded k, Enum k) => (k -> v) -> [(k, [v])]
-- sorting ff = groupSort (toPfeile $ ff)

-- naming ff = nub . map snd . toPfeile $ ff 

-- allSections ff = take one from each (groupSort ff) 


page39:: IO ()
page39= do
    putIOwords ["\npage39\n"]
    putIOwords ["fg13", showT (f Father)]
    putIOwords ["injective f", showT (injective f)]
    putIOwords ["surjective f", showT (surjective f)]
    putIOwords ["bijective f", showT (bijective f)]
    putIOwords ["f-1 feather", showT (invFunct f Feather)]
    putIOwords ["groupSort toPfeile f", showT . stacking $ f]
    putIOwords ["groupSort toPfeile f-1", showT.stacking  $  f]
    putIOwords ["countSections", showT $ countSections f]

    -- putIOwords ["sorting f", showT.sorting  $  f]
    -- putIOwords ["sorting f13", showT.sorting  $  f13]
    putIOwords ["stacking f13", showT.stacking  $  f13]
    putIOwords ["naming f13", showT.naming  $  f13]

    putIOwords ["\nExercise 5"]
    putIOwords ["g (q)", showT . g $ A5q]
    putIOwords ["injective g", showT (injective g)]
    putIOwords ["surjective g", showT (surjective g)]
    putIOwords ["countSections g", showT (countSections g)]

    putIOwords ["pag19 john . f13 prefers:", showT (f13 . john $ One)]
    putIOwords ["c1", showT . c1 $ ("1", ['a', 'b'])]
    putIOwords ["sta1 ff", showT . sta1 $ f13]
    putIOwords ["exp1 ff", showT . exp1 $ f13]
    -- putIOwords ["seq1 ff", showT . seq1 $ f13]
    

    -- putStrLn "Lib.Page13 done"
    return ()


-- construct all sections
c1 :: (a, [b]) -> [(a, b)]
c1 (a,[]) = []
c1 (a, (b:bs)) = (a,b) : c1 (a,bs)

-- - create stack
sta1 :: (Ord v, Enum v, Bounded v, Bounded k, Enum k) => (k -> v) -> [(v, [k])]
sta1 ff = groupSort $ map  (\a -> (ff a, a)) dots
-- expand
exp1 :: (Ord v, Enum v, Bounded v, Bounded k, Enum k) =>  [(v, [k])] -> [[(v,k)]]
exp1 ff = map c1 (sta1 ff)
-- sequence all allSections
seq1 ff = sequence . exp $ ff 





