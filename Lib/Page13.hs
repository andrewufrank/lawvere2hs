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

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Lib.Page13   
     where


-- import Uniform.Strings
-- page 13: sets, maps, composition

-- the objects
data Set13 = John | Mary | Sam
data Set14 = Eggs | Toast | Oatmeal | Coffee
-- the maps 
f13 :: Set13 -> Set14
f13 (John) = Eggs
f13 (Mary) = Coffee
f13 (Sam) = Coffee

g13 :: Set13 -> Set13
g13(John) = Mary
g13(Sam) = Mary
g13(Mary) = John 

page13 :: IO ()
page13= do
    putStrLn "Lib.Page13 start"
    putStrLn "Lib.Page13 done"
    return ()





