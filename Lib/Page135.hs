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

import UniformBase 
import Lib.Rules
import Data.List.Extra
import Lib.Page13
import Lib.Page39




page135 :: IO ()
page135 = do
    putIOwords ["\npage 135"]
    -- putIOwords ["toPfeile f ", showT (toPfeile f86)]
    -- -- putIOwords ["naming plot ", showT (naming plot)]
    -- putIOwords ["naming f ", showT (naming f86)]
    -- putIOwords ["stacking f ", showT (stacking f86)]
    -- putIOwords ["fixedPoints f ", showT (fixedPoints f86)]
    -- putIOwords ["sorting f ", showT (sorting f86)]
    -- -- the map f86 A -> A has a codomain of the fixed points and
    -- -- the sorting is then done with this codomain
    return ()


