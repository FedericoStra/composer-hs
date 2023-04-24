{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  Composer
-- Copyright   :  (c) Federico Stra 2023
-- License     :  MIT (see the LICENSE file in the distribution)
--
-- Maintainer  :  stra.federico@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module implements several tools to help with function composition.
--
-- The most basic ones are 'comp' and 'comp'', which take a list of functions and compose them.
-- The difference between the two is their ability to operate on infinite lists.
-- In addition there are 'compRev' and 'compRev'' which compose in reverse order.
--
-- This module also defines two functions, 'compMap' and 'compMap_', which build a composition
-- of functions parameterized by a list.
module Composer
  ( -- * Simple composition

    -- | This section documents functions to compose a list of functions, possibly in reverse order.
    --
    -- The functions 'comp' and 'compRev' are equivalent to
    --
    -- > comp []     = id
    -- > comp (f:fs) = f . comp fs
    --
    -- and
    --
    -- > compRev []     = id
    -- > compRev (f:fs) = comp fs . f
    --
    -- respectively.
    comp,
    comp',
    compRev,
    compRev',

    -- * Parameterized composition

    -- | This section documents functions to build a composition parameterized by a list,
    -- possibly in reverse order.
    --
    -- The functions 'compMap' and 'compMap_' are equivalent to
    --
    -- > compMap :: (b -> (a -> a)) -> [b] -> (a -> a)
    -- > compMap f []     = id
    -- > compMap f (x:xs) = f x . compMap f xs
    --
    -- and
    --
    -- > compMap_ :: (b -> (a -> a)) -> [b] -> (a -> a)
    -- > compMap_ f f0 []     = f0
    -- > compMap_ f f0 (x:xs) = f x . compMap_ f f0 xs
    --
    -- respectively.
    --
    -- As the name suggests, 'compMap' can also be defined in terms of 'comp' and 'map':
    --
    -- > compMap f = comp . map f
    --
    -- On the other hand, 'compMap_' is simply obtained by pre-composing with a given function,
    -- hence it can be defined as
    --
    -- > compMap_ f f0 = (. f0) . compMap f
    compMap,
    compMap_,
  )
where

import Data.List (foldl')
import Prelude (flip, foldr, id, (.))

-- | Composes a list of functions. Implemented with 'foldr'.
comp :: [a -> a] -> (a -> a)
comp = foldr (.) id

-- | Composes a list of functions. Implemented with 'foldl''.
comp' :: [a -> a] -> (a -> a)
comp' = foldl' (.) id

-- | Composes a list of functions in reverse order. Implemented with 'foldr'.
compRev :: [a -> a] -> (a -> a)
compRev = foldr (flip (.)) id

-- | Composes a list of functions in reverse order. Implemented with 'foldl''.
compRev' :: [a -> a] -> (a -> a)
compRev' = foldl' (flip (.)) id

-- | Maps a list to a sequence of functions and composes them.
--
-- @'compMap' f@ is equivalent to @'compMap_' f id@.
compMap :: (b -> (a -> a)) -> [b] -> (a -> a)
compMap = flip . foldr

-- | Maps a list to a sequence of functions and composes them,
-- pre-composing everything with an extra function.
compMap_ :: (b -> (a -> a)) -> (a -> a) -> [b] -> (a -> a)
compMap_ f f0 = flip (foldr f . f0)

{- Alternative implementations
compMap_ f f0 = (. f0) . compMap f
-}
