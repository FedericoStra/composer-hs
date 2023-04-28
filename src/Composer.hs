{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  Composer
-- Description :  Tools to help with function composition.
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
-- of functions parameterized by a list, along with their strict counterparts 'compMapRev''
-- and 'compMapRev_'' which compose in reverse order.
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
    -- As the name suggests, 'compMap' can also be defined in terms of 'comp' and 'Prelude.map':
    --
    -- @'compMap' f = 'comp' . 'Prelude.map' f@
    --
    -- On the other hand, 'compMap_' is simply obtained by pre-composing with a given function,
    -- hence it could be defined as
    --
    -- @'compMap_' f f0 = (. f0) . 'compMap' f@
    compMap,
    -- compMap',
    compMapRev',
    compMap_,
    -- compMap_',
    -- compMap'_,
    compMapRev_',

    -- * Identities #identities#
    -- $identities

    -- ** Universal property #universal_property#
    -- $universal_property

    -- ** Fusion property #fusion_property#
    -- $fusion_property

    -- ** Additional identities #additional_identities#
    -- $additional_identities
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
-- Implemented with 'foldr'.
compMap :: (b -> (a -> a)) -> [b] -> (a -> a)
compMap = flip . foldr

-- -- | Maps a list to a sequence of functions and composes them.
-- --
-- -- Equivalent to 'compMap', but implemented with 'foldl''.
-- compMap' :: (b -> (a -> a)) -> [b] -> (a -> a)
-- compMap' = flip . foldl' . flip

-- | Maps a list to a sequence of functions and composes them in reverse order.
--
-- Implemented with 'foldl''.
compMapRev' :: (b -> (a -> a)) -> [b] -> (a -> a)
compMapRev' = flip . foldl' . flip

-- | Maps a list to a sequence of functions and composes them,
-- pre-composing everything with an extra function.
--
-- Implemented with 'foldr'.
compMap_ :: (b -> (a -> a)) -> (a -> a) -> [b] -> (a -> a)
compMap_ f f0 = flip (foldr f . f0)

-- -- | Maps a list to a sequence of functions and composes them,
-- -- pre-composing everything with an extra function.
-- --
-- -- Equivalent to 'compMap_', but implemented with 'foldl''.
-- compMap_' :: (b -> (a -> a)) -> (a -> a) -> [b] -> (a -> a)
-- compMap_' f f0 = flip (foldl' (flip f) . f0)

-- -- | Maps a list to a sequence of functions and composes them,
-- -- pre-composing everything with an extra function.
-- --
-- -- This is just an alias of 'compMap_''.
-- compMap'_ :: (b -> (a -> a)) -> (a -> a) -> [b] -> (a -> a)
-- compMap'_ = compMap_'

-- | Maps a list to a sequence of functions and composes them in reverse order,
-- pre-composing everything with an extra function.
--
-- Implemented with 'foldl''.
compMapRev_' :: (b -> (a -> a)) -> (a -> a) -> [b] -> (a -> a)
compMapRev_' f f0 = flip (foldl' (flip f) . f0)

-- $identities
--
-- With reference to the article
--
-- -   /A tutorial on the universality and expressiveness of fold/,
--     Graham Hutton,
--     /J. Functional Programming/ 9 (4): 355–372, July 1999,
--     http://www.cs.nott.ac.uk/~pszgmh/fold.pdf
--
-- we can say that 'comp' and 'compMap' satisfy both a [universal property](#g:universal_property)
-- and a [fusion property](#g:fusion_property).
-- Moreover, there are several other equivalences, such as the
-- [additional identities](#g:additional_identities).

-- $universal_property
--
-- The /universal properties/ of 'compMap' and 'compMap_' can be stated as an equivalence
-- between two definitions of a function @g@ that processes lists.
--
-- === Universal property of 'compMap'
--
-- The following definitions of a function @g@ are equivalent:
--
-- -
--
--     @
--     g []     = 'id'
--     g (x:xs) = f x . g xs
--     @
--
-- -   @g = 'compMap' f@
--
-- === Universal property of 'compMap_'
--
-- The following definitions of a function @g@ are equivalent:
--
-- -
--
--     @
--     g []     = f0
--     g (x:xs) = f x . g xs
--     @
--
-- -   @g = 'compMap_' f f0@

-- $fusion_property
--
-- The /fusion property/ of 'compMap' could be more appropriately called /commutation property/.
-- It provides a simple condition under which the composition of an arbitrary function and a 'compMap'
-- can be reversed.
--
-- The /fusion property/ of 'compMap_' provides two simple conditions that are sufficient to ensure
-- that the composition of an arbitrary function and a 'compMap_' can be fused together into
-- a single 'compMap_'.
--
-- === Fusion property of 'compMap'
--
-- If the following identity holds
--
-- @
-- (h .) . f == (. h) . g
-- @
--
-- then we have
--
-- @
-- (h .) . 'compMap' f == (. h) . 'compMap' g
-- @
--
-- Notice that the commutation relationship can also be written equivalently as
--
-- @h . f x == g x . h@
--
-- and the thesis as
--
-- @
-- h . 'compMap' f xs == 'compMap' g xs . h
-- @
--
-- === Fusion property of 'compMap_'
--
-- If the following identities hold
--
-- @
-- h . f0 == g0
-- (h .) . f == (. h) . g
-- @
--
-- then we have
--
-- @
-- (h .) . 'compMap_' f f0 == 'compMap_' g g0
-- @
--
-- Notice that the commutation relationship can also be written equivalently as
--
-- @h . f x == g x . h@
--
-- and the thesis as
--
-- @
-- h . 'compMap_' f f0 xs == 'compMap_' g g0 xs
-- @

-- $additional_identities
-- @
-- 'compRev' == 'comp'    . 'Prelude.reverse'
-- 'comp'    == 'compRev' . 'Prelude.reverse'
--
-- 'comp'      == 'compMap' 'id'
--
-- 'compMap' f == 'comp' . 'Prelude.map' f
--           == 'foldr' (\x chain -> f x . chain) 'id'
--           == 'foldr' ((.) . f) 'id'
--
-- 'compMap_' f f0 == (. f0) . 'compMap' f
-- 'compMap' f     == 'compMap_' f 'id'
-- @
