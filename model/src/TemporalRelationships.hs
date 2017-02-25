{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts, GADTs #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls ,GeneralizedNewtypeDeriving #-}

module TemporalRelationships 
( TemporalRelationship(..)
 , AllenRelations(..)
 , has_starts
 , has_finishes
 , equals
 , is_during
 , is_contained_in
 , has_during
 , is_overlaps
 , has_overlaps
 , is_before
 , is_after
 , has_before
 , is_meets
 , has_meets
 , has_includes
 , has_contains
 , has_aligns_with
 , has_encloses
 , has_excludes
)
where

-- import Prelude
import Prelude as P hiding (fst, snd, lookup)

data AllenRelations =  Equals | Before | After 
    | Meets | MeetsInverse
    | During | DuringInverse
    | Start | StartInverse
    | Finish | FinishInverse
    | Overlap | OverlapInverse
  deriving (Eq, Ord, Show)


class ( Ord (TemporalPoint t) ) => TemporalRelationship t where
      type TemporalPoint t
      fst :: t -> TemporalPoint t
      fst = fst'
      snd :: t -> TemporalPoint t
      snd = snd' 
      fst' :: t -> TemporalPoint t
      snd' :: t -> TemporalPoint t

-- [starts] [starts^-1]
has_starts :: (TemporalRelationship t) => t -> t -> Bool
has_starts a b = (fst a)  == (fst b) && (snd a) /= (snd b)
-- [finishes] [finishes^-1]
has_finishes :: (TemporalRelationship t) => t -> t -> Bool
has_finishes a b = (snd a) == (snd b) && (fst a) /= (fst b)
-- [equals]
equals :: (TemporalRelationship t) => t -> t -> Bool
equals a  b = (fst a) == (fst b) && (snd a) == (snd b) 
-- [during] 
is_during :: (TemporalRelationship t) => t -> t -> Bool
is_during a b = ((fst a) > (fst b)) && ((snd a) < (snd b))
-- [during^-1] contained
is_contained_in :: (TemporalRelationship t) => t -> t -> Bool
is_contained_in = flip is_during
-- [during] or [during^-1] 
has_during :: (TemporalRelationship t) => t -> t -> Bool
has_during a b = (is_during a b) || (is_during b a)
-- [overlaps] 
is_overlaps :: (TemporalRelationship t) => t -> t -> Bool
is_overlaps a b = ((fst a) < (fst b) && (snd a) > (fst b) && (snd a) < (snd b))
-- either overlaps the other [overlaps] [overlaps^-1]
has_overlaps :: (TemporalRelationship t) => t -> t -> Bool
has_overlaps a b = (is_overlaps a b) || (is_overlaps b a)

-- [before] 
is_before :: (TemporalRelationship t) => t -> t -> Bool
is_before a b = (snd a) < (fst b)
-- [before^-1]
is_after :: (TemporalRelationship t) => t -> t -> Bool
is_after a b = (snd b) < (fst a)
-- either [before] [before^-1]
has_before :: (TemporalRelationship t) => t -> t -> Bool
has_before a b = (snd a) < (fst b) || (snd b) < (fst a)
-- [meets] [meets^-1]
is_meets :: (TemporalRelationship t) => t -> t -> Bool
is_meets a b = (snd a) == (fst b)
has_meets :: (TemporalRelationship t) => t -> t -> Bool
has_meets a b = (snd a) == (fst b) || (snd b) == (fst a)

-- Includes is [Overlaps], [Overlaps^-1] and the Contains group.
-- [Includes] 
--     [Contains] or [Overlaps]
has_includes :: (TemporalRelationship t) => t -> t -> Bool
has_includes a b = (fst a) == (fst b) || (snd a) == (snd b) || 
            ((snd a) <= (snd b) && ((fst a) >= (fst b) || (fst b) < (snd a))) || 
              ((snd a) >= (snd b) && ((fst a) < (snd b) || (fst a) <= (fst b)))

-- Contains is [Equals] and Encloses group.
-- [Contains]
--    [Encloses] or [Equals]
has_contains :: (TemporalRelationship t) => t -> t -> Bool
has_contains a b = (fst a) == (fst b) || (snd a) == (snd b) || 
           ((snd a) < (snd b) && (fst a) > (fst b)) || 
             ((snd b) < (snd a) && (fst b) > (fst a))

-- AlignsWith is [Starts],[Starts^-1], [Finishes], and [Finishes^-1].
-- [Aligns With]
--   [Starts] or [Finishes]
has_aligns_with :: (TemporalRelationship t) => t -> t -> Bool
has_aligns_with a b = (xor ((fst a) == (fst b))  ((snd a) == (snd b)) )

-- Encloses is [During], [During^-1] and the AlignsWith group.
-- [Encloses]
--   [Aligns With] or [During]
has_encloses :: (TemporalRelationship t) => t -> t -> Bool
has_encloses a b = (has_during a b) || (has_aligns_with a b)

-- Excludes is [Before], [Before^-1], [Meets], [Meets^-1].
-- [Excludes]
--   [Before] or [Meets]
has_excludes :: (TemporalRelationship t) => t -> t -> Bool
has_excludes a b = ((fst a) >= (snd b)) || ((fst b) >= (snd a))

-- | exclusive or
xor:: Bool -> Bool -> Bool
xor a b = ((not a) /= (not b))
