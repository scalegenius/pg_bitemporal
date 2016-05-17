{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules, NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TemporalRelationships where

import Prelude as P hiding (fst, snd)

import qualified Data.Text as T
import           Data.List (nub)
-- import Data.Time.Calendar (Day)


data TimePoint = TimePoint Int | TimePointInfinity

instance Eq TimePoint where
  (TimePointInfinity) == (TimePointInfinity) = True
  (TimePoint _) == (TimePointInfinity) = False
  (TimePointInfinity) == (TimePoint _) = False
  (TimePoint s1) == (TimePoint s2) = (s1 == s2)

instance Ord TimePoint where
  (TimePointInfinity) `compare` (TimePointInfinity) = EQ
  (TimePointInfinity) `compare` (TimePoint _)       = GT
  (TimePoint _)       `compare` (TimePointInfinity) = LT
  (TimePoint s1) `compare` (TimePoint s2) = (s1 `compare` s2)
  max (TimePointInfinity) (TimePointInfinity) = TimePointInfinity
  max (TimePointInfinity) _                   = TimePointInfinity
  max _                  (TimePointInfinity)  = TimePointInfinity
  max (TimePoint s1) (TimePoint s2)           = TimePoint (max s1 s2)

now :: TimePoint
now = TimePoint 123456789 -- some current value

-- is_past    :: TimePeriod -> Bool 
-- is_past  tp =   
-- is_present :: TimePeriod -> Bool 
-- is_future :: TimePeriod -> Bool 



-- | TimePeriod is a Closed-Open Range. [)
--   
-- Is Bounded by the two values 
-- Is a Range
-- Is Enum with step being a Single TimePoint
-- A simple implementation is like:
--     type TimePeriod = (TimePoint, TimePoint)
--     type TimePeriods = [TimePoint] 
-- In PostgreSQL this is:
-- select tstzrange(p_range_start, p_range_end,'[)')::timeperiod;

data TimePeriod = TimePeriod { start_point :: TimePoint, end_point :: TimePoint}

instance Eq TimePeriod where
  (TimePeriod s1 e1) == (TimePeriod s2 e2) = (s1 == s2) && (e1 == e2)

instance Ord TimePeriod where
  (TimePeriod s1 e1) `compare` (TimePeriod s2 e2) = (s1 `compare` s2) `compare` (e1 `compare` e2)

-- instance Bounded TimePeriods where
--   minBound = start_point
--   maxBound = end_point
-- 
-- instance Enum TimePeriods where
--   succ = 
--   pred = 

timeperiod :: TimePoint -> TimePoint -> TimePeriod
timeperiod a b = TimePeriod a b

fst :: TimePeriod -> TimePoint
fst tp = start_point tp
snd :: TimePeriod -> TimePoint
snd tp = end_point tp

-- 
-- [starts] [starts^-1]
has_starts ::TimePeriod->TimePeriod-> Bool
has_starts a b = (fst a)  == (fst b) && (snd a) /= (snd b)
-- [finishes] [finishes^-1]
has_finishes ::TimePeriod->TimePeriod-> Bool
has_finishes a b = (snd a) == (snd b) && (fst a) /= (fst b)
-- [equals]
equals ::TimePeriod->TimePeriod-> Bool
equals a  b = (fst a) == (fst b) && (snd a) == (snd b) 
-- [during] 
is_during ::TimePeriod->TimePeriod-> Bool
is_during a b = ((fst a) > (fst b)) && ((snd a) < (snd b))
-- [during^-1] contained
is_contained_in ::TimePeriod->TimePeriod-> Bool
is_contained_in = flip is_during
-- [during] or [during^-1] 
has_during ::TimePeriod->TimePeriod-> Bool
has_during a b = (is_during a b) || (is_during b a)
-- [overlaps] 
is_overlaps ::TimePeriod->TimePeriod-> Bool
is_overlaps a b = ((fst a) < (fst b) && (snd a) > (fst b) && (snd a) < (snd b))

-- either overlaps the other [overlaps] [overlaps^-1]
has_overlaps ::TimePeriod->TimePeriod-> Bool
has_overlaps a b = (is_overlaps a b) || (is_overlaps b a)

-- [before] 
is_before ::TimePeriod->TimePeriod-> Bool
is_before a b = (snd a) < (fst b)
-- [before^-1]
is_after ::TimePeriod->TimePeriod-> Bool
is_after a b = (snd b) < (fst a)
-- either [before] [before^-1]
has_before ::TimePeriod->TimePeriod-> Bool
has_before a b = (snd a) < (fst b) || (snd b) < (fst a)
-- [meets] [meets^-1]
is_meets ::TimePeriod->TimePeriod-> Bool
is_meets a b = (snd a) == (fst b)

has_meets ::TimePeriod->TimePeriod-> Bool
has_meets a b = (snd a) == (fst b) || (snd b) == (fst a)

-- Includes is [Overlaps], [Overlaps^-1] and the Contains group.
-- [Includes] 
--     [Contains] or [Overlaps]
has_includes ::TimePeriod->TimePeriod-> Bool
has_includes a b = (fst a) == (fst b) || (snd a) == (snd b) || 
            ((snd a) <= (snd b) && ((fst a) >= (fst b) || (fst b) < (snd a))) || 
              ((snd a) >= (snd b) && ((fst a) < (snd b) || (fst a) <= (fst b)))

-- Contains is [Equals] and Encloses group.
-- [Contains]
--    [Encloses] or [Equals]
has_contains ::TimePeriod->TimePeriod-> Bool
has_contains a b = (fst a) == (fst b) || (snd a) == (snd b) || 
           ((snd a) < (snd b) && (fst a) > (fst b)) || 
             ((snd b) < (snd a) && (fst b) > (fst a))

-- AlignsWith is [Starts],[Starts^-1], [Finishes], and [Finishes^-1].
-- [Aligns With]
--   [Starts] or [Finishes]
has_aligns_with ::TimePeriod->TimePeriod-> Bool
has_aligns_with a b = (xor ((fst a) == (fst b))  ((snd a) == (snd b)) )

-- Encloses is [During], [During^-1] and the AlignsWith group.
-- [Encloses]
--   [Aligns With] or [During]
has_encloses ::TimePeriod->TimePeriod-> Bool
has_encloses a b = (has_during a b) || (has_aligns_with a b)

-- Excludes is [Before], [Before^-1], [Meets], [Meets^-1].
-- [Excludes]
--   [Before] or [Meets]
has_excludes ::TimePeriod->TimePeriod-> Bool
has_excludes a b = ((fst a) >= (snd b)) || ((fst b) >= (snd a))


-- class TimePeriodClass a b where
--   fst :: a -> b
--   snd :: a -> b
--  
-- instance TimePeriodClass TimePeriod TimePoint where
--     fst :: TimePeriod -> TimePoint
--     fst tp = start_point tp
--     snd :: TimePeriod -> TimePoint
--     snd tp = end_point tp
-- instance TemporalRelationship TimePeriod TimePoint where
--    fst :: TimePeriod -> TimePoint
--    fst tp = start_point tp
--    snd :: TimePeriod -> TimePoint
--    snd tp = end_point tp
--
{--
-- class (Enum a, TimePeriodClass a) => TemporalRelationship a where
class (Eq e, Ord e) => TemporalRelationship t e where
      fst :: t -> e
      snd :: t -> e
      -- [starts] [starts^-1]
      has_starts :: t -> t -> Bool
      has_starts a b = (fst a)  == (fst b) && (snd a) /= (snd b)
      -- [finishes] [finishes^-1]
      has_finishes :: t -> t -> Bool
      has_finishes a b = (snd a) == (snd b) && (fst a) /= (fst b)
      -- [equals]
      equals :: t -> t -> Bool
      equals a  b = (fst a) == (fst b) && (snd a) == (snd b) 

      -- [during] 
      is_during :: t -> t -> Bool
      is_during a b = ((fst a) > (fst b)) && ((snd a) < (snd b))
      -- [during^-1] contained
      is_contained_in :: t -> t -> Bool
      is_contained_in = flip is_during
      -- [during] or [during^-1] 
      has_during :: t -> t -> Bool
      has_during a b = (is_during a b) || (is_during b a)
      -- [overlaps] 
      is_overlaps :: t -> t -> Bool
      is_overlaps a b = ((fst a) < (fst b) && (snd a) > (fst b) && (snd a) < (snd b))

      -- either overlaps the other [overlaps] [overlaps^-1]
      has_overlaps :: t -> t -> Bool
      has_overlaps a b = (is_overlaps a b) || (is_overlaps b a)

      -- [before] 
      is_before :: t -> t -> Bool
      is_before a b = (snd a) < (fst b)
      -- [before^-1]
      is_after :: t -> t -> Bool
      is_after a b = (snd b) < (fst a)
      -- either [before] [before^-1]
      has_before :: t -> t -> Bool
      has_before a b = (snd a) < (fst b) || (snd b) < (fst a)

      -- [meets] [meets^-1]
      is_meets :: t -> t -> Bool
      is_meets a b = (snd a) == (fst b)

      has_meets :: t -> t -> Bool
      has_meets a b = (snd a) == (fst b) || (snd b) == (fst a)

      -- Includes is [Overlaps], [Overlaps^-1] and the Contains group.
      -- [Includes] 
      --     [Contains] or [Overlaps]
      has_includes :: t -> t -> Bool
      has_includes a b = (fst a) == (fst b) || (snd a) == (snd b) || 
            ((snd a) <= (snd b) && ((fst a) >= (fst b) || (fst b) < (snd a))) || 
              ((snd a) >= (snd b) && ((fst a) < (snd b) || (fst a) <= (fst b)))

      -- Contains is [Equals] and Encloses group.
      -- [Contains]
      --    [Encloses] or [Equals]
      has_contains :: t -> t -> Bool
      has_contains a b = (fst a) == (fst b) || (snd a) == (snd b) || 
           ((snd a) < (snd b) && (fst a) > (fst b)) || 
             ((snd b) < (snd a) && (fst b) > (fst a))

      -- AlignsWith is [Starts],[Starts^-1], [Finishes], and [Finishes^-1].
      -- [Aligns With]
      --   [Starts] or [Finishes]
      has_aligns_with :: t -> t -> Bool
      has_aligns_with a b = (xor ((fst a) == (fst b) ((snd a) == (snd b) ))

      -- Encloses is [During], [During^-1] and the AlignsWith group.
      -- [Encloses]
      --   [Aligns With] or [During]
      has_encloses :: t -> t -> Bool
      has_encloses a b = (has_during a b) || (has_aligns_with a b)

      -- Excludes is [Before], [Before^-1], [Meets], [Meets^-1].
      -- [Excludes]
      --   [Before] or [Meets]
      has_excludes :: t -> t -> Bool
      has_excludes a b = ((fst a) >= (snd b)) || ((fst b) >= (snd a))
--}

-- | exclusive or
xor:: Bool -> Bool -> Bool
xor a b = ((not a) /= (not b))
