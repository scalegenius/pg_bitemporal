{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules, NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}


-- import Prelude

import Prelude as P hiding (fst, snd, lookup)

-- import qualified Data.Text as T
-- import qualified Data.List as L
import           Data.Map (Map, lookup)
import qualified Data.Map as Map

-- import qualified Data.Text as T
-- import           Data.List (nub)


class TimeFlow a where
    timeflow :: TimeTick -> a -> a
    mkTimeflow :: TimeTick -> (a -> a)
    mkTimeflow t =  timeflow t

-- must support +
type TimeResolution = Int

data TimeTick = TimeTick TimeResolution 
  deriving (Show)

data TimePoint = TimePoint TimeResolution | TimePointInfinity

instance Show TimePoint where
  show TimePointInfinity = "infinity" 
  show (TimePoint x) = show x

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

instance TimeFlow TimePoint where
    timeflow (TimeTick t) (TimePoint tp) = TimePoint (subtract t tp)
    timeflow _ TimePointInfinity = TimePointInfinity 

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

data TimePeriod = TimePeriod TimePoint TimePoint

instance Show TimePeriod where
  show (TimePeriod tp1 tp2)  = "\"[" ++(show tp1) ++ ", " ++ (show tp2) ++ ")\""

instance Eq TimePeriod where
  (TimePeriod s1 e1) == (TimePeriod s2 e2) = (s1 == s2) && (e1 == e2)

instance Ord TimePeriod where
  (TimePeriod s1 e1) `compare` (TimePeriod s2 e2) = (s1 `compare` s2) `compare` (e1 `compare` e2)

instance TimeFlow TimePeriod where
    timeflow t (TimePeriod tp1 tp2) = TimePeriod (s tp1) (s tp2) 
           where s = timeflow t

-- now :: TimePoint
-- now = TimePoint 123456789 -- some current value
-- data Now = Now {now :: Int, tick :: TimeTick }
now :: TimePoint
now = TimePoint 0

tick :: TimeTick
tick = TimeTick 1

ticks ::  TimeResolution -> TimeTick
ticks n = TimeTick (fromIntegral n)

-- tf = mkTimeFlow tick
-- 
-- If now == 0 then
--   TimeFlow is the operation 
-- 
-- 
instance TimeFlow [BitRecord] where
  timeflow t records = fmap (timeflow t) records

-- data BitRecord = BitRecord (TimePeriod, TimePeriod, TimePoint)
-- instance TimeFlow BitRecord where
--    timeflow t (BitRecord (tp1, tp2, created))=BitRecord (timeflow t tp1, timeflow t tp2, created)

type BitRecord = (TimePeriod, TimePeriod, TimePoint)
instance TimeFlow (TimePeriod, TimePeriod, TimePoint) where
    timeflow t (tp1, tp2, created)= (timeflow t tp1, timeflow t tp2, created)


mkTimePeriod :: TimeResolution -> TimeResolution ->  TimePeriod
mkTimePeriod a b = TimePeriod (TimePoint a) (TimePoint (a+b))

mkBitRecord :: BitRecord
mkBitRecord  = ( mkTimePeriod 10 5, mkTimePeriod 5 15, TimePoint 0 )

mkBR :: TimeResolution -> TimeResolution -> TimeResolution -> TimeResolution -> BitRecord
mkBR i iLen j jLen = ( mkTimePeriod i iLen , mkTimePeriod j jLen, TimePoint 0 )

-- generateBitemporal :: Assertive -> Effective -> Now -> BitRecord
-- generateBitemporal a e =  BitRecord (,,) (assertive a) (effecitve e)

data Assertive = Posted | Current | Pending
        deriving (Show, Eq, Ord)
assertives :: [Assertive ]
assertives = [Posted, Current, Pending]
-- Posted  (-1, _)
-- Current ( 0, _)
-- Pending ( 1, _)
assertiveValue :: Assertive -> Int
assertiveValue a = 
    case a of
      Posted -> -1
      Current ->  0
      Pending ->  1

data Effective = History | Updates | Projection
        deriving (Show, Eq, Ord)
effectives :: [Effective ]
effectives = [History, Updates, Projection]
-- History    (_,-1)
-- Updates    (_, 0)
-- Projection (_, 1)

effectiveValue :: Effective -> Int
effectiveValue e = 
    case e of
      History -> -1
      Updates -> 0
      Projection -> 1

pairs :: [(Assertive ,Effective)]
pairs = [ (a, e) | a <- assertives, e <- effectives ]

    
-- aTimePoint :: Assertive -> (Now -> TimePoint)
--aTimePoint a = \now -> 

-- createTimePoint = 
-- class (Assertive a, Effective e) => Ops a e where

-- UpdateEffective == BitemporalUpdate
-- UpdateAssertive == BitemporalCorrection
-- DeleteEffective == BitemporalInactive
-- DeleteAssertive == Bitemporal
-- DeletePhysical == SQL delete
-- Insert          == BiemporalInsert
data BitemporalOperation = Insert | UpdateEffective | UpdateAssertive
                     | DeleteEffective | DeleteAssertive | DeletePhysical
-- 
-- Op is the Transistion between Boxes?
-- TimeFlow is an Operation?
-- 

data Result = NoResults | OneResult | ManyResults


-- action operation business_data assertive_range effective_range
--
-- op ops -> current box -> current record _. Results(Assertive, Effective) -> Maybe a
op :: BitemporalOperation -> (Assertive, Effective) -> a -> Maybe Result
op UpdateEffective (Current, History) date = Nothing

op UpdateEffective (Current, Updates) date = 
        if isPast date then
              Nothing
        else if isFuture date then
              Nothing
        else  -- isNow
              Nothing
  where isPast _ = True
        isFuture _ = False
op _operation _box _ =  Nothing

{--
d1 <- choose (0,300)
d2 <- choose (0,300)
tp <- choose (0,300)
guard (validAllenRelation d1 d2 tp) 
return (Rec d1 d2 tp)
d1>d2
--}

-- validTense :: TimePeriod -> TimePeriod -> TimePoint -> Bool
-- validTense assertive effective now  = 
        


type OperationalTense = (TimePeriod,TimePeriod)




data AllenRelations =  Equals | Before | After 
    | Meets | MeetsInverse
    | During | DuringInverse
    | Start | StartInverse
    | Finish | FinishInverse
    | Overlap | OverlapInverse
  deriving (Eq, Ord, Show)

type PossiblePaths = Map AllenRelations [(Assertive, Effective)] 

possiblePaths :: PossiblePaths -- [(AllenRelations,[(Assertive ,Effective)])]
possiblePaths = Map.fromList [
   (Equals, [ (Posted, History),(Current,Updates),(Pending,Projection)])
  , (Before, [(Posted,History),(Posted,Updates),(Posted,Projection)
            ,(Current,Projection) ,(Pending,Projection)] )
  ,(After,[(Posted,History),(Current,History),(Pending,History)
            ,(Pending,Updates),(Pending,Projection)] )
  , (Meets, [(Posted,History),(Posted,Updates) 
             ,(Current,Projection),(Pending,Projection)] )
  , (MeetsInverse, [(Posted,History),(Current,History)
                  ,(Pending,Updates),(Pending,Projection)] )
  , ( During, [(Posted,History), (Posted,Updates),(Current,Updates)
              ,(Pending,Updates),(Pending,Projection)])
  , ( DuringInverse, [(Posted,History),(Current,History)
               ,(Current,Updates),(Current,Projection) ,(Pending,Projection)])
  , (Start, [(Posted,History), (Posted,Updates)
               ,(Current,Updates),(Pending,Projection)])
  , (StartInverse, [(Posted,History),(Current,History)
                 ,(Current,Updates),(Pending,Projection)])
  , ( Finish, [(Posted,History),(Current,Updates)
            ,(Current,Projection),(Pending,Projection)])
  , ( FinishInverse, [(Posted,History),(Current,Updates)
                  ,(Pending,Updates),(Pending,Projection)])
  , ( Overlap, [(Posted,History),(Posted,Updates),(Current,Updates)
                 ,(Current,Projection) ,(Pending,Projection)])
  , ( OverlapInverse, [(Posted,History),(Current,History),(Current,Updates)
                        ,(Pending,Updates),(Pending,Projection)])
  ] --end possbilePaths

{--
countPaths1 (op ,list) = (op, length list)
xxxx=   [(Equals,3),(Before,5),(After,5)
        ,(Meets,4),(MeetsInverse,4)
        ,(During,5),(DuringInverse,5)
        ,(Start,4),(StartInverse,4)
        ,(Finish,4),(FinishInverse,4)
        ,(Overlap,5),(OverlapInverse,5)]
--}


--copy
{--
           [(Posted,History)
          , (Posted,Updates), (Posted,Projection)
           ,(Current,History),(Current,Updates),(Current,Projection)
           ,(Pending,History),(Pending,Updates)
        ,(Pending,Projection)]

--}
allboxes :: [(Assertive ,Effective)]
allboxes = [(Posted,History), (Posted,Updates), (Posted,Projection)
           ,(Current,History),(Current,Updates),(Current,Projection)
           ,(Pending,History),(Pending,Updates),(Pending,Projection)]


-- is_past    :: TimePeriod -> Bool 
-- is_past  tp =   
-- is_present :: TimePeriod -> Bool 
-- is_future :: TimePeriod -> Bool 


class (Ord t) => TimePeriodClass t e where
   fst :: t -> TimePoint
   snd :: t -> TimePoint
   
instance TimePeriodClass TimePeriod where
     fst tp = start_point tp
     snd tp = end_point tp

instance TemporalRelationship TimePeriod

class (Ord t, TimePeriodClass t) => TemporalRelationship t where
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
      has_aligns_with a b = (xor ((fst a) == (fst b))  ((snd a) == (snd b)) )

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

-- | exclusive or
xor:: Bool -> Bool -> Bool
xor a b = ((not a) /= (not b))
