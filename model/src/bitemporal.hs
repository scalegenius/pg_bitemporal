{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies  #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable, EmptyDataDecls ,GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- import Prelude
import Prelude as P hiding (fst, snd, lookup, mod, (.), filter, id)

import           Data.Map (Map)
import qualified Data.Map as Map

-- import qualified Data.Text as T
import qualified Data.List as L
--import           Data.List (nub)

import           Data.Maybe (fromJust)
import TemporalRelationships
import Test.QuickCheck

-- data Row a = Row { effectiveStart :: UTCtime, value:: a}
-- data Table = Table { fields :: [Field], unique :: Set [Field] }
-- [(TableField,TableField)]
-- insert :: [[(TableField,TableField)] -> Row a -> Table a
--  data TimeContext
-- insert :: [[(TableField,TableField)] -> Now -> Row a -> Table a
-- data Row a = Row { effectiveStart :: NominalDiffTime, value:: a}
-- type FieldName = T.Text
-- data FieldType = ColTypeBool | ColTypeText | ColTypeInteger | ColType
-- type Column = (FieldName, FieldType)
-- data PhysicalRow = PhysicalRow     { physicalColumns  :: [Column] }
-- data PhysicalTable = PhysicalTable { physicalRows :: [PhysicalRow] }

-- data Domain = Domain { domainId :: Integer
--         , domainValue :: T.Text
--         , domainDescription :: T.Text 
-- } deriving (Eq)
-- 
-- data BiTemporalRow = BiTemporalRow {
--              domainData    :: Domain -- [Domain]
--            , effective     :: TimePeriod
--            , assertive     :: TimePeriod
--            , rowCreated    :: TimePoint
-- }
-- 
-- data BiTemporalTable = BiTemporalTable {  bitemporalRows :: [BiTemporalRow]  }
-- 
-- insert :: BiTemporalTable -> Domain -> TimePeriod -> TimePeriod -> BiTemporalTable
-- insert p_table p_values p_effective p_asserted = 
--       let (rows,_ ) = query p_table p_effective in
--         if (null rows) then
--           BiTemporalTable ((bitemporalRows p_table) ++  [new_row])
--         else
--           error "Duplicate row insert"
--     where 
--         new_row = BiTemporalRow p_values p_effective p_asserted now 
--      
-- update :: BiTemporalTable -> Domain -> TimePeriod -> TimePeriod -> BiTemporalTable
-- update t r _effective _assertive = t
-- 
-- | query with predicates
--
--
--query :: BiTemporalTable -> TimePeriod -> ([BiTemporalRow], [BiTemporalRow])
--query _table _tp  = ([],[])
--
--
--type DomainCriteria = Domain -> Domain -> Bool
-- then are the they correct bitemporal dimension

-- rows_equal :: Domain -> Domain -> Bool
-- rows_equal r1 r2 = r1 == r2

-- row_match :: Row -> Bool
-- type TemporalCriteria = TimePeriod -> TimePeriod -> Bool
-- 
-- match_effective :: DomainCriteria -> Domain 
--                 -> TemporalCriteria -> TimePeriod 
--                 -> Bool
-- match_effective df d tf t = False


{--
query :: TimePeriod -> BiTemporalTable -> (Row -> Bool) -> [Row]

--
-- Samples from Chris & Chad conversation
--
query :: TimePoint -> BiTemporalTable -> (Row -> Bool) -> [Row]

2 weeks -- 3 weeks -- 4 weeks
assertive  <here>          effective
           new assert -- go in the past
           new effect -- go in the future

queryInternal :: TimePeriod -> BiTemporableTable -> (Row -> Bool) -> ([Row],[Row])
queryInternal _ t db p =
  (filter p db,filter (not . p) db)

-- insert
(rowsAssertive,_) <- queryInternal p_asserted (domainConstraint p_values)
if not (null rows)
   then error ""
   else database ++ [newrow]

(row,rest) <- queryInternal .. .. predicate
let newdb = map update row ++ rest
--}

-- | Constraints
--
class Sql t where
    toSQL :: t -> String

class TimeFlow a where
    timeflow :: TimeTick -> a -> a
    mkTimeflow :: TimeTick -> (a -> a)
    mkTimeflow t =  timeflow t

-- must support +
type TimeResolution = Int

-- incrementTime :: TimeResolution ->  -> TimePoint
-- extendTimePoint a b = TimePoint (a + b)
-- extendTimePoint :: (Num a) => TimeResolution -> a -> TimePoint
-- extendTimePoint a b = TimePoint (a + b)

data TimeTick = TimeTick TimeResolution 
  deriving (Show)

data TimePoint = TimePoint TimeResolution | TimePointInfinity

instance Sql TimePoint where
    toSQL TimePointInfinity = "'infinity'::date"
    toSQL (TimePoint x) = "(current_date + interval '" ++ (show x) ++ " days')::date"

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


instance Arbitrary TimePoint where
   arbitrary = frequency [
      (9, do
         b1 <- arbitrary 
         return $ TimePoint b1
      ),
      (1, return TimePointInfinity)
      ]

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

instance Sql TimePeriod where
    toSQL (TimePeriod a b)= "daterange(" ++ (toSQL a) ++ "," ++ (toSQL b) ++ ", '[)')"

instance Show TimePeriod where
  show (TimePeriod tp1 tp2)  = "\"[" ++(show tp1) ++ ", " ++ (show tp2) ++ ")\""

instance Eq TimePeriod where
  (TimePeriod s1 e1) == (TimePeriod s2 e2) = (s1 == s2) && (e1 == e2)

instance Ord TimePeriod where
  (TimePeriod s1 e1) `compare` (TimePeriod s2 e2) = (s1 `compare` s2) `compare` (e1 `compare` e2)

instance TimeFlow TimePeriod where
    timeflow t (TimePeriod tp1 tp2) = TimePeriod (s tp1) (s tp2) 
           where s = timeflow t

instance  Arbitrary TimePeriod where
    arbitrary = do
         b1 <- arbitrary
         b2 <- arbitrary
         if b1 < b2
            then return $ TimePeriod b1 b2
            else return $ TimePeriod b2 b1

instance TemporalRelationship TimePeriod where
     type TemporalPoint TimePeriod = TimePoint
     fst' (TimePeriod a _) = a 
     snd' (TimePeriod _ a) = a

-- now = TimePoint 123456789 -- some current value
-- data Now = Now {now :: Int, tick :: TimeTick }
now :: TimePoint
now = TimePoint 0
-- If now == 0 then
--   TimeFlow is the operation 

tick :: TimeTick
tick = TimeTick 1

ticks ::  TimeResolution -> TimeTick
ticks n = TimeTick (fromIntegral n)

instance TimeFlow (TimePeriod, TimePeriod) where
    timeflow t (tp1, tp2)= (timeflow t tp1, timeflow t tp2)

instance TimeFlow [(TimePeriod, TimePeriod)] where
    timeflow t = fmap (timeflow t)


data Assertive = Posted | Current | Pending
        deriving (Show, Eq, Ord)
assertives :: [Assertive ]
assertives = [Posted, Current, Pending]


-- Posted  (-1, _)
-- Current ( 0, _)
-- Pending ( 1, _)
instance TimeDimensionPredicate Assertive where
   timePredicate a = case a of
      Posted -> (is_past)
      Current ->  (is_now)
      Pending ->  (is_future)

data Effective = History | Updates | Projection
        deriving (Show, Eq, Ord)
effectives :: [Effective ]
effectives = [History, Updates, Projection]
-- History    (_,-1)
-- Updates    (_, 0)
-- Projection (_, 1)

instance TimeDimensionPredicate Effective where
   timePredicate e =  case e of
      History -> (is_past)
      Updates -> (is_now)
      Projection -> (is_future)

class TimeDimensionPredicate t where
    timePredicate :: t -> (TimePeriod -> Bool)

timePredicates :: (Assertive, Effective) -> ((TimePeriod -> Bool) , (TimePeriod -> Bool) )
timePredicates (a,e) = ( timePredicate a, timePredicate e )

allboxes_cached :: [(Assertive ,Effective)]
allboxes_cached = [(Posted,History), (Posted,Updates), (Posted,Projection)
           ,(Current,History),(Current,Updates),(Current,Projection)
           ,(Pending,History),(Pending,Updates),(Pending,Projection)]
allboxes :: [(Assertive ,Effective)]
allboxes = [ ( a, e ) | a <- assertives, e <- effectives ]

is_past :: (TemporalRelationship t, TemporalPoint t ~ TimePoint) =>  t -> Bool 
is_past tp =  (snd tp) < now
is_now  :: (TemporalRelationship t, TemporalPoint t ~ TimePoint) =>  t -> Bool 
is_now  tp = (fst tp) <= now && (snd tp) >= now
is_future :: (TemporalRelationship t, TemporalPoint t ~ TimePoint) =>  t -> Bool 
is_future tp = (fst tp) > now


genPair :: Gen OperationalTense ->  IO (TimePeriod, TimePeriod)
genPair a = do
        (y,z) <- generate a
        return (y,z)
genPairs = fmap genTense $ [ ( a, e ) | a <- assertives, e <- effectives ]

genTense :: (Assertive , Effective) -> Gen OperationalTense
genTense ae = do
     let (af, ef) = timePredicates ae in do
        arbitrary `suchThat` (g af ef) :: Gen OperationalTense
  where g :: (TimePeriod -> Bool) -> (TimePeriod -> Bool) -> OperationalTense -> Bool
        g af ef (a,e) = (af a) && (ef e)

genTuple :: (Gen OperationalTense, Gen TimePoint) ->  IO (TimePeriod, TimePeriod, TimePoint)
genTuple (a,c) = do
        (x,y) <- generate a
        z <- generate c
        return (x,y,z)

genTuples :: [(Gen OperationalTense, Gen TimePoint)]
genTuples =
     fmap g $ fmap genTense [ ( a, e ) | a <- assertives, e <- effectives ]
  where
      g a = (a, genRowCreated)


genRelationTense :: AllenRelations -> (Assertive , Effective) -> Gen OperationalTense
genRelationTense Equals ae  = do 
     let (af, ef) = timePredicates ae in do
        (a,_) <- arbitrary `suchThat` (g af ef)
        return (a,a)
  where g :: (TimePeriod -> Bool) -> (TimePeriod -> Bool) 
             -> OperationalTense -> Bool
        g af ef (a,_) = (af a) && (ef a)

genRelationTense rel ae  = do 
     let (af, ef) = timePredicates ae in
        arbitrary `suchThat` (g rel af ef)
  where g :: AllenRelations -> (TimePeriod -> Bool) -> (TimePeriod -> Bool) 
             -> OperationalTense -> Bool
        g rel af ef ot@(a,e) = (af a) && (ef e) && (byRelation rel ot)

magicHappen :: [(AllenRelations, [(Assertive, Effective)])] -> [Gen OperationalTense]
magicHappen list = 
            L.concat $ (L.map grt list)
      where grt (rel, lst) = L.map (genRelationTense rel) lst

possiblePath rel = fromJust $ Map.lookup rel possiblePaths


type OperationalTense = (TimePeriod,TimePeriod)
--   (AllenRelations, OperationalTense)
--   (AllenRelations, OperationalTense, NowTimePoint)

instance Sql OperationalTense where
    toSQL (a,b)= "(" ++ (toSQL a) ++ ", " ++ (toSQL b) ++ ")"

instance Sql [OperationalTense] where
    toSQL lst = "VALUES " ++ (L.intercalate "," $ fmap toSQL lst) ++ ";"

genTimePeriod :: IO TimePeriod
genTimePeriod = generate arbitrary :: IO TimePeriod

genBitRecord :: IO BitRecord
genBitRecord  = generate arbitrary :: IO BitRecord

genOperationalTense :: IO OperationalTense 
genOperationalTense = generate arbitrary :: IO OperationalTense 

genRelation :: AllenRelations -> Gen OperationalTense
genRelation rel =  arbitrary `suchThat` (byRelation rel)

byRelation:: AllenRelations -> OperationalTense -> Bool
byRelation rel (a,b) = rel == (whichRelation a b)

genRowCreated :: Gen TimePoint
genRowCreated = 
      arbitrary `suchThat` notInfinity
  where notInfinity a = a /= TimePointInfinity 

test1 = do
           d <- mapM genTuple (test_pathgenerated rel)
           print $ (fromJust $ Map.lookup rel possiblePaths ) 
           print d
           print $ (fmap (f)) d
  where
     genTuple (a,b,c) = do
        x <- generate a
        y <- generate b
        z <- generate c
        return (x,y,z)
     rel = Before
     f = (uncurry3' (getRelationFunction Before))
     uncurry3' :: (t1 -> t2 -> t) -> (t1, t2, t3) -> t
     uncurry3' f (a,b,_) = f a b
     test_pathgenerated rel = fmap aeToBiTuple $ (fromJust $ Map.lookup rel possiblePaths )
     genTimePeriodif :: (TimePeriod -> Bool) -> Gen TimePeriod
     genTimePeriodif f = arbitrary `suchThat` f
     aeToBiTuple :: (Assertive, Effective) -> (Gen TimePeriod, Gen TimePeriod, Gen TimePoint)
     aeToBiTuple (a,e) =(genTimePeriodif $ timePredicate a, genTimePeriodif $ timePredicate e, genRowCreated)
     getRelationFunction   :: TemporalRelationship t => AllenRelations -> t -> t -> Bool
     getRelationFunction rel = fromJust $ Map.lookup rel allenRelationOps



------------------------
main = do
    x <- genTimePeriod
    y <- genBitRecord
    boxes <- mapM genPair genPairs
    aa <- mapM generate ( magicHappen (Map.toList possiblePaths) )
    putStrLn "Main"
    putStrLn $ show x
    putStrLn $ show y
    putStrLn $ "Generate 9 Boxes"
    putStrLn $ show boxes
    putStrLn $ "Generate Possible Paths"
    test1
    putStrLn $ "Generate All Possible Paths"
    putStrLn $ show aa
    putStrLn $ show $ toSQL aa
    
--    z <- generate infiniteList :: IO [OperationalTense]
--    aa <- generate genMeets
--    putStrLn $ show (take 5 $ z)
--    putStrLn $ show (aa )
--    putStrLn $ show (bb )



-- data BitRecord = BitRecord (TimePeriod, TimePeriod, TimePoint)
-- instance TimeFlow BitRecord where
--    timeflow t (BitRecord (tp1, tp2, created))=BitRecord (timeflow t tp1, timeflow t tp2, created)
-- type BitRecord = (AllenRelations, (Assertive, Effective), TimePeriod, TimePeriod, TimePoint)
type BitRecord = (TimePeriod, TimePeriod, TimePoint)
instance TimeFlow (TimePeriod, TimePeriod, TimePoint) where
    timeflow t (tp1, tp2, created)= (timeflow t tp1, timeflow t tp2, created)

instance TimeFlow [BitRecord] where
  timeflow t records = fmap (timeflow t) records

mkTimePeriod :: TimeResolution -> TimeTick ->  TimePeriod
mkTimePeriod a (TimeTick b) = mkTimePeriod' a (a+b)
  where
    mkTimePeriod' :: TimeResolution -> TimeResolution ->  TimePeriod
    mkTimePeriod' a' b' = TimePeriod (TimePoint a') (TimePoint b')

mkBitRecord :: BitRecord
mkBitRecord  = ( mkTimePeriod 10 $ ticks 5, mkTimePeriod 5 $ ticks 15, TimePoint 0 )

mkBR :: TimeResolution -> TimeResolution -> TimeResolution -> TimeResolution -> BitRecord
mkBR i iLen j jLen = ( mkTimePeriod i (ticks iLen), mkTimePeriod j (ticks jLen), TimePoint 0 )
-- generateBitemporal :: Assertive -> Effective -> Now -> BitRecord
-- generateBitemporal a e =  BitRecord (,,) (assertive a) (effecitve e)

-- type AllenRelationshipFunction = (t -> t -> Bool)
-- validTense :: (TemporalRelationship t) => (t->t->Bool)->TimePeriod->TimePeriod->TimePoint->Bool
-- validTense _allenRelation _assertive _effective _now  = False

instance Arbitrary AllenRelations where
    arbitrary = elements  [ Equals, Before, After,
                Meets, MeetsInverse, During, DuringInverse,
                Start, StartInverse, Finish, FinishInverse,
                Overlap, OverlapInverse ]

{--
data AllenRelations =  Equals | Before | After 
    | Meets | MeetsInverse
    | During | DuringInverse
    | Start | StartInverse
    | Finish | FinishInverse
    | Overlap | OverlapInverse
--}


whichRelation :: (Ord t, TemporalRelationship t) => t -> t -> AllenRelations
whichRelation a b = 
      if ( has_excludes a b) then 
          if (has_before a b) then
              if (is_before a b) then
                  Before
              else
                  After
          else
              if (is_meets a b) then
                Meets
              else
                MeetsInverse
      else -- (has_includes  a b)
          if (has_overlaps a b) then
              if (is_overlaps a b) then
                Overlap
              else
                OverlapInverse
          else -- (has_contains a b)
              if (equals a b) then
                Equals 
              else -- (has_encloses a b)
                if (has_during a b) then
                    if (is_during a b) then
                        During
                    else -- is_contained_in a b
                        DuringInverse
                else -- has_aligns_with a b
                    if (has_starts a b) then
                        case (compare a b) of
                          LT -> StartInverse
                          _  -> Start
                    else
                        case (compare a b) of
                          GT -> FinishInverse
                          _  -> Finish
      
                        
relations = [Equals,Before,After,Meets,MeetsInverse,During,DuringInverse,Start,StartInverse,Finish,FinishInverse,Overlap,OverlapInverse]

allenRelationOps :: (TemporalRelationship t) => Map AllenRelations (t->t->Bool)
allenRelationOps = Map.fromList [
      ( Equals ,  equals)
    , (Before , is_before)
    , (After , is_after)
    , (Meets , is_meets)
    , (MeetsInverse, flip is_meets)
    , (During , is_during)
    , (DuringInverse , is_contained_in)
    , (Start , has_starts)
    , (StartInverse, has_starts)
    , (Finish , has_finishes)
    , (FinishInverse , has_finishes)
    , ( Overlap , is_overlaps)
    , ( OverlapInverse , flip is_overlaps)
  ]
--  , has_includes
--   , has_contains
-- , has_aligns_with
-- , has_encloses
-- , has_excludes

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
--           [(Posted,History)
--          , (Posted,Updates), (Posted,Projection)
--           ,(Current,History),(Current,Updates),(Current,Projection)
--           ,(Pending,History),(Pending,Updates)
--        ,(Pending,Projection)]
--



-- class (Assertive a, Effective e) => Ops a e where

-- Insert          == BiemporalInsert
-- UpdateEffective == BitemporalUpdate
-- UpdateAssertive == BitemporalCorrection
-- DeleteEffective == BitemporalInactive
-- DeleteAssertive == Bitemporal Delete
-- DeletePhysical == SQL delete

data BitemporalOperation = UpdateEffective | Insert

data TemporalStateTransformations a = Create | Erase | Modify a -- | PhysicalRemove
    deriving (Show, Eq)

data TST_Modify a = Merge | Split | Shorten a | Lengthen a
    deriving (Show, Eq)

data TST_Direction = Backwards | Forwards
    deriving (Show, Eq)


-- 
-- Op is the Transistion between Boxes?
-- TimeFlow is an Operation?
-- 
data TemporalResult = NoResults | OneResult | ManyResults

-- action operation business_data assertive_range effective_range
--
-- op ops -> current box -> current record _. Results(Assertive, Effective) -> Maybe a
op :: BitemporalOperation -> (Assertive, Effective) -> a -> Maybe TemporalResult
op UpdateEffective (Current, History) _date = Nothing

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

