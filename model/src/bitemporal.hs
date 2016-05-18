{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules, NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell, TypeOperators #-}

import Prelude hiding (mod, (.), filter, id)

import qualified Data.Text as T
import Data.List (nub)

import TemporalRelationships

type Count = Int

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

data Domain = Domain { domainId :: Integer
        , domainValue :: T.Text
        , domainDescription :: T.Text 
} deriving (Eq)

data BiTemporalRow = BiTemporalRow {
             domainData    :: Domain -- [Domain]
           , effective     :: TimePeriod
           , assertive     :: TimePeriod
           , rowCreated    :: TimePoint
}

data BiTemporalTable = BiTemporalTable {  bitemporalRows :: [BiTemporalRow]  }

insert :: BiTemporalTable -> Domain -> TimePeriod -> TimePeriod -> BiTemporalTable
insert p_table p_values p_effective p_asserted = 
      let (rows,_ ) = query p_table p_effective in
        if (null rows) then
          BiTemporalTable ((bitemporalRows p_table) ++  [new_row])
        else
          error "Duplicate row insert"
    where 
        new_row = BiTemporalRow p_values p_effective p_asserted now 
     
update :: BiTemporalTable -> Domain -> TimePeriod -> TimePeriod -> BiTemporalTable
update t r _effective _assertive = t

-- | query with predicates
--

query :: BiTemporalTable -> TimePeriod -> ([BiTemporalRow], [BiTemporalRow])
query _table _tp  = ([],[])


type DomainCriteria = Domain -> Domain -> Bool
-- then are the they correct bitemporal dimension

rows_equal :: Domain -> Domain -> Bool
rows_equal r1 r2 = r1 == r2

-- row_match :: Row -> Bool
type TemporalCriteria = TimePeriod -> TimePeriod -> Bool

match_effective :: DomainCriteria -> Domain 
                -> TemporalCriteria -> TimePeriod 
                -> Bool
match_effective df d tf t = False


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

main :: IO ()
main = do 
    print "run"
