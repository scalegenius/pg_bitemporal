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

data Row { domainId :: Integer
        , domainValue :: T.Text
        , domainDescription :: T.Text }

data BiTemporalRow = BiTemporalRow {
             domainData    :: Row
           , effective     :: TimePeriod
           , assertive     :: TimePeriod
           , rowCreated    :: TimePoint
}

data BiTemporalTable = BiTemporalTable {  bitemporalRows :: [BiTemporalRow]  }

insert :: BiTemporalTable -> Row -> TimePeriod -> TimePeriod -> BiTemporalTable
insert p_table p_values p_effective p_asserted = 
       BiTemporalTable ((bitemporalRows p_table) ++ new_row)
    where new_row = BiTemporalRow p_values p_effective p_asserted now 
     

update :: BiTemporalTable -> Row -> TimePeriod -> TimePeriod -> BiTemporalTable






-- | Constraints
--

main :: IO ()
main = do 
    print "run"
