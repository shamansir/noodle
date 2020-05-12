module Xodus.Query where


import Prelude

import Data.Array
import Data.Ord as O

import Xodus.Dto


type Query = Query' Selector


data Query' a = Query' Database (Array EntityType) a


data Field = Field String


data Condition = Condition (Entity -> Boolean)


data Comparison = Comparison (Entity -> Entity -> O.Ordering)


data Selector
    = All
    | AllOf EntityType
    | Take Int Selector
    | Drop Int Selector
    | Filter Condition Selector
    | Union Selector Selector
    | Intersect Selector Selector
    | Sort Comparison Selector


make :: Database -> (Array EntityType) -> Selector -> Query
make = Query'


instance functorQuery :: Functor Query' where
    map f (Query' database entityTypes value) = Query' database entityTypes $ f value


instance applyQuery :: Apply Query' where
    apply (Query' _ _ f) (Query' database entityTypes value) = Query' database entityTypes $ f value


instance showQuery :: Show a => Show (Query' a) where
    show (Query' _ _ v) = show v


instance showSelector :: Show Selector where
    show = showWithLetter


showWithLetter :: Selector -> String
showWithLetter All = "A"
showWithLetter (AllOf (EntityType { name })) = "AO(" <> name <> ")"
showWithLetter (Take n s) = "T(" <> show n <> "," <> show s <> ")"
showWithLetter (Drop n s) = "D(" <> show n <> "," <> show s <> ")"
showWithLetter (Union sA sB) = "U(" <> show sA <> "," <> show sB <> ")"
showWithLetter (Intersect sA sB) = "I(" <> show sA <> "," <> show sB <> ")"
showWithLetter (Sort _ s) = "S(" <> show s <> ")"
showWithLetter (Filter _ s) = "F(" <> show s <> ")"


showWithSymbol :: Selector -> String
showWithSymbol All = "∀"
showWithSymbol (AllOf (EntityType { name })) = "∃(" <> name <> ")"
showWithSymbol (Take n s) = "⤆(" <> show n <> "," <> show s <> ")"
showWithSymbol (Drop n s) = "⤇(" <> show n <> "," <> show s <> ")"
showWithSymbol (Union sA sB) = "∪(" <> show sA <> "," <> show sB <> ")"
showWithSymbol (Intersect sA sB) = "∩(" <> show sA <> "," <> show sB <> ")"
showWithSymbol (Sort _ s) = "⟐(" <> show s <> ")"
showWithSymbol (Filter _ s) = "≢(" <> show s <> ")"


data SortDirection
    = Ascending
    | Descending


data ComparisonOp
    = EQ
    | LT
    | GT
    | GTE
    | LTE


data ConditionInfo = ConditionInfo Field ComparisonOp String
data SortInfo = SortInfo Field SortDirection


toOp :: forall a. Eq a => Ord a => ComparisonOp -> (a -> a -> Boolean)
toOp EQ = (==)
toOp LT = (<)
toOp GT = (>)
toOp LTE = (<=)
toOp GTE = (>=)


dirToOp :: forall a. Eq a => Ord a => SortDirection -> (a -> a -> O.Ordering)
dirToOp Ascending a b | a == b = O.EQ
dirToOp Ascending a b | a < b = O.LT
dirToOp Ascending a b | a > b = O.GT
dirToOp Ascending a b | otherwise = O.EQ
dirToOp Descending a b | a == b = O.EQ
dirToOp Descending a b | a < b = O.GT
dirToOp Descending a b | a > b = O.LT
dirToOp Descending a b | otherwise = O.EQ


makePInfo :: String -> ComparisonOp -> String -> ConditionInfo
makePInfo f op v = ConditionInfo (Field f) op v


makeSInfo :: String -> SortDirection -> SortInfo
makeSInfo f dir = SortInfo (Field f) dir


instance showPInfo :: Show ConditionInfo where
    show (ConditionInfo (Field field) op v) = "if " <> field <> " " <> show op <> " " <> v


instance showSInfo :: Show SortInfo where
    show (SortInfo (Field field) dir) = "sort by " <> field <> " " <> show dir


instance showCompOp :: Show ComparisonOp where
    show EQ = "equals to"
    show LT = "less than"
    show GT = "greater than"
    show LTE = "less than or equal"
    show GTE = "greater than or equal"


instance showSortDir :: Show SortDirection where
    show Ascending = "ascending"
    show Descending = "descending"


-- To convert back to input
class ShowAsVal a where
    showV :: a -> String


instance showDirVal :: ShowAsVal SortDirection where
    showV Ascending = "asc"
    showV Descending = "desc"


instance showCompVal :: ShowAsVal ComparisonOp where
    showV EQ = "=="
    showV LT = "<"
    showV GT = ">"
    showV LTE = "<="
    showV GTE = ">="


instance showPInfoVal :: ShowAsVal ConditionInfo where
    showV (ConditionInfo (Field field) op v) = field <> " " <> showV op <> " " <> v


instance showSInfoVal :: ShowAsVal SortInfo where
    showV (SortInfo (Field field) dir) = field <> " " <> showV dir
