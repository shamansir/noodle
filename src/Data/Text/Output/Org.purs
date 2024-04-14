module Data.Text.Output.Org where

import Prelude

import Type.Proxy (Proxy(..))

import Data.Newtype (class Newtype)
import Data.Maybe (Maybe(..))
import Data.Date (Date)
import Data.Time (Time)
import Data.DateTime as DT
import Data.Either
import Data.Tuple.Nested ((/\), type (/\))
import Data.Text.Output (OutputKind, class Renderer, layout, Support)
import Data.Text.Format (Level(..), Bullet(..))


foreign import data Org :: OutputKind


org = Proxy :: _ Org


type Priority = Either Char Int


newtype PropName = PropName String
newtype PropValue = PropValue String
newtype MacroCode = MacroCode String


derive instance Newtype PropName _
derive instance Newtype PropValue _
derive instance Newtype MacroCode _


data TodoKeyword
    = TODO
    | DOING
    | DONE


data PlanningKeyword
    = Deadline
    | Scheduled
    | Closed


data Heading =
    Heading
        { level :: Level
        , keyword :: Maybe TodoKeyword
        , priority :: Maybe Priority
        , comment :: Boolean
        , title :: Maybe String
        , tags :: Array String
        }
        (Array Planning)


data Block
    = Block { name :: String, parameters ::  Array String } String


data DynamicBlock
    = DynamicBlock { name :: String, parameters ::  Array String } String


data Drawer
    = Drawer String (Array Keyword)


data PropertyDrawer
    = PropertyDrawer String (Array Property)


data Check
    = Open
    | Close
    | InProgress


data Item
    = Item
        { bullet :: Bullet
        , counterSet :: Maybe Int
        , check :: Maybe Check
        , tag :: Maybe String
        }
        String


{- data TaskStatus
    = Done
    | Doing
    | Scheduled Date (Maybe Time)
    | ProgressPercent Int
    | ProgressOf Int Int
    | AutoProgress
    -- TODO :  -- add interval
-}


data Task = Task Heading String


data FixedWidth -- move to `Format`


newtype Key = Key String
newtype Value = Value String


data Keyword = Keyword Key Value


data Property = Property PropName PropValue


-- data Macro = Macro


-- TODO data Latex


data Timing
    = Date Date
    | Time Time
    | DateTime DT.DateTime
    -- TODO | Interval
    -- TODO | Repeat


data PlanningMode
    = Active
    | Inactive


data Unit
    = Year
    | Hour
    | Day
    | Week
    | Month


data RepeaterMode
    = Cumulative
    | CatchUp
    | Restart


data Repeater = Repeater Int Unit (Maybe (Int /\ Unit))


data Delay = Delay Int Unit


data Planning =
    Planning
        { type :: PlanningKeyword
        , timing :: Timing
        , when :: Either Repeater Delay
        , mode :: PlanningMode
        }


{-
instance Show TaskStatus where
    show = case _ of
        Done -> "DONE"
        Doing -> "DOING"
        Scheduled date time -> "AT" <> show date <> "::" <> show time
        ProgressPercent prc -> show prc <> "%"
        ProgressOf done total -> show done <> "/" <> show total
        AutoProgress -> "AUTO"
-}

{-
        Timing timing ->
            case timing of
                Date date -> case FDT.formatDateTime "YYYY-DD-MM" $ DT.DateTime date zeroTime of
                        E.Right success -> D.text success
                        E.Left _ -> D.text "<Date?>"
                Time time -> case FDT.formatDateTime "HH:mm:ss" $ DT.DateTime zeroDate time of
                        E.Right success -> D.text success
                        E.Left _ -> D.text "<Time?>"
                DateTime datetime -> case FDT.formatDateTime "YYYY-DD-MM HH:mm:ss" datetime of
                        E.Right success -> D.text success
                        E.Left _ -> D.text "<Time?>"

-}

{-
            zeroDate = Dt.canonicalDate (fromMaybe bottom $ toEnum 0) bottom bottom
            zeroTime = Tm.Time bottom bottom bottom bottom
-}