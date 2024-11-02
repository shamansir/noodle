module Cli.App where

import Prelude

import Data.Maybe (Maybe(..))

import Options.Applicative as OA
import Options.Applicative ((<**>))

import Cli.State (State)

import Noodle.Toolkit.Families (Families)


type Options =
    { fromFile :: Maybe String
    }


defaultOptions =
    { fromFile : Nothing
    } :: Options


data App pstate (fs :: Families) repr m
    = App Options (State pstate fs repr m)