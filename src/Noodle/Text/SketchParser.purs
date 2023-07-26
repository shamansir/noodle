module Noodle.Text.SketchParser where

import Prelude

import Type.Proxy (Proxy)

import Data.String.Utils (lines) as String
import Data.Array as Array
import Data.Foldable (foldr)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Tuple (uncurry)
import Data.Maybe (isJust, fromMaybe)
import Data.Either (Either(..))
import Data.Int as Int

import Parsing (Parser)
import Parsing.Combinators (many1)

import Data.Array (fromFoldable)
import Data.String (joinWith, replaceAll)  as String
import Data.Tuple.Nested ((/\), type (/\))
import Toolkit.Hydra2.Lang.ToCode (class ToCode, NDF, PS, JS, pureScript, toCode, javaScript)


import Noodle.Text.SketchParser.Expr (Expr(..), Level(..), expr)


type ModuleName = String


data Script = Script ModuleName (Array Expr)


script :: ModuleName -> Parser String Script
script mn = do
  lines <- many1 $ expr Top
  pure $ Script mn $ fromFoldable lines


traverseExprs :: (Expr -> Expr) -> Script -> Script
traverseExprs f (Script moduleName exprs) =
  Script moduleName $ deeper <$> exprs
  where
    deeper :: Expr -> Expr
    deeper (Chain l { subj, startOp, args, tail }) =
      f $ Chain l { subj, startOp, args : deeper <$> args, tail : mapSubOp deeper <$> tail }
    deeper otherExpr = f otherExpr
    mapSubOp f { op, args } = { op, args : f <$> args }


replacements :: Array (Pattern /\ Replacement)
replacements = -- this helps to fix bracket parsing in `FnInline` without specifically parsing any contents of functions' code
  [ Pattern "sin(time)" /\ Replacement "sin{[time]}"
  ]


prepare :: String -> String
prepare str = foldr (uncurry String.replaceAll) str replacements


fixback :: Script -> Script
fixback = traverseExprs fixExpr
  where
    fixExpr (FnInline { args, code }) =
      FnInline
        { args
        , code :
          case code of
            Left unparsed -> Left $ foldr (uncurry String.replaceAll) unparsed $ swapPR <$> replacements
            Right parsed -> code
        }
    fixExpr cexpr@(Chain Top { subj, args, startOp, tail }) =
      if startOp == "setBins" && isJust subj
      then
        let
          nextArgs =
            Array.modifyAt 0
              (\texpr -> case texpr of
                  Num n -> Token $ show $ Int.floor n
                  _ -> texpr
              )
              args
            # fromMaybe args
        in
          Chain
            Top
            { subj, startOp, tail
            , args : nextArgs
            }
      else cexpr
    fixExpr otherExpr = otherExpr
    swapPR (Pattern pattern /\ Replacement replacement) =
      Pattern replacement /\ Replacement pattern


instance Show Script where
  show :: Script -> String
  show (Script _ exprs) =
    String.joinWith "\n" $ show <$> exprs


instance ToCode PS Script where
  toCode :: Proxy PS -> Script -> String
  toCode _ (Script moduleName exprs) =
    (String.joinWith "\n" $ pursHeader moduleName) <> "\n\n" <>
    (String.joinWith "\n" pursPrefix) <> "\n    " <>
    (String.joinWith "\n    " $ Array.concat $ String.lines <$> toCode pureScript <$> exprs)


instance ToCode JS Script where
  toCode :: Proxy JS -> Script -> String
  toCode _ (Script _ exprs) = String.joinWith "\n" $ toCode javaScript <$> exprs


pursHeader ∷ String → Array String
pursHeader moduleName =
  [ "module " <> moduleName <> " where"
  , ""
  , "import Prelude (Unit, discard, (#), ($))"
  , "import Prelude (show) as Core"
  , ""
  , "import Effect (Effect)"
  , "import Effect.Console as Console"
  , ""
  , "import Toolkit.Hydra2.Lang"
  , "import Toolkit.Hydra2.Lang.ToCode (toCode, pureScript, javaScript)"
  , "import Toolkit.Hydra2.Lang.Api"
  ]


pursPrefix :: Array String
pursPrefix =
  [ "example :: Program Unit"
  , "example = do"
  ]