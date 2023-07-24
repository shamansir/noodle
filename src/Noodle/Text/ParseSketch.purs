module Noodle.Text.ParseSketch where

import Prelude

import Data.Either (Either(..))

import Effect (Effect)
import Effect.Console as Console

import Parsing (runParser)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Toolkit.Hydra2.Lang.ToCode (pureScript, toCode, javaScript)
import Noodle.Text.SketchParser as Parser


main :: Effect Unit
main = do
  fileContents <- readTextFile UTF8 "test/hydra-examples/examples-website/mahalia_3.js"
  let parseResult = runParser fileContents Parser.script
  case parseResult of
    Right script -> do
        Console.log "ORIGINAL: ======\n"
        Console.log fileContents
        Console.log "EXPR: ======\n"
        Console.log $ show script
        Console.log "PURS ======\n"
        Console.log $ toCode pureScript script
        Console.log "JS ======\n"
        Console.log $ toCode javaScript script
    Left error -> do
        Console.log "Parse failed."
        Console.log $ show error
  -- Console.log $