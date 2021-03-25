
module TensorFlow.TfModel where

import Data.Tuple.Nested (type (/\))


type Shape = Int /\ Int /\ Int


data TfModel
    = Empty
    | InputLayer Shape
    | Conv2DLayer TfModel
    | MaxPoooling2DLayer TfModel
    | Concatenate TfModel TfModel
    | Add TfModel TfModel



toCode :: TfModel -> String
toCode _ = "Hello Hell"
