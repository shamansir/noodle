
module TensorFlow.TfModel where


data TfModel
    = Empty
    | InputLayer
    | Conv2DLayer TfModel
    | MaxPoooling2DLayer TfModel
    | Concatenate TfModel TfModel
    | Add TfModel TfModel



toCode :: TfModel -> String
toCode _ = "Hello Hell"
