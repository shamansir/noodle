module HydraGenData where


data Hydra
    = Noise Value Value
    | Voronoi Value Value Value
    | Osc Value Value Value
    | Shape Value Value Value
    | Gradient Value
    | Src Texture
    | Solid Value Value Value Value
    | Src Source
    | Prev ?ArgType
    | Rotate Texture Value Value
    | Scale Texture Value Value Value Value Value
    | Pixelate Texture Value Value
    | Repeat Texture Value Value Value Value
    | RepeatX Texture Value Value
    | RepeatY Texture Value Value
    | Kaleid Texture Value
    | Scroll Texture Value Value Value Value
    | ScrollX Texture Value Value
    | ScrollY Texture Value Value
    | Posterize Texture Value Value
    | Shift Texture Value Value Value Value
    | Invert Texture Value
    | Contrast Texture Value
    | Brightness Texture Value
    | Luma Texture Value Value
    | Tresh Texture Value Value
    | Color Value Value Value Value
    | Saturate Texture Value
    | Hue Texture Value
    | Colorama Texture Value
    | Sum Texture ?ArgType
    | R Texture Value Value
    | G Texture Value Value
    | B Texture Value Value
    | A Texture Value Value
    | Add Texture Texture Value
    | Sub Texture Texture Value
    | Layer Texture Texture Value
    | Blend Texture Texture Value
    | Mult Texture Texture Value
    | Diff Texture Texture
    | Mask Texture Texture
    | ModulateRepeat Texture Texture Value Value Value Value
    | ModulateRepeatX Texture Texture Value Value
    | ModulateRepeatY Texture Texture Value Value
    | ModulateKaleid Texture Texture Value
    | ModulateScrollX Texture Texture Value Value
    | ModulateScrollY Texture Texture Value Value
    | Modulate Texture Texture Value
    | ModulateScale Texture Texture Value Value
    | ModulatePixelate Texture Texture Value Value
    | ModulateRotate Texture Texture Value Value
    | ModulateHue Texture Texture Value
    | Render From
    | Update UpdateFn
    | SetResolution Value Value
    | Hush ?Unit_Type
    | SetFunction GlslFn
    | Speed Value
    | Bpm Value
    | Width
    | Height
    | Pi
    | Time
    | Mouse
    | InitCam Source Value
    | InitImage Source String
    | InitVideo Source String
    | Init SourceOptions
    | InitStream Source ?ArgType
    | InitScreen Source
    | Fast Array Value
    | Smooth Array Value
    | Ease Array Ease
    | Offset Array Value
    | Fit Array Value Value
    | Fft Audio AudioBin
    | SetSmooth Audio Value
    | SetCutoff Audio Value
    | SetBins Audio Value
    | SetScale Audio Value
    | Hide Audio ?ArgType
    | Show Audio ?ArgType
    | Out Texture Output