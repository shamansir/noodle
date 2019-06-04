module Example.Html where

import Prelude

import Effect (Effect)

import Data.Map (Map)
import Data.Map (singleton, insert, empty) as Map
import Data.Tuple.Nested ((/\), type (/\))
import Data.Exists

import Rpd.Network (empty) as Network
import Rpd.Renderer.Html.Html (htmlRenderer)
import Rpd.Renderer.Html.VDom as VDom
import Rpd.Toolkit (Toolkits(..), ToolkitName(..)) as T

import Example.Network (network)
import Example.Toolkit (toolkit)




-- class IsFoo a

-- data X = X
-- data Y = Y

-- instance isFooX :: IsFoo X
-- instance isFooY :: IsFoo Y

-- data Foo = Foo (forall r. (forall a. IsFoo a => a -> r) -> r)

-- toFoo :: forall a . IsFoo a => a -> Foo
-- toFoo v = Foo \f -> f v

-- fromFoo :: forall r. Foo -> (forall a. IsFoo a => a -> r) -> r
-- fromFoo (Foo f) = f

-- foos :: Array Foo
-- foos = [toFoo X, toFoo Y]

-- newtype FooMap k = FooMap (Map k Foo)

-- foos' :: FooMap String
-- foos' = FooMap
--   $ Map.insert "y" (toFoo Y)
--   $ Map.singleton "x" (toFoo X)



-- data RenderResult = RenderResultI Int | RenderResultS String




class Channels c d where
    rep1 ∷ c → d
    rep2 ∷ c → c -> d

data Toolkit d c = Toolkit c {}
type ToolkitE d = Exists (Toolkit d)

mkToolkitE :: forall c d. (Channels c d) => c -> ToolkitE d
mkToolkitE a = mkExists (Toolkit a {})

newtype Toolkits d = Toolkits (Map String (ToolkitE d))

newtype X = X Int
newtype Y = Y String

data F = G | H

instance channelsXF ∷ Channels X F where
    rep1 (X n) = G
    rep2 _ _ = G

instance channelsYF ∷ Channels Y F where
    rep1 (Y s) = H
    rep2 _ _ = G

foos' ∷ Toolkits F
foos' = Toolkits
  $ Map.insert "y" (mkToolkitE $ Y "y")
  $ Map.singleton "x" (mkToolkitE $ X 1)


-- class MyClass c d where rep ∷ c → d

-- data Foo d c = Foo c (c -> d)
-- type FooE d = Exists (Foo d)

-- mkFooE :: forall c d. (MyClass c d) => c -> FooE d
-- mkFooE a = mkExists (Foo a rep)

-- newtype DifferentFoos d = DifferentFoos (Map String (FooE d))

-- newtype X = X Int
-- newtype Y = Y String

-- data F = G | H

-- instance myClassXF ∷ MyClass X F where rep (X n) = G
-- instance myClassYF ∷ MyClass Y F where rep (Y s) = H

-- foos' ∷ DifferentFoos F
-- foos' = DifferentFoos
--   $ Map.insert "y" (mkFooE $ Y "y")
--   $ Map.singleton "x" (mkFooE $ X 1)




-- class MyClass c d

-- instance myClassCAD :: MyClass CA D
-- instance myClassCBD :: MyClass CB D

-- newtype Foo c d = Foo (MyClass c d => c /\ d)

-- -- data DifferentFoos d =
-- --   DifferentFoos (forall c. Map String (Foo c d))

-- data DifferentFoosF d c = DifferentFoosF (String -> c)
-- newtype DifferentFoos d = DifferentFoos (Exists (DifferentFoosF d))

-- data CA = CA
-- data CB = CB
-- data D = D

-- ab :: Foo CA D
-- ab = Foo (CA /\ D)

-- ac :: Foo CB D
-- ac = Foo (CB /\ D)

-- myDifferentFoos :: DifferentFoos D
-- myDifferentFoos = DifferentFoos
--   $ Map.insert "ab" ab
--   $ Map.singleton "ac" ac

main :: Effect Unit
main =
    let
        toolkits = T.Toolkits $ Map.empty -- singleton (ToolkitName "example") toolkit
    in
        VDom.embed' "#app" htmlRenderer toolkits network
