module Test.HMaps
  ( MyMapping, Map
  )
  where



import Prelude

import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Heterogeneous.Folding
    ( class HFoldlWithIndex
    , class FoldingWithIndex
    , hfoldlWithIndex
    , class FoldlRecord
    )
import Heterogeneous.Mapping
    ( class HMapWithIndex
    , class Mapping
    , class MappingWithIndex
    , hmap, hmapWithIndex
    -- , class HFoldlWithIndex
    )
import Prim.Row (class Cons) as Row
import Type.Proxy (Proxy(..))
import Data.List (List)
import Data.List as List
import Data.Array ((:))
import Data.Array as Array

import Record as Record
import Record.Extra as Record
import Prim.Row as R
import Prim.RowList as RL
import Type.RowList as RL
import Unsafe.Coerce (unsafeCoerce)



data AddOneAndShow = AddOneAndShow

instance addOneAndShow ::
  (Semiring n, Show n) =>
  Mapping AddOneAndShow n String where
  mapping AddOneAndShow = add one >>> show


--
-- fooAaa = hmap (show >>> Console.log) { c : 20, a : "foo" }
-- fooAaa = hmap (show) { c : 20, a : "foo" }
fooAaa :: { c :: String, a :: String, in :: String }
fooAaa = hmap (AddOneAndShow) { c : 20, a : 3, in : { a : 12 } }


data ToArray = ToArray

instance mappingToArray ::
  Mapping ToArray a (Array (Tuple x b)) where
  mapping ToArray = const []


instance ixMappingToArray ::
  MappingWithIndex ToArray i a (Array (Tuple x b)) where
  mappingWithIndex ToArray _ = const []


fooBbb :: forall t194 t195 t202 t203 t210 t211.
      { a :: Array (Tuple t195 t194)
      , c :: Array (Tuple t203 t202)
      , in :: Array (Tuple t211 t210)
      }
fooBbb = hmap (ToArray) { c : 20, a : 3, in : { a : 12 } }


data ToArrayAToX x = ToArrayAToX

data ToArrayToX x = ToArrayToX


class AToX a x where
  aToX :: a -> x


class ToX x where
  toX :: forall a. a -> x


instance AToX Int RecFoo where
  aToX :: Int -> RecFoo
  aToX n = RecFoo { foo : n + 2 }


{-}
instance Semigroup a => ToX (RecSG a) where
  --toX :: forall a'. a' -> RecSG a
  toX n = RecSG { sg : n <> n } -}


instance AToX String RecFoo where
  aToX :: String -> RecFoo
  aToX str = RecFoo { foo : String.length str + 2 }


instance AToX Int RecBar where
  aToX :: Int -> RecBar
  aToX n = RecBar { bar : show (n - 2) }


instance AToX String RecBar where
  aToX :: String -> RecBar
  aToX str = RecBar { bar : str <> show (String.length str) }


instance mappingToArrayAToX ::
  AToX a x =>
  Mapping (ToArrayAToX x) a (Array (x /\ a)) where
  mapping ToArrayAToX a = [ aToX a /\ a ]


instance ixMappingToArrayAToX ::
  AToX a x =>
  MappingWithIndex (ToArrayAToX x) i a (Array (i /\ x /\ a)) where
  mappingWithIndex ToArrayAToX idx a = [ idx /\ aToX a /\ a ]


instance mappingToArrayToX ::
  ToX x =>
  Mapping (ToArrayToX x) a (Array (x /\ a)) where
  mapping ToArrayToX a = [ toX a /\ a ]


instance ixMappingToArrayToX ::
  ToX x =>
  MappingWithIndex (ToArrayToX x) i a (Array (i /\ x /\ a)) where
  mappingWithIndex ToArrayToX idx a = [ idx /\ toX a /\ a ]


newtype RecFoo = RecFoo { foo :: Int }
newtype RecBar = RecBar { bar :: String }

newtype RecSG a = RecSG { sg :: a }

derive newtype instance Eq RecFoo
derive newtype instance Eq RecBar


testWithAToXFoo =
  hmap (ToArrayAToX :: ToArrayAToX RecFoo) { c : 20, a : 3, f : 15, x : "aaa" }
  ==
    { c : [ RecFoo { foo : 22 } /\ 20 ]
    , a : [ RecFoo { foo : 5 } /\ 3 ]
    , f : [ RecFoo { foo : 17 } /\ 15 ]
    , x : [ RecFoo { foo : 5 } /\ "aaa" ]
    -- , d : [ Rec { foo : 15 } /\ "aaa" ]
    }


testWithAToXBar =
  hmap (ToArrayAToX :: ToArrayAToX RecBar) { c : 20, a : 3, f : 15, x : "aaa" }
  ==
    { c : [ RecBar { bar : "18" } /\ 20 ]
    , a : [ RecBar { bar : "1" } /\ 3 ]
    , f : [ RecBar { bar : "13" } /\ 15 ]
    , x : [ RecBar { bar : "aaa3" } /\ "aaa" ]
    -- , d : [ Rec { foo : 15 } /\ "aaa" ]
    }


{-
testWithToXFoo =
  hmap (ToArrayToX :: ToArrayToX RecFoo) { c : 20, a : 3, f : 15, x : "aaa" }
  ==
    { c : [ RecFoo { foo : 22 } /\ 20 ]
    , a : [ RecFoo { foo : 5 } /\ 3 ]
    , f : [ RecFoo { foo : 17 } /\ 15 ]
    , x : [ RecFoo { foo : 5 } /\ "aaa" ]
    -- , d : [ Rec { foo : 15 } /\ "aaa" ]
    }


testWithToXBar =
  hmap (ToArrayToX :: ToArrayToX RecBar) { c : 20, a : 3, f : 15, x : "aaa" }
  ==
    { c : [ RecBar { bar : "18" } /\ 20 ]
    , a : [ RecBar { bar : "1" } /\ 3 ]
    , f : [ RecBar { bar : "13" } /\ 15 ]
    , x : [ RecBar { bar : "aaa3" } /\ "aaa" ]
    -- , d : [ Rec { foo : 15 } /\ "aaa" ]
    }
-}


newtype ZipProps fns = ZipProps { | fns }


instance zipProps ::
  (IsSymbol sym, Row.Cons sym (a -> b) x fns) =>
  MappingWithIndex (ZipProps fns) (SProxy sym) a b where
  mappingWithIndex (ZipProps fns) prop = Record.get prop fns


{- test :: { a :: Int, b :: Tuple String Number, c :: Boolean }
test =
    let
        zipRecord = hmapWithIndex <<< ZipProps
    in
        { a: add 1
        , b: Tuple "bar"
        , c: \a -> not a
        }
        `zipRecord`
        { a: 12
        , b: 42.0
        , c: true
        } -}

-- { a: 13, b: (Tuple "bar" 42.0), c: false }


data ShowProps = ShowProps


instance showProps ::
  (Show a, IsSymbol sym) =>
  FoldingWithIndex ShowProps (SProxy sym) String a String where
  foldingWithIndex ShowProps prop str a =
    pre <> reflectSymbol prop <> ": " <> show a
    where
    pre | str == "" = ""
        | otherwise = str <> ", "


showRecord :: forall r.
  HFoldlWithIndex ShowProps String { | r } String =>
  { | r } ->
  String
showRecord r =
  "{ " <> hfoldlWithIndex ShowProps "" r <> " }"


type TestRows = ( a :: String, b :: Int, c :: Boolean )


-- test :: String
test = showRecord { a: "foo" , b: 42 , c: false }


data ShowValues = ShowValues

instance showValues ::
  (Show a, IsSymbol sym) =>
  FoldingWithIndex ShowValues (Proxy sym) String a String
  where
  foldingWithIndex _ _ str a = pre <> show a
    where
    pre | str == "" = ""
        | otherwise = str <> ", "

showTwice :: forall r.
  HFoldlWithIndex ShowProps String { | r } String =>
  HFoldlWithIndex ShowValues String { | r } String =>
  { | r } ->
  String
showTwice r = do
  let a = "{ " <> hfoldlWithIndex ShowProps "" r <> " }"
      b = "[ " <> hfoldlWithIndex ShowValues "" r <> " ]"
  a <> b



test' = showTwice { a: "foo" , b: 42 , c: false }


-- data HoldProps :: forall k. k -> Type
data HoldProps (p :: Symbol -> Type) x = HoldProps


class Holder (proxy :: Symbol -> Type) x where
  hold :: forall sym. IsSymbol sym => proxy sym -> x
  extract :: forall r. x -> (forall sym. IsSymbol sym => proxy sym -> r) -> r


class FromProxy (trg :: Symbol -> Type) where
  fromProxy :: forall sym. IsSymbol sym => Proxy sym -> trg sym


instance Holder ProxyTest HoldsSymbol where
  hold = holdSymbol
  extract = withSymbol


instance FromProxy ProxyTest where
  fromProxy :: forall sym. Proxy sym -> ProxyTest sym
  fromProxy _ = ProxyTest


instance holdProps ::
  (IsSymbol sym, Holder p x, FromProxy p) =>
  FoldingWithIndex (HoldProps p x) (Proxy sym) (Array x) a (Array x) where
  foldingWithIndex HoldProps prop symbols _ =
    hold (fromProxy prop :: p sym) : symbols


order :: forall r p x.
  Holder p x => FromProxy p => Proxy p ->
  HFoldlWithIndex (HoldProps p x) (Array x) { | r } (Array x) =>
  { | r } ->
  (Array x)
order _ r =
  hfoldlWithIndex (HoldProps :: HoldProps p x) ([] :: Array x) r


test1 :: Array HoldsSymbol
test1 = order (Proxy :: _ ProxyTest) { foo : "a", bar : 42 }


data ProxyTest (s :: Symbol) = ProxyTest


newtype HoldsSymbol = HoldsSymbol (forall r. (forall sym. IsSymbol sym => ProxyTest sym -> r) -> r)


holdSymbol :: forall sym. IsSymbol sym => ProxyTest sym -> HoldsSymbol
holdSymbol sym = HoldsSymbol (_ $ sym)


holdSymbolP :: forall sym. IsSymbol sym => Proxy sym -> HoldsSymbol
holdSymbolP sym = HoldsSymbol (_ $ (ProxyTest :: _ sym))


withSymbol :: forall r. HoldsSymbol -> (forall sym. IsSymbol sym => ProxyTest sym -> r) -> r
withSymbol (HoldsSymbol fn) = fn



{-
showWithIndex :: forall hlist.
  HFoldlWithIndex ShowWithIndex (Array (Tuple Int String)) hlist (Array (Tuple Int String)) =>
  hlist ->
  Array (Tuple Int String)
showWithIndex =
  hfoldlWithIndex ShowWithIndex ([] :: Array (Tuple Int String))
-}










data Test = Test

testF :: forall a. MyMapping IsMap a -> String
testF _ = "aaa"

testX :: MyMapping IsMap String -> String
testX _ = "aaa"

testXX :: MyMapping (MakeMap Unit) String -> String
testXX _ = "aaa"

testA :: Map -> String
testA (Map x) = x


testB :: Map -> String
testB _ = testA $ Map "x"


-- testC :: Map -> String
-- testC = testA ( Map "x" :: MakeMap String)


data Map = Map String
foreign import data MyMapping :: Map -> Type -> Type
foreign import data IsMap :: Map

foreign import data MakeMap :: Type -> Map


-- data MyMapping


-- data Foo :: MyMapping String
data Foo ( a :: MyMapping IsMap String ) = Foo








data Focus
foreign import data Empty :: Focus
foreign import data OnKeys :: Row Type -> Focus
foreign import data FocusT :: Focus -> Focus

foreign import data HoldKeys :: Row Type -> Type



class TestFocus (x :: Focus) a | a -> x where
  something :: Proxy x -> a

class TestFocusNP (x :: Focus) a | a -> x where
  somethingNP :: a


instance TestFocus Empty String where
  something :: Proxy Empty -> String
  something _ = "foo"
else instance TestFocus (OnKeys x) String where
  something :: Proxy (OnKeys x) -> String
  something _ = "bar"


instance TestFocusNP Empty String where
  somethingNP :: String
  somethingNP = "foo"
else instance TestFocusNP (OnKeys x) String where
  somethingNP :: String
  somethingNP = "bar"


instance TestFocus (OnKeys x) Int where
  something :: Proxy (OnKeys x) -> Int
  something _ = 42
else instance TestFocus Empty Int where
  something :: Proxy Empty -> Int
  something _ = 20


-- instance TestFocus Empty Int where
--   something :: Proxy Empty -> Int
--   something _ = 42

-- class TestFocusA (x :: HoldKeys row) a | a -> x where
--   somethingA :: a



class (RL.RowToList row ks, Record.Keys ks) <= HasKeysAt row ks
instance (RL.RowToList row ks, Record.Keys ks) => HasKeysAt row ks


class HasKeysAt row ks <= HasKeys row ks a | a -> row, a -> ks
    where listKeys :: a -> List String


newtype Wrapper (row :: Row Type) =
    Wrapper (Record row)

data WrapperX (row :: Row Type) =
    WrapperX (forall rl. (HasKeysAt row rl => Record row))

data WrapperXRl rl (row :: Row Type) =
    WrapperXRl (HasKeysAt row rl => Record row)



newtype WrapperA (row :: Row Type) a =
    WrapperA (a /\ Record row)


instance HasKeysAt row ks => HasKeys row ks (WrapperX row) where
  listKeys (WrapperX row) = Record.keys row


instance HasKeysAt row ks => HasKeys row ks (Wrapper row) where
    listKeys :: Wrapper row -> List String
    listKeys (Wrapper row) = Record.keys row


instance HasKeysAt row ks => HasKeys row ks (WrapperA row a) where
    listKeys :: forall a. WrapperA row a -> List String
    listKeys (WrapperA (_ /\ row)) = Record.keys row


class ConverterFA x where
    convertFA :: forall row. Wrapper row -> x

-- class ConverterFA_ Empty x | x -> focus where
--     convertFA :: forall row. Wrapper row -> x

class ConverterFXA x where
    convertFXA :: forall row. Wrapper row -> x

class ConverterFAA x where
    convertFAA :: forall a row. WrapperA row a -> x


class ConverterRow (row :: Row Type) x where
    convertKnowningRow :: Wrapper row -> x


class ConverterRowA (row :: Row Type) x where
    convertKnowningRowA :: forall a. WrapperA row a -> x


class ConverterFocus (p :: Focus) x | x -> p where
    convertFocused :: forall row. Wrapper row -> x

class ConverterFocusP (p :: Focus) x where
    convertFocusedP :: forall row. Proxy p -> Wrapper row -> x


-- class ConverterFocus Empty x where
--     convertFocused :: forall row. Proxy p -> Wrapper row -> x


class ConverterAX a x | a -> x, x -> a where
  convertAX :: a -> x

newtype Keys row ks = Keys (List String)


newtype Keys' = Keys' (List String)


-- Compiles ::

instance HasKeysAt row ks => ConverterRow row (Keys row ks)
    where
        convertKnowningRow
            :: Wrapper row
            -> Keys row ks
        convertKnowningRow wrapper = Keys (Record.keys (Proxy :: Proxy row)) -- (listKeys wrapper)


instance HasKeysAt row ks => ConverterRowA row (Keys row ks)
    where
        convertKnowningRowA
            :: forall a
             . WrapperA row a
            -> Keys row ks
        convertKnowningRowA wrapper = Keys (listKeys wrapper)


instance HasKeysAt row ks => ConverterRow row Keys'
    where
        convertKnowningRow
            :: Wrapper row
            -> Keys'
        convertKnowningRow wrapper = Keys' (listKeys wrapper)


instance HasKeysAt row ks => ConverterRowA row Keys'
    where
        convertKnowningRowA
            :: forall a
             . WrapperA row a
            -> Keys'
        convertKnowningRowA wrapper = Keys' (listKeys wrapper)


instance HasKeysAt row ks => ConverterAX (Wrapper row) Keys'
    where
        convertAX
            :: Wrapper row
            -> Keys'
        convertAX wrapper = Keys' (listKeys wrapper)


{-
instance HasKeysAt row ks => ConverterAX (Wrapper row) String
    where
        convertAX
            :: Wrapper row
            -> Keys row ks
        convertAX wrapper = Keys' (listKeys wrapper)
-}


testConvert = convertAX (Wrapper { a : 4 })


instance HasKeysAt row ks => ConverterFocus (OnKeys row) Keys'
    where
        convertFocused
            :: forall row'
             . Wrapper row'
            -> Keys'
        convertFocused wrapper = Keys' $ Record.keys (Proxy :: Proxy row) -- Keys (listKeys proxy)


instance HasKeysAt row ks => ConverterFocus (OnKeys row) (Keys row ks)
    where
        convertFocused
            :: forall row'
             . Wrapper row'
            -> Keys row ks
        convertFocused wrapper = Keys $ Record.keys (Proxy :: Proxy row) -- Keys (listKeys proxy)


instance HasKeysAt row ks => ConverterFocusP (OnKeys row) Keys'
    where
        convertFocusedP
            :: forall row'
             . Proxy (OnKeys _)
            -> Wrapper row'
            -> Keys'
        convertFocusedP proxy wrapper = Keys' $ Record.keys (Proxy :: Proxy row) -- Keys (listKeys proxy)


instance HasKeysAt row ks => ConverterFocusP (OnKeys row) (Keys row ks)
    where
        convertFocusedP
            :: forall row'
             . Proxy (OnKeys row)
            -> Wrapper row'
            -> Keys row ks
        convertFocusedP proxy wrapper = Keys $ Record.keys (Proxy :: Proxy row) -- Keys (listKeys proxy)


{-
instance HasKeysAt row ks => ConverterFA (Keys row ks)
    where
        convertFA
            :: Wrapper row
            -> Keys row ks
        convertFA wrapper = Keys (listKeys wrapper)
-}

{-}
instance HasKeysAt row ks => ConverterFA (Keys row ks)
    where
        convertFA
            :: forall row'
             . Wrapper row'
            -> Keys row ks
        convertFA (Wrapper row) = Keys (Record.keys row)-- (listKeys wrapper)


instance ConverterFA Keys'
    where
        convertFA
            :: forall row' ks
             . HasKeysAt row' ks => Wrapper row'
            -> Keys'
        convertFA (Wrapper row) = Keys' (Record.keys row)-- (listKeys wrapper)
-}



{-
instance HasKeysAt row ks => ConverterFA (List String)
    where
        convertFA
            :: forall row
             . Wrapper row
            -> List String
        convertFA (Wrapper record) = Record.keys (unsafeCoerce record) -}


-- instance HasKeysAt row ks => ConverterFAA (Keys row ks)
--     where
--         convertFAA
--             :: forall a
--              . WrapperA row a
--             -> Keys row ks
--         convertFAA wrapper = Keys (listKeys wrapper)


-- instance HasKeysAt row ks => ConverterFA Keys'
--     where
--         convertFA
--             :: forall row
--              . Wrapper row
--             -> Keys'
--         convertFA wrapper = Keys' (listKeys wrapper)


-- instance HasKeysAt row ks => ConverterFAA Keys'
--     where
--         convertFAA
--             :: forall a
--              . WrapperA row a
--             -> Keys row ks
--         convertFAA wrapper = Keys' (listKeys wrapper)






-- An example using the Boolean-like data type YesNo:
data YesNo = Yes | No

data YesNoKind
foreign import data YesK :: YesNoKind
foreign import data NoK  :: YesNoKind

{-
Read yesK and noK as:
  yesK = (YesNoProxyValue :: YesNoProxy Yes) - a value of type "YesNoProxy Yes"
  noK  = (YesNoProxyValue :: YesNoProxy No)  - a value of type "YesNoProxy No" -}
yesK :: Proxy YesK
yesK = Proxy

noK :: Proxy NoK
noK = Proxy

class ReflectYesNoKind :: YesNoKind -> Constraint
class ReflectYesNoKind a where
  reflectYesNo :: Proxy a -> YesNo

instance ReflectYesNoKind YesK where
-- reflectYesNo (Proxy :: Proxy Yes) = Yes
   reflectYesNo _                    = Yes

instance ReflectYesNoKind NoK where
-- reflectYesNo (Proxy :: Proxy No) = No
   reflectYesNo _                   = No


-- We can also use instance chains here to distinguish
-- one from another

class IsYes :: YesNoKind -> Constraint
class IsYes a where
  isYes :: Proxy a -> YesNo

instance IsYes YesK where
  isYes _ = Yes
else instance IsYes NoK where
  isYes _ = No

-- Using instance chains here is more convenient if we had
-- a lot more type-level values than just 2. In some cases,
-- it is needed in cases where a type-level type can have an
-- infinite number of values, such as a type-level String

-- Open a REPL, import this module, and then run this code:
--    reflectYesNo yesK
--    reflectYesNo noK
--    isYes yesK
--    isYes noK


-- necessary for not getting errors while trying the functions in the REPL

instance Show YesNo where
    show Yes = "Yes"
    show No  = "No"


-- We can reify a YesNo by defining a callback function that receives
-- the corresponding type-level value as its only argument
-- (where we do type-level programming):

reifyYesNo :: forall returnType
            . YesNo
            -> (forall b. ReflectYesNoKind b => Proxy b -> returnType)
            -> returnType
reifyYesNo Yes function = function yesK
reifyYesNo No  function = function noK





data KindName
foreign import data Value :: KindName


data Value_Level_Type = V

-- NANS
inst :: Proxy Value
inst = Proxy

-- The class name is usually "Is[KindName]"
class IsKindName :: KindName -> Constraint
class IsKindName a where
  -- and the reflect function is usually "reflect[KindName]"
  reflectKindName :: Proxy a -> Value_Level_Type

instance IsKindName Value where
  reflectKindName _ = V

-- NANS
class IsKindName a <= ConstrainedToKindName a

-- NANS
instance ConstrainedToKindName Value

-- Usually reify[KindName]
reifyKindName :: forall r
           . Value_Level_Type
          -> (forall a. IsKindName a => Proxy a -> r)
          -> r
reifyKindName _valueLevel function = function inst


testReify :: forall t950. (forall a. IsKindName a => Proxy a -> t950) -> t950
testReify = reifyKindName V


testReify2 :: Value_Level_Type
testReify2 = reifyKindName V reflectKindName