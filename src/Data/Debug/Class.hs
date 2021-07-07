{-# language UndecidableInstances #-}
{-# language DataKinds #-}
{-# language PolyKinds #-}
{-# language StandaloneKindSignatures #-}
{-# language FlexibleContexts #-}
{-# language DerivingVia #-}
{-# language DeriveGeneric #-}
{-# language TypeApplications #-}
module Data.Debug.Class where

-- Debugged
import qualified Data.Debug.Type as Debug

-- base
import GHC.Exts hiding (toList)
import GHC.Generics
import Data.Kind
import GHC.TypeLits
import Data.Proxy
import Data.Foldable (toList)

-- text
import qualified Data.Text as Text

-- strict
import Data.Strict.Tuple

-- containers
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

-- unordered-containers
import qualified Data.HashMap.Strict as HashMap


-- keys
import Data.Key (FoldableWithKey(..), Key)
import qualified Data.Key as Key
import Numeric.Natural (Natural)
import Data.Int
import Data.Word
import Data.Monoid

type Debug :: Type -> Constraint
class Debug a where
  debug :: a -> Debug.Repr

------------------------------------------------------------
---- | primitive instances |--------------------------------
------------------------------------------------------------


instance Debug Int where
  debug = Debug.int

instance Debug Word where
  debug = Debug.word

instance Debug Natural where
  debug = Debug.natural

instance Debug Integer where
  debug = Debug.integer

instance Debug Int8 where
  debug = Debug.int . fromIntegral

instance Debug Int16 where
  debug = Debug.int . fromIntegral

instance Debug Int32 where
  debug = Debug.int . fromIntegral

instance Debug Int64 where
  debug = Debug.integer . fromIntegral

instance Debug Word8 where
  debug = Debug.word . fromIntegral

instance Debug Word16 where
  debug = Debug.word . fromIntegral

instance Debug Word32 where
  debug = Debug.word . fromIntegral

instance Debug Word64 where
  debug = Debug.natural . fromIntegral

instance Debug Float where
  debug = Debug.double . realToFrac

instance Debug Double where
  debug = Debug.double

instance Debug Char where
  debug = Debug.char

instance {-# OVERLAPPING #-} Debug String where
  debug = Debug.text . Text.pack


------------------------------------------------------------
---- | Constructor types |----------------------------------

instance Debug () where
  debug ()  = Debug.constructor "()" []

instance Debug Bool where
  debug True  = Debug.constructor "True"   []
  debug False  = Debug.constructor "False" []


instance Debug a => Debug (Maybe a) where
  debug Nothing  = Debug.constructor "Nothing" []
  debug (Just a) = Debug.constructor "Just" [debug a]

instance Debug Ordering where
  debug LT  = Debug.constructor "LT" []
  debug EQ  = Debug.constructor "EQ" []
  debug GT  = Debug.constructor "GT" []

instance Debug a => Debug (Sum a) where
  debug (Sum a) = Debug.constructor "Sum" [debug a]

instance Debug a => Debug (Product a) where
  debug (Product a) = Debug.constructor "Product" [debug a]

instance Debug a => Debug (First a) where
  debug (First a) = Debug.constructor "First" [debug a]

instance Debug a => Debug (Last a) where
  debug (Last a) = Debug.constructor "Last" [debug a]


instance (Debug a, Debug b) => Debug (Either a b) where
  debug (Left a ) = Debug.constructor "Left"  [debug a]
  debug (Right b) = Debug.constructor "Right" [debug b]

------------------------------------------------------------
---- | Opaque Types |---------------------------------------
------------------------------------------------------------


instance Debug (Ptr a) where
  debug _ = Debug.opaque_ "ptr"

instance Debug (FunPtr a) where
  debug _ = Debug.opaque_ "fun_ptr"

------------------------------------------------------------
---- | List-Like Types | -----------------------------------
------------------------------------------------------------


instance {-# OVERLAPPABLE #-} Debug a => Debug [a] where
  debug xs = Debug.list "List" (map debug xs)

newtype ListLike (s :: Symbol) (ls :: Type) = ListLike {getListLike :: ls}

instance (Foldable f, Debug a, KnownSymbol name) => Debug (ListLike name (f a)) where
  debug (ListLike xs) =
    Debug.list ( Text.pack (symbolVal @name Proxy) ) ( map debug $ toList xs)

------------------------------------------------------------
---- | Map Types |------------------------------------------
------------------------------------------------------------

instance (Debug k, Debug v) => Debug (Map.Map k v) where
  debug = Debug.dict "Map" .  fmap f . Map.toList
    where
      f (k,v) = debug k :!: debug v


instance (Debug v) => Debug (IntMap.IntMap v) where
  debug = Debug.dict "IntMap" .  fmap f . IntMap.toList
    where
      f (k,v) = debug k :!: debug v

instance (Debug k, Debug v) => Debug (HashMap.HashMap k v) where
  debug = Debug.dict "HashMap" .  fmap f . HashMap.toList
    where
      f (k,v) = debug k :!: debug v

newtype DictLike (s :: Symbol) (ls :: Type) = DictLike {getDictLike :: ls}

instance (FoldableWithKey f, Debug v, Debug (Key f) , KnownSymbol name) => Debug (DictLike name (f v)) where
  debug (DictLike xs) =
      Debug.dict ( Text.pack (symbolVal @name Proxy) ) ( map f $ Key.toKeyedList xs)
    where
      f (k,v) = debug k :!: debug v

------------------------------------------------------------
--- | Generics |--------------------------------------------
------------------------------------------------------------

type GDebug :: (Type -> Type) -> Constraint
class GDebug f where
  gdebug :: f a -> Debug.Repr
  gdebugs :: f a -> [Debug.Repr]
  gdebugs = pure . gdebug

type GDebugRecord :: (Type -> Type) -> Constraint
class GDebugRecord f where
  grecord :: f a -> [Text.Text :!: Debug.Repr]

newtype Genericly a = Genericly {getGenericly :: a}

instance (Generic a, GDebug (Rep a)) => Debug (Genericly a) where
  debug = gdebug . from . getGenericly

instance GDebug f => GDebug (D1 s f) where
  gdebug (M1 f) = gdebug f

instance (Debug a) => (GDebug (K1 l a)) where
  gdebug (K1 c) = debug c

instance (GDebugRecord f, Constructor ('MetaCons n s 'True), Constructor ('MetaCons n s 'True))
  => GDebug (C1 ('MetaCons n s 'True) f) where
  gdebug c@(M1 f) = Debug.record (Text.pack (conName c)) (grecord f)

instance (GDebug f, Constructor ('MetaCons n s 'False)) => GDebug (C1 ('MetaCons n s 'False) f) where
  gdebug c@(M1 f) = Debug.constructor (Text.pack (conName c)) (gdebugs f)

instance (Selector s, GDebug f) => GDebugRecord (S1 s f) where
  grecord c@(M1 f) = [Text.pack (selName c) :!: gdebug f]

instance (GDebugRecord f, GDebugRecord g) => GDebugRecord (f :*: g) where
  grecord (f :*: g) = grecord f <> grecord g


instance (GDebug f, GDebug g) => GDebug (f :+: g) where
  gdebug  (L1 f)  = gdebug f
  gdebug  (R1 f)  = gdebug f
  gdebugs (L1 f)  = gdebugs f
  gdebugs (R1 f)  = gdebugs f
