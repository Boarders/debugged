module Data.Debug.Generics where

import qualified "this" Data.Debug.Type as Debug

import "base" GHC.Generics
import "base" Data.Kind

import qualified "text" Data.Text as Text

import "strict" Data.Strict.Tuple
import "this" Data.Debug.Class
  
------------------------
-- Generics Instances -- 
------------------------

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
