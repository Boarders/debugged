module Data.Debug.PrettyPrinter where

-- debug
import Data.Debug.Type

-- prettyprinter
import Prettyprinter (pretty, Doc, (<+>))
import qualified Prettyprinter as Pretty


--containers
import Data.Tree (Tree(..))
import qualified Data.Tree as Tree

-- text
import Data.Text (Text)
import qualified Data.Text as Text

-- split
import qualified Data.List.Split as Split (chunksOf) -- to do swap this out


{-
data LayoutOptions = LayoutOptions
  { listIndent   :: !Int
  , recordIndent :: !Int
  , dictIndent   :: !Int
  }
  deriving (Show, Eq, Ord)

data Layout t =
    Indent t (Layout t)
  | TopList   [[(Layout t)]]
  | TopRecord [[(Layout t)]]
  | TopDict   [[Layout t]]
  | InternalRow Char Char [(Layout t)]
  | LayoutPrim t
  | Constructor Text [Layout t]
  | RecKeyValue Text (Layout t)
  | DictKeyValue (Layout t) (Layout t)
  | TextLit Text
  deriving (Eq, Ord, Functor)

layoutPrim :: PrimLabel -> Layout PrimLabel
layoutPrim = LayoutPrim


layoutTop :: LayoutOptions -> Tree Label -> Layout PrimLabel
layoutTop (LayoutOptions lInd rInd dInd) (Node label xs) = case label of
  Prim p -> layoutPrim p
  Record -> TopRecord (getRows rInd xs)
  Prop t ->
    let
      value = head xs -- invariant: a Prop should always have a single child
    in
      RecKeyValue t (layoutInternal value)
  List -> TopList (getRows lInd xs)
  App t -> Constructor t (map layoutInternal xs)
  Opaque t -> Constructor t (map layoutInternal xs)
  Literal t -> TextLit t
  Assoc t -> TopDict (getRows dInd xs)
  AssocProp ->
    let
      (key : [value]) = xs -- invariant: assocProp has two children
    in
      DictKeyValue (layoutInternal key) (layoutInternal value)

layoutInternal :: Tree Label -> Layout PrimLabel
layoutInternal (Node label xs) = case label of
  Prim p -> layoutPrim p
  Record -> InternalRow '{' '}' (map layoutInternal xs)
  Prop t ->
    let
      value = head xs -- invariant: a Prop should always have a single child
    in
      RecKeyValue t (layoutInternal value)
  List ->  InternalRow '[' ']' (map layoutInternal xs)
  App t -> Constructor t (map layoutInternal xs)
  Opaque t -> Constructor t (map layoutInternal xs)
  Literal t -> TextLit t
  Assoc t -> InternalRow '{' '}' (map layoutInternal xs)
  AssocProp ->
    let
      (key : [value]) = xs -- invariant: assocProp has two children
    in
      DictKeyValue (layoutInternal key) (layoutInternal value)  


getRows :: Int -> [Tree Label] -> [[Layout PrimLabel]]
getRows n = Split.chunksOf n . map layoutInternal

-}
  
{-
prettyRepr :: Repr -> undefined
prettyRepr (Repr tree) = Pretty.enclose "<" ">" (prettyTreeLabel tree)

prettyTreeLabel :: Tree Label -> Doc ann
prettyTreeLabel (Node label xs) = case label of
  Int n -> pretty n
  Double d -> pretty d
  Bool b -> pretty b
  Char c -> pretty c
  Text t -> pretty t
  List -> Pretty.list . map prettyTreeLabel $ xs
  Record -> prettyRecord . map prettyTreeLabel $ xs
  Prop t ->
    let
      value = head xs -- invariant: a Prop should always have a single child
    in
      recordField (pretty t) (prettyTreeLabel value)

  App t -> pretty t <+> Pretty.hsep (map prettyTreeLabel xs)
  Opaque t -> pretty t <+> Pretty.hsep (map prettyTreeLabel xs)
  Literal t -> pretty t
  Assoc t -> pretty t <+> Pretty.hsep (map prettyTreeLabel xs)
  AssocProp ->
    let
      (key : [value]) = xs -- invariant: assocProp has two children
    in
      recordField (prettyTreeLabel key) (prettyTreeLabel value)


prettyRecord :: [Doc ann] -> Doc ann
prettyRecord = Pretty.group . Pretty.encloseSep (Pretty.flatAlt "{ " "{")
                                                (Pretty.flatAlt " }" "}")
                                                ", "

recordField :: Doc ann -> Doc ann -> Doc ann
recordField label value = label <> ":" <+> value
-}
