{-# language RankNTypes #-}
module Data.Debug.Pretty where

-- debug
import Data.Debug.Type

-- prettyprinter
import Prettyprinter (pretty, Doc, (<+>))
import qualified Prettyprinter as Pretty


--containers
import Data.Tree (Tree(..))
import qualified Data.Tree as Tree

-- split
import qualified Data.List.Split as Split (chunksOf) -- to do swap this out

-- base
import Data.List (transpose, intersperse)


data Annotation =
    AnnListlike
  | AnnRecord
  | AnnDict
  | AnnPrimitive
  | AnnConstructor
  | AnnTop
  | AnnTopName
  deriving (Eq, Show)


data DocOptions = DocOptions
  { listIndent   :: !Int
  , recordIndent :: !Int
  , dictIndent   :: !Int
  }
  deriving (Show, Eq, Ord)

defaultDocOptions :: DocOptions
defaultDocOptions = DocOptions 5 1 1

prettyRepr :: Repr -> Doc Annotation
prettyRepr (Repr tree) = Pretty.enclose "<" ">" (prettyTreeTop defaultDocOptions tree)

prettyPrimitive :: PrimLabel -> Doc Annotation
prettyPrimitive = \case
    PrimInt    p -> Pretty.annotate AnnPrimitive (pretty p)
    PrimBool   p -> Pretty.annotate AnnPrimitive (pretty p)
    PrimDouble p -> Pretty.annotate AnnPrimitive (pretty p)
    PrimChar   p -> Pretty.annotate AnnPrimitive (pretty p)
    PrimText   p -> Pretty.annotate AnnPrimitive (pretty p)

prettyTreeTop :: DocOptions -> Tree Label -> Doc Annotation
prettyTreeTop (DocOptions lInd rInd dInd) ts@(Node label xs) = case label of
  List t | length xs >= lInd
             -> Pretty.vsep
                  [ Pretty.annotate AnnTopName (pretty t)
                  , (Pretty.annotate AnnTop (rowLayout Pretty.list lInd xs))]
  Record t | length xs >= rInd
             ->Pretty.hsep
                  [ Pretty.annotate AnnTopName (pretty t)
                  , (Pretty.annotate AnnTop (rowLayout prettyDict lInd xs))
                  ]
  Dict t | length xs >= dInd
             -> Pretty.vsep
                  [ Pretty.annotate AnnTopName (pretty t)
                  , (Pretty.annotate AnnTop (rowLayout prettyDict dInd xs))
                  ]
  l          -> prettyTreeInternal ts

prettyTreeInternal :: Tree Label -> Doc Annotation
prettyTreeInternal (Node label xs) = case label of
  Prim p -> Pretty.annotate AnnPrimitive . prettyPrimitive $ p
  List t ->
    Pretty.hsep [pretty t, Pretty.annotate AnnListlike . Pretty.list . map prettyTreeInternal $ xs]
  Record t ->
    Pretty.hsep [pretty t , Pretty.annotate AnnRecord . prettyRecord . map prettyTreeInternal $ xs]
  Prop t ->
    let
      value = head xs -- invariant: a Prop should always have a single child
    in
      recordField (pretty t) (prettyTreeInternal value)

  App t -> Pretty.annotate AnnConstructor $ pretty t <+> Pretty.hsep (map prettyTreeInternal xs)
  Opaque t -> pretty t <+> Pretty.hsep (map prettyTreeInternal xs)
  Literal t -> pretty t
  Dict t -> pretty t <+> Pretty.hsep (map prettyTreeInternal xs)
  AssocProp ->
    let
      (key : [value]) = xs -- invariant: assocProp has two children
    in
      recordField (prettyTreeInternal key) (prettyTreeInternal value)


prettyRecord :: [Doc ann] -> Doc ann
prettyRecord = Pretty.group . Pretty.encloseSep (Pretty.flatAlt "{ " "{")
                                                (Pretty.flatAlt " }" "}")
                                                ", "

recordField :: Doc ann -> Doc ann -> Doc ann
recordField label value = label <> ":" <+> value



rowLayout :: (forall ann . [Doc ann] -> Doc ann) -> Int -> [Tree Label] -> Doc Annotation
rowLayout layout n = layout . map f .  Split.chunksOf n . map prettyTreeInternal
  where
    f = Pretty.concatWith (\a b -> a <> ", " <> b)


prettyDict :: [Doc ann] -> Doc ann
prettyDict = Pretty.group . Pretty.encloseSep (Pretty.flatAlt "{ " "{")
                                        (Pretty.flatAlt " }" "}")
                                        ", "
