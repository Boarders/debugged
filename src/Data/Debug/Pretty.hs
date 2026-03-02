{-# language RankNTypes #-}
module Data.Debug.Pretty where

-- debug
import Data.Debug.Type
import Data.Debug.Diff

-- prettyprinter
import Prettyprinter (pretty, Doc, (<+>))
import qualified Prettyprinter as Pretty


--containers
import Data.Tree (Tree(..))

-- split
import qualified Data.List.Split as Split (chunksOf) -- to do swap this out


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

prettyPrimitive :: PrimLabel -> Doc ann
prettyPrimitive = \case
    PrimInt     p -> pretty p
    PrimWord    p -> pretty p
    PrimBool    p -> pretty p
    PrimDouble  p -> pretty p
    PrimChar    p -> pretty p
    PrimText    p -> pretty p
    PrimNatural p -> pretty p
    PrimInteger p -> pretty p


prettyTreeTop :: DocOptions -> Tree Label -> Doc Annotation
prettyTreeTop (DocOptions lInd rInd dInd) ts@(Node label xs) = case label of
  List t | length xs >= lInd
             -> Pretty.vsep
                  [ Pretty.annotate AnnTopName (pretty t)
                  , Pretty.annotate AnnTop (rowLayout Pretty.list lInd xs)
                  ]
  Record t | length xs >= rInd
             ->Pretty.hsep
                  [ Pretty.annotate AnnTopName (pretty t)
                  , Pretty.annotate AnnTop (rowLayout prettyDict lInd xs)
                  ]
  Dict t | length xs >= dInd
             -> Pretty.vsep
                  [ Pretty.annotate AnnTopName (pretty t)
                  , Pretty.annotate AnnTop (rowLayout prettyDict dInd xs)
                  ]
  _          -> prettyTreeInternal ts

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
  Dict t ->
    Pretty.hsep [pretty t, Pretty.annotate AnnDict . Pretty.list . map prettyTreeInternal $ xs]    
  AssocProp ->
    let
      (key : [value]) = xs -- invariant: assocProp has two children
    in
      recordField (prettyTreeInternal key) (prettyTreeInternal value)


prettyNode :: Label -> [Doc ann] -> Doc ann
prettyNode label children = case label of
  Prim p -> prettyPrimitive p
  List t ->
    Pretty.hsep [pretty t, Pretty.list children]
  Record t ->
    Pretty.hsep [pretty t , prettyRecord children]
  Prop t ->
    recordField (pretty t) (head children)
  App t -> pretty t <+> Pretty.hsep children
  Opaque t -> pretty t <+> Pretty.hsep children
  Literal t -> pretty t
  Dict t -> pretty t <+> Pretty.list children
  AssocProp ->
    let
      [k, v] = children
    in
      recordField k v

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


--------------------------------------------------------------------------------
-------------------- | Pretty printing diffs -----------------------------------


data DiffAnnotation =
    AnnSame
  | AnnRemoved
  | AnnAdded



prettyDiffTree :: DiffTree -> Doc DiffAnnotation
prettyDiffTree (DiffTree tree) = Pretty.enclose "<" ">" (prettyDiffTreeTop defaultDocOptions tree)


prettyDiffTreeTop :: DocOptions -> Tree (Delta Label) -> Doc DiffAnnotation
prettyDiffTreeTop opts (Node node children) = case node of
  Same a ->
    prettyNode a (map (prettyDiffTreeTop opts) children)
  Different -> case children of
    [left, right] ->
      let
        leftTree  = Pretty.annotate AnnRemoved $ "-" <> prettyDiffTreeTop opts left
        rightTree = Pretty.annotate AnnAdded   $ "+" <> prettyDiffTreeTop opts right
      in
        Pretty.hsep [leftTree, rightTree]
    -- impossible case    
    _ -> mempty
    
  Extra1 ->
    case children of
      [c] -> Pretty.annotate AnnRemoved $ "-" <> prettyDiffTreeTop opts c
      -- impossible case
      _  -> mempty
  Extra2 ->
    case children of
      [c] -> Pretty.annotate AnnAdded $ "+" <> prettyDiffTreeTop opts c
      -- impossible case
      _  -> mempty    
  SubTree a ->
    prettyNode a (map (prettyDiffTreeTop opts) children)
