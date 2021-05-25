module Data.Debug.Type where

-- containers
import Data.Tree (Tree(..))

-- text
import Data.Text (Text)

-- base
import Data.Functor.Classes(Ord1(..))
import Data.Foldable

-- strict
import Data.Strict.Tuple

prune :: forall a. a -> (a -> Bool) -> Int -> Tree a -> Tree a
prune replacement counts depth = go (max 1 depth)
  where
  -- If we've reached a leaf anyway, just print it
  go 0 n@(Node _ []) =
    n
  go 0 _ =
    Node replacement []
  go d (Node label children) =
    let
      d' = if counts label then d-1 else d
    in
      Node label (map (go d') children)


newtype Repr = Repr {getRepr :: Tree Label}
  deriving stock (Eq, Show)

instance Ord Repr where
  compare (Repr t1) (Repr t2) =
    liftCompare compare t1 t2

data PrimLabel =
    PrimInt    Int
  | PrimBool   Bool
  | PrimDouble Double
  | PrimChar   Char
  | PrimText   Text
  deriving (Eq, Ord, Show)

pattern Int :: Int -> Label
pattern Int n = Prim (PrimInt n)

pattern Bool :: Bool -> Label
pattern Bool n = Prim (PrimBool n)

pattern Double :: Double -> Label
pattern Double n = Prim (PrimDouble n)

pattern Char :: Char -> Label
pattern Char n = Prim (PrimChar n)

pattern Text :: Text -> Label
pattern Text n = Prim (PrimText n)

-- | Labels for a tree which encodes a debugging representation of some
-- | PureScript value.
data Label
  -- Note that Repr trees are not correct-by-construction by default; whether
  -- or not children are permitted at a given node depends on the constructor
  -- of the label. We do however use newtypes, and we avoid exporting
  -- constructors in favour of specialised constructor functions, in order to
  -- ensure that all trees constructed by the exposed API are guaranteed to
  -- be valid.

  -- These labels should always be leaves, and should only ever be used to
  -- represent the Prim types they contain.
  = Prim PrimLabel

  -- This label represents a "flat" Foldable value type. It's children
  -- should be the contents of the array.
  | List Text

  -- This label represents a record value . Its immediate
  -- children should all have Prop labels.
  | Record Text

  -- This node represents a property with the given name. It should only occur
  -- as a direct descendent of a Record- or Opaque-labelled node.  A node
  -- with this label should always have exactly one child: the property value.
  | Prop Text

  -- Function application; mostly intended for use with data constructors.
  -- Children of nodes with this label represent the arguments to the
  -- function/data constructor.
  | App Text

  -- These constructors are for representations of opaque data types, or data
  -- types where this representation is more helpful than the 'obvious'
  -- representation (e.g. List).

  -- This constructor represents an opaque data type such as `(->)` or `Ref`.
  -- The argument should contain the name of the data type. Nodes with this
  -- label may have a number of Prop labels as immediate children, or
  -- alternatively a single Literal label.
  | Opaque Text

  -- This constructor should only ever appear as an immediate child of an
  -- `Opaque` constructor, and should always be a leaf node. It is useful for
  -- types which have an obvious representation which doesn't fit into any of
  -- the other options for constructing Repr values. For example, dates or
  -- times may want to use this constructor in order to display as e.g. <Date:
  -- 2019-02-01> or <Time: 12:00? respectively.
  | Literal Text

  -- This constructor is for map-like collections. The children are the
  -- contents; every direct child should have an AssocProp label.
  | Dict Text

  -- This constructor is for the key-value pairs of map-like collections. Each
  -- node with this label should have exactly two children: the key and the
  -- value.
  | AssocProp
  deriving (Eq, Ord, Show)

-- |
-- This function is used in diffing to determine if the label
-- is important or just a marker for the start of a structure
-- e.g.
--
isStructureLabel :: Label -> Bool
isStructureLabel = \case
  AssocProp -> True
  List _    -> True
  Record _  -> True
  _         -> False

fuzzyEqual :: Label -> Label -> Bool
fuzzyEqual = (==)

mkLeaf :: Label -> Repr
mkLeaf label = Repr (Node label [])

int :: Int -> Repr
int = mkLeaf . Int

double :: Double -> Repr
double = mkLeaf . Double

bool :: Bool -> Repr
bool = mkLeaf . Bool

char :: Char -> Repr
char = mkLeaf . Char

text :: Text -> Repr
text = mkLeaf . Text

list :: Text -> [Repr] -> Repr
list name = Repr . Node (List name) . map getRepr

record :: Text -> [(Text :!: Repr)] -> Repr
record name = Repr . Node (Record name) . makeProps


makeProps :: [(Text :!: Repr)] -> [Tree Label]
makeProps = map mkProp . toList
  where
    mkProp :: Text :!: Repr -> Tree Label
    mkProp (name :!: (Repr val)) =
      Node (Prop name) [val]

constructor :: Text -> [Repr] -> Repr
constructor name args =
  Repr (Node (App name) (map getRepr args))


opaque :: Text -> Repr -> Repr
opaque name child =
  Repr ((Node (Opaque name)) [getRepr child])


opaque_ :: Text -> Repr
opaque_ name =
  Repr (Node (Opaque name) [])


opaqueLiteral :: Text -> Text -> Repr
opaqueLiteral name val =
  Repr (Node (Opaque name) [Node (Literal val) []])


dict :: Text -> [Repr :!: Repr] -> Repr
dict name contents =
  Repr (Node (Dict name) (map mkAssocProp contents))
  where
    mkAssocProp :: Repr :!: Repr -> Tree Label
    mkAssocProp ((Repr k) :!: (Repr v)) = Node AssocProp [k, v]


addsDepth :: Label -> Bool
addsDepth = \case
  Prop _    -> False
  AssocProp -> False
  _         -> True


labelIsUnimportant :: Label -> Bool
labelIsUnimportant = \case
  AssocProp -> True
  List _      -> True
  Record _    -> True
  _           -> False

