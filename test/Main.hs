{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module Main where

-- tasty
import Test.Tasty

-- tasty-quickcheck
import Test.Tasty.QuickCheck as QC

-- tasty-hunit
import Test.Tasty.HUnit      as HU

-- prettyprinter
import Prettyprinter
import Prettyprinter.Render.Text

-- Debugged
import Data.Debug.Class
import Data.Debug.Pretty
import Data.Debug.Diff

-- base
import System.IO

-- containers
import qualified Data.Map as Map
import qualified Data.Tree as Tree

-- unordered-containers
import qualified Data.HashMap.Strict as HashMap

-- prettyprinter-ansi-ternimal
import Prettyprinter.Render.Terminal (color, Color(..))
import qualified Prettyprinter.Render.Terminal as Render



main :: IO ()
main = do
  putDoc . prettyRepr . debug $ ([1,2,3] :: [Int])
--  putDoc . prettyRepr . debug $ (exL)
  putStrLn ""
  putStrLn ""
  renderIO stdout (layoutSmart defaultLayoutOptions . prettyRepr . debug $ exL)
  putStrLn ""
  putStrLn ""
  renderIO stdout (layoutSmart defaultLayoutOptions . prettyRepr . debug $ exM)
  putStrLn ""
  putStrLn ""
  Render.renderIO stdout (fmap addColors . layoutSmart defaultLayoutOptions . prettyRepr . debug $ exHM)
  putStrLn ""
  putStrLn ""
  Render.renderIO stdout (fmap addColors. layoutSmart defaultLayoutOptions . prettyRepr . debug $ exL)
  putStrLn ""
  putStrLn ""
  Render.renderIO stdout (fmap addColors. layoutSmart defaultLayoutOptions . prettyRepr . debug $ exX)
  putStrLn ""
  putStrLn ""
  Render.renderIO stdout (fmap addColors. layoutSmart defaultLayoutOptions . prettyRepr . debug $ exRev)
  putStrLn ""
  putStrLn ""
  putStrLn $ "diff of " <> (show exX) <> " " <> (show exX')
  let diff = (diffRepr (debug exX) (debug exX'))
--  putStrLn $
--    Tree.drawTree . fmap show . \case {DiffTree t -> t}$ 
--    diff
--
  putStrLn ""
  putStrLn ""
  Render.renderIO stdout (fmap diffColors. layoutSmart defaultLayoutOptions . prettyDiffTree $ diff)
  putStrLn ""
  putStrLn ""

exX :: X
exX = X 3 4 [5, 6, 7, 50]

exX' :: X
exX' = X 3 4 [1, 6, 7]

exRev :: RevList String
exRev = RevList ["kitty!", "there", "ahoy"]

exM :: Map.Map String (Maybe Int)
exM = Map.fromList keyVals

exHM :: HashMap.HashMap String (Maybe Int)
exHM = HashMap.fromList keyVals

exL :: [Int]
exL = [1..100]

keyVals :: [(String, Maybe Int)]
keyVals = [("Bashful", Just 1), ("Grumpy", Just 2), ("Schro", Nothing), ("Baby Cat", Just 4), ("Nill Nill", Just 5), ("Max", Just 6)]
--  defaultMain tests

addColors :: Annotation -> Render.AnsiStyle
addColors = \case
  AnnListlike -> color Red
  AnnRecord -> color Cyan
  AnnDict -> color Magenta
  AnnPrimitive -> color Green
  AnnConstructor -> color Blue
  AnnTop -> color Cyan
  AnnTopName -> color Magenta

diffColors :: DiffAnnotation -> Render.AnsiStyle
diffColors = \case
  AnnSame    -> color White
  AnnRemoved -> color Red
  AnnAdded   -> color Green





{-
tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]


properties :: TestTree
properties =
  testGroup "Property Tests"
    $ locallyNamelessProperties <> churchProperties
  
locallyNamelessProperties :: [TestTree]
locallyNamelessProperties =
   [ QC.testProperty
     "fromLocallyNameless ∘ toLocallyNameless === id"
     (withMaxSuccess 1000 $ fromLocallyNamelessLeftInverse @String)
  ]

churchProperties :: [TestTree]
churchProperties = 
  [ QC.testProperty
      "Addition of Church numerals is commutative"
     (withMaxSuccess 100 additionIsCommutative)
  , QC.testProperty
      "Multiplication of Church numerals is commutative"
     (withMaxSuccess 100 multiplicationIsCommutative)
  ]
unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ HU.testCase
      "two plus two is four" $
       nf (churchAdd .$ cTwo .$ cTwo) @?= cFour
  , HU.testCase
      "two times three is six" $
       nf (churchMult .$ cTwo .$ cThree) @?= cSix
  , HU.testCase
      "two times three is six" $
       nf (churchMult .$ cTwo .$ cThree) @?= cSix
  ]
  
fromLocallyNamelessLeftInverse :: (Ord a, Show a) => Term a -> Property
fromLocallyNamelessLeftInverse e =
  (fromLocallyNameless . toLocallyNameless) e === e

additionIsCommutative :: Nat -> Nat -> Property
additionIsCommutative n m =
      nf (churchAdd .$ fromNat n .$ fromNat m)
  === nf (churchAdd .$ fromNat m .$ fromNat n)


multiplicationIsCommutative :: Nat -> Nat -> Property
multiplicationIsCommutative n m =
      nf (churchMult .$ fromNat n .$ fromNat m)
  === nf (churchMult .$ fromNat m .$ fromNat n)
  
--}
