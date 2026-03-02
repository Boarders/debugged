module Data.Debug.IO where

import "this" Data.Debug.Type
import "this" Data.Debug.Class
import "this" Data.Debug.Pretty
import "this" Data.Debug.Generics
import "this" Data.Debug.Diff

import "base" System.IO

import "prettyprinter" Prettyprinter
import "prettyprinter" Prettyprinter.Render.Text

-- prettyprinter-ansi-ternimal
import Prettyprinter.Render.Terminal (color, Color(..))
import qualified Prettyprinter.Render.Terminal as Render


defaultAnnColours :: Annotation -> Render.AnsiStyle
defaultAnnColours = \case
  AnnListlike -> color Red
  AnnRecord -> color Cyan
  AnnDict -> color Magenta
  AnnPrimitive -> color Green
  AnnConstructor -> color Blue
  AnnTop -> color Cyan
  AnnTopName -> color Magenta

defaultDiffColours :: DiffAnnotation -> Render.AnsiStyle
defaultDiffColours = \case
  AnnSame    -> color White
  AnnRemoved -> color Red
  AnnAdded   -> color Green

debugDoc :: Debug a => a -> Doc Annotation
debugDoc = prettyRepr . debug

diffDoc :: (Debug a, Debug b) => a -> b -> Doc DiffAnnotation
diffDoc a b = prettyDiffTree (diffRepr (debug a) (debug b))

ansiDebug :: Debug a => a -> IO ()
ansiDebug = Render.renderIO stdout . ansiDebugWith defaultAnnColours defaultLayoutOptions

ansiDebugWith :: Debug a =>
  (Annotation -> Render.AnsiStyle) ->
  LayoutOptions ->
  a -> SimpleDocStream Render.AnsiStyle
ansiDebugWith withAnns layoutOpts =
    fmap withAnns
  . layoutSmart layoutOpts
  . debugDoc

-- ansiDiff :: (Debug a, Debug b) => a -> b -> IO ()
-- ansiDiff a b =
--   Render.renderIO stdout . ansiDebugWith defaultAnnColours defaultLayoutOptions

ansiDiffWith :: (Debug a, Debug b) =>
  (DiffAnnotation -> Render.AnsiStyle) ->
  LayoutOptions ->
  a -> b -> SimpleDocStream Render.AnsiStyle
ansiDiffWith withAnns layoutOpts a =
    fmap withAnns
  . layoutSmart layoutOpts
  . diffDoc a 

putDebugWith :: Debug a => a -> IO ()
putDebugWith = undefined

debugDocStream :: Debug a => a -> SimpleDocStream t
debugDocStream = undefined
  -- Render.renderIO stdout (fmap addColors. layoutSmart defaultLayoutOptions . prettyRepr . debug $ exRev)
