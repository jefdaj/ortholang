{-# LANGUAGE FlexibleInstances #-}

module OrthoLang.Core.Pretty
  ( prettyShow
  -- , prettyResult (moved to Eval)
  , writeScript
  , OrthoLangExpr(..)
  , prettyNum -- TODO get rid of this?

  -- re-export for convenience
  , Pretty()
  , render
  , renderIO
  , pPrint
  , pPrintHdl
  )
  where

import Prelude hiding ((<>))
import Data.Scientific            (Scientific(), toBoundedInteger)
-- import Development.Shake.FilePath ((</>))
import OrthoLang.Core.Types
import OrthoLang.Core.Config (showConfig)
import Text.PrettyPrint.HughesPJClass
import System.Console.Terminal.Size (Window(..), size)
import System.IO (Handle, hPutStrLn)
-- import Control.Monad.Trans (liftIO)
-- import Data.String.Utils          (replace)
import Test.Tasty.Golden (writeBinaryFile)

getWidth :: IO Int
getWidth = do
  s <- size
  return $ case s of
    Nothing -> 120
    Just (Window {width = w}) -> w

-- Render with my custom style (just width so far)
-- Needs to have the optional constant width for the REPL tests
renderIO :: OrthoLangConfig -> Doc -> IO String
renderIO cfg doc = do
  currentWidth <- getWidth
  let renderWidth = case cfgWidth cfg of
                      Nothing -> currentWidth
                      Just w  -> w
  let s = style {lineLength = renderWidth, ribbonsPerLine = 0.2}
  -- let s = style {lineLength = renderWidth}
  return $ renderStyle s doc

-- Print something pretty to a handle, rendering with custom style from Pretty.hs
-- TODO move to Pretty.hs?
pPrintHdl :: Pretty a => OrthoLangConfig -> Handle -> a -> IO ()
pPrintHdl cfg hdl thing = renderIO cfg (pPrint thing) >>= hPutStrLn hdl

instance Pretty OrthoLangType where
  pPrint Empty          = error "should never need to print Empty"
  pPrint (ListOf Empty) = text "empty list"
  pPrint (ListOf     t) = text "list of" <+> pPrint t <> text "s"
  pPrint (ScoresOf   t) = text "list of" <+> pPrint t <> text "s with scores"
  pPrint (OrthoLangTypeGroup {tgExt = t, tgDesc = d}) = text t <+> parens (text d)
  pPrint (OrthoLangType      { tExt = t,  tDesc = d}) = text t <+> parens (text d)

instance Pretty OrthoLangVar where
  pPrint (OrthoLangVar _ s) = text s -- TODO show the salt?

-- TODO add descriptions here? if so, need to separate actual extension code
-- instance Pretty Ext where
--   pPrint (ListOf e) = pPrint e <> text "s"
--   pPrint (Ext   e) = text e

instance {-# OVERLAPPING #-} Pretty OrthoLangAssign where
  pPrint (v, e) = pPrint v <+> text "=" <+> pPrint e
  -- this adds type info, but makes the pretty-print not valid source code
  -- pPrint (v, e) = text (render (pPrint v) ++ "." ++ render (pPrint $ typeExt e))

instance {-# OVERLAPPING #-} Pretty OrthoLangScript where
  pPrint [] = empty
  -- pPrint as = text $ unlines $ map prettyShow as
  pPrint as = vcat $ map pPrint as

-- TODO move to a "files/io" module along with debug fns?
-- TODO use safe write here?
writeScript :: OrthoLangConfig -> OrthoLangScript -> FilePath -> IO ()
writeScript cfg scr path = do
  txt <- renderIO cfg $ pPrint scr
  writeBinaryFile path txt

-- TODO actual Eq instance, or what? how do we compare types?
instance Pretty OrthoLangExpr where
  pPrint e@(OrthoLangLit _ _ s)
    | typeOf e == num = prettyNum s
    | otherwise = text $ show s
  pPrint (OrthoLangRef _ _ _ v)    = pPrint v
  pPrint (OrthoLangFun _ _ _ s es) = text s <+> sep (map pNested es)
  pPrint (OrthoLangList _ _ _ es)  = pList es
  pPrint (OrthoLangRules (CompiledExpr t (ExprPath p) _)) = text $ "Compiled " ++ extOf t ++ " " ++ p

  -- this is almost right except it breaks lines too early (always nesting),
  -- which looks super weird for short bops:
  -- pPrint (OrthoLangBop _ _ _ c e1 e2) = pPrint e1 $$ nest (-2) (text c) $$ pPrint e2

  -- this one is a little better: the first line is right and *then* it starts doing that
  -- TODO ask on stackoverflow if there's any better way, but later
  pPrint (OrthoLangBop _ _ _ c e1 e2) = sep $ punctuate (text $ " " ++ c) [pPrint e1, pPrint e2]

pList :: (Pretty a) => [a] -> Doc
pList es = text "[" <> sep (punctuate (text ",") (map pPrint es)) <> text "]"

prettyNum :: String -> Doc
prettyNum s = text $
  case toBoundedInteger n of
    Just i  -> show (i :: Int)
    Nothing -> show n -- as decimal
  where
    n = read s :: Scientific

-- this adds parens around nested function calls
-- without it things can get really messy!
pNested :: OrthoLangExpr -> Doc
pNested e@(OrthoLangFun  _ _ _ _ _  ) = parens $ pPrint e
pNested e@(OrthoLangBop  _ _ _ _ _ _) = parens $ pPrint e
pNested e = pPrint e

-- TODO update this by mapping over the fields
instance Pretty OrthoLangConfig where
  pPrint = text . showConfig

-- TODO change this to something useful
instance Pretty OrthoLangFunction where
  pPrint fn = text $ "OrthoLangFunction '" ++ head (fNames fn) ++ "'"

instance Show OrthoLangFunction where
  show = prettyShow

-- TODO change this to something useful
instance Pretty OrthoLangModule where
  pPrint fn = text $ "OrthoLangModule '" ++ mName fn ++ "'"

-- instance Show OrthoLangModule where
  -- show = prettyShow
