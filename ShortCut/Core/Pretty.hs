{-# LANGUAGE FlexibleInstances #-}

module ShortCut.Core.Pretty
  ( prettyShow
  -- , prettyResult (moved to Eval)
  , writeScript
  , CutExpr(..)
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
import ShortCut.Core.Types
import ShortCut.Core.Config (showConfig)
import Text.PrettyPrint.HughesPJClass
import System.Console.Terminal.Size (Window(..), size)
import System.IO (Handle, hPutStrLn)
-- import Control.Monad.Trans (liftIO)
-- import Data.String.Utils          (replace)

getWidth :: IO Int
getWidth = do
  s <- size
  return $ case s of
    Nothing -> 120
    Just (Window {width = w}) -> w

-- Render with my custom style (just width so far)
-- Needs to have the optional constant width for the REPL tests
renderIO :: CutConfig -> Doc -> IO String
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
pPrintHdl :: Pretty a => CutConfig -> Handle -> a -> IO ()
pPrintHdl cfg hdl thing = renderIO cfg (pPrint thing) >>= hPutStrLn hdl

instance Pretty CutType where
  pPrint Empty          = error "should never need to print Empty"
  pPrint (ListOf Empty) = text "empty list"
  pPrint (ListOf     t) = text "list of" <+> pPrint t <> text "s"
  pPrint (ScoresOf   t) = text "list of" <+> pPrint t <> text "s with scores"
  pPrint t              = text (tExt t) <+> parens (text $ tDesc t)

instance Pretty CutVar where
  pPrint (CutVar _ s) = text s -- TODO show the seed?

-- TODO add descriptions here? if so, need to separate actual extension code
-- instance Pretty Ext where
--   pPrint (ListOf e) = pPrint e <> text "s"
--   pPrint (Ext   e) = text e

instance {-# OVERLAPPING #-} Pretty CutAssign where
  pPrint (v, e) = pPrint v <+> text "=" <+> pPrint e
  -- this adds type info, but makes the pretty-print not valid source code
  -- pPrint (v, e) = text (render (pPrint v) ++ "." ++ render (pPrint $ typeExt e))

instance {-# OVERLAPPING #-} Pretty CutScript where
  pPrint [] = empty
  -- pPrint as = text $ unlines $ map prettyShow as
  pPrint as = vcat $ map pPrint as

-- TODO move to a "files/io" module along with debug fns?
-- TODO use safe write here?
writeScript :: FilePath -> CutScript -> IO ()
writeScript path scr = writeFile path $ unlines $ map prettyShow scr

-- TODO actual Eq instance, or what? how do we compare types?
instance Pretty CutExpr where
  pPrint e@(CutLit _ _ s)
    | typeOf e == num = prettyNum s
    | otherwise = text $ show s
  pPrint (CutRef _ _ _ v)    = pPrint v
  pPrint (CutFun _ _ _ s es) = text s <+> sep (map pNested es)
  pPrint (CutList _ _ _ es)  = pList es
  pPrint (CutRules (CompiledExpr e _)) = pPrint e -- TODO is this right?

  -- this is almost right except it breaks lines too early (always nesting),
  -- which looks super weird for short bops:
  -- pPrint (CutBop _ _ _ c e1 e2) = pPrint e1 $$ nest (-2) (text c) $$ pPrint e2

  -- this one is a little better: the first line is right and *then* it starts doing that
  -- TODO ask on stackoverflow if there's any better way, but later
  pPrint (CutBop _ _ _ c e1 e2) = sep $ punctuate (text $ " " ++ c) [pPrint e1, pPrint e2]

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
pNested :: CutExpr -> Doc
pNested e@(CutFun  _ _ _ _ _  ) = parens $ pPrint e
pNested e@(CutBop  _ _ _ _ _ _) = parens $ pPrint e
pNested e = pPrint e

-- TODO update this by mapping over the fields
instance Pretty CutConfig where
  pPrint = text . showConfig

-- TODO change this to something useful
instance Pretty CutFunction where
  pPrint fn = text $ "CutFunction '" ++ fName fn ++ "'"

instance Show CutFunction where
  show = prettyShow

-- TODO change this to something useful
instance Pretty CutModule where
  pPrint fn = text $ "CutModule '" ++ mName fn ++ "'"

-- instance Show CutModule where
  -- show = prettyShow
