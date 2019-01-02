{-# LANGUAGE FlexibleInstances #-}

module Detourrr.Core.Pretty
  ( prettyShow
  -- , prettyResult (moved to Eval)
  , writeScript
  , DtrExpr(..)
  , prettyNum -- TODO get rid of this?

  -- re-export for convenience
  , Pretty()
  , render
  , renderIO
  , pPrint
  , pPrintHdl
  )
  where

import Data.Scientific            (Scientific(), toBoundedInteger)
-- import Development.Shake.FilePath ((</>))
import Detourrr.Core.Types
import Detourrr.Core.Config (showConfig)
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
renderIO :: DtrConfig -> Doc -> IO String
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
pPrintHdl :: Pretty a => DtrConfig -> Handle -> a -> IO ()
pPrintHdl cfg hdl thing = renderIO cfg (pPrint thing) >>= hPutStrLn hdl

instance Pretty DtrType where
  pPrint Empty          = error "should never need to print Empty"
  pPrint (ListOf Empty) = text "empty list"
  pPrint (ListOf     t) = text "list of" <+> pPrint t <> text "s"
  pPrint (ScoresOf   t) = text "list of" <+> pPrint t <> text "s with scores"
  pPrint t              = text (tExt t) <+> parens (text $ tDesc t)

instance Pretty DtrVar where
  pPrint (DtrVar s) = text s

-- TODO add descriptions here? if so, need to separate actual extension code
-- instance Pretty Ext where
--   pPrint (ListOf e) = pPrint e <> text "s"
--   pPrint (Ext   e) = text e

instance {-# OVERLAPPING #-} Pretty DtrAssign where
  pPrint (v, e) = pPrint v <+> text "=" <+> pPrint e
  -- this adds type info, but makes the pretty-print not valid source code
  -- pPrint (v, e) = text (render (pPrint v) ++ "." ++ render (pPrint $ typeExt e))

instance {-# OVERLAPPING #-} Pretty DtrScript where
  pPrint [] = empty
  -- pPrint as = text $ unlines $ map prettyShow as
  pPrint as = vcat $ map pPrint as

-- TODO move to a "files/io" module along with debug fns?
-- TODO use safe write here?
writeScript :: FilePath -> DtrScript -> IO ()
writeScript path scr = writeFile path $ unlines $ map prettyShow scr

-- TODO actual Eq instance, or what? how do we compare types?
instance Pretty DtrExpr where
  pPrint e@(DtrLit _ _ s)
    | typeOf e == num = prettyNum s
    | otherwise = text $ show s
  pPrint (DtrRef _ _ _ v)    = pPrint v
  pPrint (DtrFun _ _ _ s es) = text s <+> sep (map pNested es)
  pPrint (DtrList _ _ _ es)  = pList es
  pPrint (DtrRules (CompiledExpr e _)) = pPrint e -- TODO is this right?

  -- this is almost right except it breaks lines too early (always nesting),
  -- which looks super weird for short bops:
  -- pPrint (DtrBop _ _ _ c e1 e2) = pPrint e1 $$ nest (-2) (text c) $$ pPrint e2

  -- this one is a little better: the first line is right and *then* it starts doing that
  -- TODO ask on stackoverflow if there's any better way, but later
  pPrint (DtrBop _ _ _ c e1 e2) = sep $ punctuate (text $ " " ++ c) [pPrint e1, pPrint e2]

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
pNested :: DtrExpr -> Doc
pNested e@(DtrFun  _ _ _ _ _  ) = parens $ pPrint e
pNested e@(DtrBop  _ _ _ _ _ _) = parens $ pPrint e
pNested e = pPrint e

-- TODO update this by mapping over the fields
instance Pretty DtrConfig where
  pPrint = text . showConfig

-- TODO change this to something useful
instance Pretty DtrFunction where
  pPrint fn = text $ "DtrFunction '" ++ fName fn ++ "'"

instance Show DtrFunction where
  show = prettyShow

-- TODO change this to something useful
instance Pretty DtrModule where
  pPrint fn = text $ "DtrModule '" ++ mName fn ++ "'"

-- instance Show DtrModule where
  -- show = prettyShow
