{-# LANGUAGE FlexibleInstances #-}

module ShortCut.Core.Pretty (prettyShow) where

import Data.Scientific (Scientific())
import ShortCut.Core.Types
import Text.PrettyPrint.HughesPJClass

instance Pretty CutType where
  pPrint (CutType ext desc) = text ext <+> parens (text desc)
  pPrint (ListOf t) = text "list of" <+> pPrint t <> text "s"

instance Pretty CutVar where
  pPrint (CutVar s) = text s

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
  pPrint as = fsep $ map pPrint as

instance Pretty CutExpr where
  pPrint e@(CutLit _ s)
    | typeOf e == num       = text $ show $ (read s :: Scientific)
    | otherwise             = text $ show s
  pPrint (CutRef _ v)       = pPrint v
  pPrint (CutFun _ s es)    = text s <+> fsep (map pNested es)
  pPrint (CutList _ es)     = pList es
  pPrint (CutBop _ c e1 e2) = if (length $ render $ one) > 80 then two else one
    where
      bopWith fn = fn (pPrint e1) (nest (-2) (text c) <+> pPrint e2)
      one = bopWith (<+>)
      two = bopWith ($+$)

pList :: (Pretty a) => [a] -> Doc
pList es = text "[" <> fsep (punctuate (text ",") (map pPrint es)) <> text "]"

-- this adds parens around nested function calls
-- without it things can get really messy!
pNested :: CutExpr -> Doc
pNested e@(CutFun _ _ _  ) = parens $ pPrint e
pNested e@(CutBop _ _ _ _) = parens $ pPrint e
pNested e = pPrint e

instance Pretty CutConfig where
  pPrint cfg = vcat $ map (\(k,fn) -> text k <+> text "=" <+> text (fn cfg))
    [ ("script" , show . cfgScript )
    , ("tmpdir" , show . cfgTmpDir )
    , ("verbose", show . cfgVerbose)
    ]

-- TODO change this to something useful
instance Pretty CutFunction where
  pPrint fn = text $ "CutFunction '" ++ fName fn ++ "'"

instance Show CutFunction where
  show = prettyShow

-- TODO change this to something useful
instance Pretty CutModule where
  pPrint fn = text $ "CutModule '" ++ mName fn ++ "'"

instance Show CutModule where
  show = prettyShow
