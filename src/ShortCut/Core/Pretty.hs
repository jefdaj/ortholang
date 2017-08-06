{-# LANGUAGE FlexibleInstances #-}

module ShortCut.Core.Pretty
  ( prettyShow
  , prettyResult
  )
  where

import Data.Scientific            (Scientific())
import Development.Shake.FilePath ((</>))
import ShortCut.Core.Types
import Text.PrettyPrint.HughesPJClass

instance Pretty CutType where
  pPrint EmptyList  = text "empty list" -- TODO remove
  pPrint (ListOf t) = text "list of" <+> pPrint t <> text "s"
  pPrint t          = text (tExt t) <+> parens (text $ tDesc t)

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

-- TODO actual Eq instance, or what? how do we compare types?
instance Pretty CutExpr where
  pPrint e@(CutLit _ _ s)
    | typeOf e == num       = text $ show (read s :: Scientific)
    -- | tExt t == tExt num    = text $ show (read s :: Scientific)
    | otherwise             = text $ show s
  pPrint (CutRef _ _ _ v)       = pPrint v
  pPrint (CutFun _ _ _ s es)    = text s <+> fsep (map pNested es)
  pPrint (CutList _ _ _ es)     = pList es
  pPrint (CutBop _ _ _ c e1 e2) = if (length $ render $ one) > 80 then two else one
    where
      bopWith fn = fn (pPrint e1) (nest (-2) (text c) <+> pPrint e2)
      one = bopWith (<+>)
      two = bopWith ($+$)

pList :: (Pretty a) => [a] -> Doc
pList es = text "[" <> fsep (punctuate (text ",") (map pPrint es)) <> text "]"

-- this adds parens around nested function calls
-- without it things can get really messy!
pNested :: CutExpr -> Doc
pNested e@(CutFun  _ _ _ _ _  ) = parens $ pPrint e
pNested e@(CutBop  _ _ _ _ _ _) = parens $ pPrint e
pNested e = pPrint e

instance Pretty CutConfig where
  pPrint cfg = vcat $ map (\(k,fn) -> text k <+> text "=" <+> text (fn cfg))
    [ ("script" , show . cfgScript )
    , ("tmpdir" , show . cfgTmpDir )
    , ("verbose", show . cfgDebug)
    ]

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

-- TODO oh i finally get it!
--      nothing's wrong here. extract_seqs should be adding a layer of
--      indirection that it isn't adding: lists are lists of paths to literals,
--      not lists of literals themselves! (this is fairly annoying for writing
--      scripts though)

-- This seems to be separately required to show the final result of eval
-- TODO is there a way to get rid of it?
-- TODO rename prettyContents? prettyResult?
-- TODO should this actually open external programs
-- TODO idea for lists: if any element contains "\n", just add blank lines between them
-- TODO for str and num lists, showing should be like prettyShowing right?
prettyResult :: CutConfig -> CutType -> FilePath -> IO Doc
prettyResult _ EmptyList  _ = return $ text "[]"
prettyResult cfg (ListOf t) f = do
  paths    <- fmap lines $ readFile $ cfgTmpDir cfg </> f
  pretties <- mapM (prettyResult cfg t) paths
  return $ text "[" <> fsep ((punctuate (text ",") pretties)) <> text "]"
prettyResult cfg t f = do
  txt <- readFile $ cfgTmpDir cfg </> f
  tCat t $ txt
