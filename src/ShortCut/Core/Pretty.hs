{-# LANGUAGE FlexibleInstances #-}

module ShortCut.Core.Pretty
  ( prettyShow
  , prettyResult
  , writeScript
  , CutExpr(..)
  )
  where

import Data.Scientific            (Scientific())
import Development.Shake.FilePath ((</>))
import ShortCut.Core.Types
import ShortCut.Core.Config (showConfig)
import Text.PrettyPrint.HughesPJClass
-- import Control.Monad.Trans (liftIO)

instance Pretty CutType where
  pPrint EmptySet  = text "empty set" -- TODO remove
  pPrint (SetOf t) = text "set of" <+> pPrint t <> text "s"
  pPrint t          = text (tExt t) <+> parens (text $ tDesc t)

instance Pretty CutVar where
  pPrint (CutVar s) = text s

-- TODO add descriptions here? if so, need to separate actual extension code
-- instance Pretty Ext where
--   pPrint (SetOf e) = pPrint e <> text "s"
--   pPrint (Ext   e) = text e

instance {-# OVERLAPPING #-} Pretty CutAssign where
  pPrint (v, e) = pPrint v <+> text "=" <+> pPrint e
  -- this adds type info, but makes the pretty-print not valid source code
  -- pPrint (v, e) = text (render (pPrint v) ++ "." ++ render (pPrint $ typeExt e))

instance {-# OVERLAPPING #-} Pretty CutScript where
  pPrint [] = empty
  -- pPrint as = text $ unlines $ map prettyShow as
  pPrint as = fsep $ map pPrint as

-- TODO move to a "files/io" module along with debug fns?
writeScript :: FilePath -> CutScript -> IO ()
writeScript path scr = writeFile path $ unlines $ map prettyShow scr

-- TODO actual Eq instance, or what? how do we compare types?
instance Pretty CutExpr where
  pPrint e@(CutLit _ _ s)
    | typeOf e == num = text $ show (read s :: Scientific)
    | otherwise       = text $ show s
  pPrint (CutRef _ _ _ v)    = pPrint v
  pPrint (CutFun _ _ _ s es) = text s <+> fsep (map pNested es)
  pPrint (CutSet _ _ _ es)  = pList es
  pPrint (CutBop _ _ _ c e1 e2) = if (length $ render $ one) > 80 then two else one
    where
      bopWith fn = fn (pPrint e1) (nest (-2) (text c) <+> pPrint e2)
      one = bopWith (<+>)
      two = bopWith ($+$)

pList :: (Pretty a) => [a] -> Doc
pList es = text "{" <> fsep (punctuate (text ",") (map pPrint es)) <> text "}"

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

-- This seems to be separately required to show the final result of eval
-- TODO is there a way to get rid of it?
-- TODO rename prettyContents? prettyResult?
-- TODO should this actually open external programs
-- TODO idea for sets: if any element contains "\n", just add blank lines between them
-- TODO clean this up!
prettyResult :: CutConfig -> CutType -> FilePath -> IO Doc
prettyResult _ EmptySet  _ = return $ text "{}"
prettyResult cfg (SetOf t) f
  | t `elem` [str, num] = do
    -- liftIO $ putStrLn $ "pretty set of " ++ show t
    -- liftIO $ putStrLn $ "from file " ++ f
    lits     <- fmap lines $ readFile $ cfgTmpDir cfg </> f
    -- liftIO $ putStrLn $ "lits are: " ++ show lits
    let lits' = if t == str
                  then map (\s -> text $ "\"" ++ s ++ "\"") lits
                  else map text lits
    return $ text "{" <> sep ((punctuate (text ",") lits')) <> text "}"
  | otherwise = do
    -- liftIO $ putStrLn $ "pretty set of " ++ show t
    paths    <- fmap lines $ readFile $ cfgTmpDir cfg </> f
    pretties <- mapM (prettyResult cfg t) paths
    return $ text "{" <> sep ((punctuate (text ",") pretties)) <> text "}"
prettyResult cfg t f = fmap text $ (tShow t) (cfgTmpDir cfg </> f)
