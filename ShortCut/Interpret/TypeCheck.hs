{-# LANGUAGE GADTs #-}

module ShortCut.Interpret.TypeCheck where

import ShortCut.Types
import ShortCut.Monads

import Control.Monad                  (foldM)
import Data.Scientific                (Scientific)
import Text.PrettyPrint.HughesPJClass (prettyShow)

----------------------------------------------------------
-- work-in-progress to rewrite Typed in terms of records
----------------------------------------------------------

-- TODO maybe I need heterogenous/type-level lists like in Glambda after all?

-- import qualified Data.Map as M
-- 
-- checkFns :: M.Map String ([ParsedExpr] -> CheckM TypedExpr)
-- checkFns = M.fromList
--   [ ("test1", test1)
--   ]
-- 
-- test1 :: [ParsedExpr] -> CheckM TypedExpr
-- test1 = undefined


-- TODO give up for now and work only on the Tn-Seq stuff! Jesus this is addictive...

-- 
-- -- My current strategy is to see if I can get away with just specifying
-- -- *return* types at compile time, then doing runtime checks for argument
-- -- types. I think Haskell could still verify that all my check functions
-- -- are OK. To test, I'll try to make everything take a [ParsedExpr]...
-- 
-- data TypedRecord where
--   TypedRecord :: Returns b -> FnRecord a b -> TypedRecord
-- 
-- -- tExpr2 :: ParsedExpr -> CheckM TypedExpr
-- -- tExpr2 (Fil s)       = tFil s
-- 
-- -- TODO is this is viable way forward, and not (too) ugly?
-- --      have all functions take a [ParsedExpr] by definition
-- --      they all just do runtime checks on the number + type of their args
-- --      haskell ensures that those runtime checks are always valid
-- data FnRecord a b = FnRecord
--   { fnArgs :: a -- should be some combination of Returns types, but not enforced
--   , fnCheck :: [ParsedExpr] -> CheckM b -- (or runtime error if not possible)
--   }
-- 
-- check :: String -> [ParsedExpr] -> CheckM TypedExpr
-- check name es = do
--   exprs <- mapM tExpr es
--   let rec = M.lookup name testRecs
--   case rec of
--     Nothing -> error $ "no such function " ++ name
--     Just r ->
--       let expect = fnArgs r
--           actual = map fn1 exprs
--       in if expected == actual
--           then return $ TypedExpr (fnRetn r) (fnCheck r $ exprs)
--           else error $ "wrong arg types " ++ show actual
-- testCheck1 name _ = error $ "programmer error in " ++ name
-- 
-- fn1 :: TypedExpr -> a
-- fn1 (TypedExpr rtn _) = rtn
-- 
-- testRecs :: M.Map String TypedRecord
-- testRecs = M.fromList
--   [ ("test1", TypedRecord RNumber testRec1)
--   -- , ("test2", TypedRecord RNumber testRec2)
--   ]
-- 
-- --testCheck2 :: [ParsedExpr] -> Scientific
-- --testCheck2 = undefined
-- --
-- testRec1 :: FnRecord (Returns Scientific, Returns Scientific) Scientific
-- testRec1 = FnRecord
--   { fnArgs  = (RNumber, RNumber)
--   , fnCheck = testCheck1
--   }
--
--testRec2 :: FnRecord (Returns String, Returns Scientific) Scientific
--testRec2 = FnRecord
--  { fnArgs  = (RFile, RNumber)
--  , fnCheck = testCheck2
--  }

-- Parse.hs transforms text into abstract syntax trees, rejecting anything that
-- obviously doesn't fit the grammar.
--
-- This module tries to catch all the remaining issues:
-- * does a Var exist matching every Ref?
-- * are the types sane (numbers are multiplied, sets are unioned, etc)?
--
-- This module returns a similar abstract syntax tree with more detailed types.
-- For example Bop '+' becomes Add or Union, depending on if the things being
-- added are numbers or sets. If they're some of both, this throws an error.

-- TODO look through this and see if it can be translated to use types!

--------------------------
-- main check functions --
--------------------------

tExpr :: ParsedExpr -> CheckM TypedExpr
tExpr (Fil s)       = tFil s
tExpr (Num n)       = tNum n
tExpr (Ref v)       = tRef v
tExpr (Cmd s es)    = tCmd s es
tExpr (Bop c e1 e2) = tBop c (e1,e2)

tAssign :: ParsedAssign -> CheckM TypedAssign
tAssign ((VarName var), expr) = do
  cexpr <- tExpr expr
  return (TypedVar var, cexpr) -- TODO is the var always going to be OK?

-- I'm not sure what this is supposed to mean design-wise,
-- but it has the type required by foldM for use in tScript
foldAssign :: TypedScript -> ParsedAssign -> CheckM TypedScript
foldAssign script assign = do
  let (cassign, _, _) = runCheckM (tAssign assign) [] script
  case cassign of
    Left err -> throw err
    Right c  -> return $ script ++ [c]

tScript :: ParsedScript -> CheckM TypedScript
tScript = foldM foldAssign []

-----------------
-- basic types --
-----------------

tFil :: String -> CheckM TypedExpr
tFil s = return $ TypedExpr RFile $ File s

tNum :: Scientific -> CheckM TypedExpr
tNum n = return $ TypedExpr RNumber $ Number n

----------------------
-- binary operators --
----------------------

tBop :: Char -> (ParsedExpr, ParsedExpr) -> CheckM TypedExpr
tBop c (e1,e2) = case c of
  '+' -> tPlus  (e1,e2)
  '-' -> tDash  (e1,e2)
  '*' -> tStar  (e1,e2)
  '/' -> tSlash (e1,e2)
  '&' -> tAmp   (e1,e2)
  _   -> throw $ NoSuchFunction [c]

tPlus :: (ParsedExpr, ParsedExpr) -> CheckM TypedExpr
tPlus (e1, e2) = do
  TypedExpr r1 c1 <- tExpr e1
  TypedExpr r2 c2 <- tExpr e2
  case (r1, r2) of
    (RNumber , RNumber ) -> tAdd      (c1,c2)
    (RGenes  , RGenes  ) -> tUnion r1 (c1,c2)
    (RGenomes, RGenomes) -> tUnion r1 (c1,c2)
    _ -> throw $ WrongArgTypes "+" ["set", "same type of set"]
          [prettyShow r1, prettyShow r2]

tDash :: (ParsedExpr, ParsedExpr) -> CheckM TypedExpr
tDash (e1,e2) = do
  TypedExpr r1 c1 <- tExpr e1
  TypedExpr r2 c2 <- tExpr e2
  case (r1, r2) of
    (RNumber , RNumber ) -> tSubtract      (c1,c2)
    (RGenes  , RGenes  ) -> tDifference r1 (c1,c2)
    (RGenomes, RGenomes) -> tDifference r1 (c1,c2)
    _ -> throw $ WrongArgTypes "-" ["set", "same type of set"]
          [prettyShow r1, prettyShow r2]

tAmp :: (ParsedExpr, ParsedExpr) -> CheckM TypedExpr
tAmp (e1,e2) = do
  TypedExpr r1 c1 <- tExpr e1
  TypedExpr r2 c2 <- tExpr e2
  case (r1, r2) of
    (RGenes  , RGenes  ) -> tIntersect r1 (c1,c2)
    (RGenomes, RGenomes) -> tIntersect r1 (c1,c2)
    _ -> throw $ WrongArgTypes "*" ["set", "same type of set"]
          [prettyShow r1, prettyShow r2]

tStar :: (ParsedExpr, ParsedExpr) -> CheckM TypedExpr
tStar (e1,e2) = do
  TypedExpr r1 c1 <- tExpr e1
  TypedExpr r2 c2 <- tExpr e2
  case (r1, r2) of
    (RNumber, RNumber) -> tMultiply (c1,c2)
    _ -> throw $ WrongArgTypes "*" ["number", "number"]
          [prettyShow r1, prettyShow r2]

tSlash :: (ParsedExpr, ParsedExpr) -> CheckM TypedExpr
tSlash (e1,e2) = do
  TypedExpr r1 c1 <- tExpr e1
  TypedExpr r2 c2 <- tExpr e2
  case (r1, r2) of
    (RNumber, RNumber) -> tDivide (c1,c2)
    _ -> throw $ WrongArgTypes "/" ["number", "number"]
          [prettyShow r1, prettyShow r2]

----------
-- math --
----------

tMathOp :: ((Typed Scientific, Typed Scientific) -> Typed Scientific)
        ->  (Typed Scientific, Typed Scientific) -> CheckM TypedExpr
tMathOp fn (c1,c2) = return $ TypedExpr RNumber $ fn (c1,c2)

tAdd :: (Typed Scientific, Typed Scientific) -> CheckM TypedExpr
tAdd = tMathOp Add

tSubtract :: (Typed Scientific, Typed Scientific) -> CheckM TypedExpr
tSubtract = tMathOp Subtract

tMultiply :: (Typed Scientific, Typed Scientific) -> CheckM TypedExpr
tMultiply = tMathOp Multiply

-- TODO what about division by zero??
tDivide :: (Typed Scientific, Typed Scientific) -> CheckM TypedExpr
tDivide = tMathOp Divide

----------
-- sets --
----------

tSetOp :: ((Typed [a], Typed [a]) -> Typed [a])
       -> Returns [a] -> (Typed [a], Typed [a]) -> CheckM TypedExpr
tSetOp fn r (c1,c2) = return $ TypedExpr r $ fn (c1,c2)

tUnion :: Ord a => Returns [a] -> (Typed [a], Typed [a]) -> CheckM TypedExpr
tUnion = tSetOp Union

tIntersect :: Ord a => Returns [a] -> (Typed [a], Typed [a]) -> CheckM TypedExpr
tIntersect = tSetOp Intersect

tDifference :: Ord a => Returns [a] -> (Typed [a], Typed [a]) -> CheckM TypedExpr
tDifference = tSetOp Difference

--------------
-- commands --
--------------

tCmd :: String -> [ParsedExpr] -> CheckM TypedExpr
tCmd s es = case s of
  "load_aa_seqs"      -> tLoadFAA       es
  "load_na_seqs"      -> tLoadFNA       es
  "load_genes"        -> tLoadGenes     es
  "load_genomes"      -> tLoadGenomes   es
  "filter_genomes"    -> tFilterGenomes es
  "filter_genes"      -> tFilterGenes   es
  "worst_best_evalue" -> tWorstBest     es
  _ -> throw $ NoSuchFunction s

-- tLoadFile :: (FilePath -> Typed a) -> Returns a
          -- -> [ParsedExpr] -> CheckM TypedExpr
-- tLoadFile fn r [e] = do
--   TypedExpr s c <- tExpr e
--   case s of
--     RFile -> return $ TypedExpr r $ fn c
--     _ -> throw "type error!"
-- tLoadFile _ _ _ = throw "type error!"

-- TODO why the weird type error when combining these into tLoadFile?
tLoadFAA :: [ParsedExpr] -> CheckM TypedExpr
tLoadFAA [e] = do
  TypedExpr s c <- tExpr e
  case s of
    RFile -> return $ TypedExpr RFastaAA $ LoadFAA c
    w -> throw $ WrongArgTypes "load_aa_seqs" [prettyShow RFile] [prettyShow w]
tLoadFAA es = throw $ WrongArgNumber "load_aa_seqs" 1 (length es)

-- TODO why the weird type error when combining these into tLoadFile?
tLoadFNA :: [ParsedExpr] -> CheckM TypedExpr
tLoadFNA [e] = do
  TypedExpr s c <- tExpr e
  case s of
    RFile -> return $ TypedExpr RFastaNA $ LoadFNA c
    w -> throw $ WrongArgTypes "load_na_seqs" [prettyShow RFile] [prettyShow w]
tLoadFNA es = throw $ WrongArgNumber "load_na_seqs" 1 (length es)

-- TODO why the weird type error when combining these into tLoadFile?
tLoadGenomes :: [ParsedExpr] -> CheckM TypedExpr
tLoadGenomes [e] = do
  TypedExpr s c <- tExpr e
  case s of
    RFile -> return $ TypedExpr RGenomes $ LoadGenomes c
    w -> throw $ WrongArgTypes "load_genomes" ["string"] [prettyShow w]
tLoadGenomes es = throw $ WrongArgNumber "load_genomes" 1 (length es)

-- TODO why the weird type error when combining these into tLoadFile?
tLoadGenes :: [ParsedExpr] -> CheckM TypedExpr
tLoadGenes [e] = do
  TypedExpr s c <- tExpr e
  case s of
    RFile -> return $ TypedExpr RGenes $ LoadGenes c
    w -> throw $ WrongArgTypes "load_genes" ["string"] [prettyShow w]
tLoadGenes es = throw $ WrongArgNumber "load_genes" 1 (length es)

tFilterGenomes :: [ParsedExpr] -> CheckM TypedExpr
tFilterGenomes [genomes, genes, cutoff] = do
  TypedExpr r1 c1 <- tExpr genomes
  TypedExpr r2 c2 <- tExpr genes
  TypedExpr r3 c3 <- tExpr cutoff
  case (r1, r2, r3) of
    (RGenomes, RGenes, RNumber) ->
      return $ TypedExpr RGenomes $ FilterGenomes (c1,c2,c3)
    _ -> throw $ WrongArgTypes
      "filter_genomes"
      [prettyShow RGenomes, prettyShow RGenes, prettyShow RNumber]
      ["<can't show yet>"]
tFilterGenomes es = throw $ WrongArgNumber "filter_genomes" 1 (length es)

tFilterGenes :: [ParsedExpr] -> CheckM TypedExpr
tFilterGenes [genes, genomes, cutoff] = do
  TypedExpr r1 c1 <- tExpr genes
  TypedExpr r2 c2 <- tExpr genomes
  TypedExpr r3 c3 <- tExpr cutoff
  case (r1, r2, r3) of
    (RGenes, RGenomes, RNumber) ->
      return $ TypedExpr RGenes $ FilterGenes (c1,c2,c3)
    _ -> throw $ WrongArgTypes
      "filter_genes"
      [prettyShow RGenes, prettyShow RGenomes, prettyShow RNumber]
      [prettyShow r1, prettyShow r2, prettyShow r3]
tFilterGenes es = throw $ WrongArgNumber "filter_genes" 1 (length es)

tWorstBest :: [ParsedExpr] -> CheckM TypedExpr
tWorstBest [genes, genomes] = do
  TypedExpr r1 c1 <- tExpr genes
  TypedExpr r2 c2 <- tExpr genomes
  case (r1, r2) of
    (RGenes, RGenomes) -> return $ TypedExpr RNumber $ WorstBest (c1,c2)
    _ -> throw $ WrongArgTypes
      "tWorstBest"
      [prettyShow RGenes, prettyShow RGenomes]
      [prettyShow r1, prettyShow r2]
tWorstBest es = throw $ WrongArgNumber "worst_best_evalue" 1 (length es)

----------
-- refs --
----------

tRef :: VarName -> CheckM TypedExpr
tRef (VarName v) = do
  script <- getScript
  case lookup (TypedVar v) script of
    Just (TypedExpr r _) -> return $ TypedExpr r $ Reference r v
    Nothing -> throw $ NoSuchVariable v
