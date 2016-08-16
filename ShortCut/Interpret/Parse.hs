{-# LANGUAGE GADTs #-}

-- TODO: stop accidentally interpreting args in the wrong order as one big variable
-- TODO: start adding nice error messages to each parser
-- TODO: fix bug where a non-function with args parses to varname with args dropped
--       (example: 'this = load_that cool')

module ShortCut.Interpret.Parse where

import Control.Applicative            ((<|>), many)
import Control.Monad                  (void, foldM)
import Control.Monad.Identity         (Identity)
import Data.Char                      (isPrint)
import Data.Scientific                (Scientific)
import ShortCut.Types
import Text.Parsec                    (parse, try)
import Text.Parsec.Char               (char, digit ,letter, spaces, anyChar
                                      ,newline, string, oneOf)
import Text.Parsec.Combinator         (optional, many1, manyTill, eof
                                      ,lookAhead, between, choice, anyToken)
import Text.Parsec.Expr               (buildExpressionParser, Assoc(..)
                                      ,Operator(..))
import Text.Parsec.Prim               (Parsec)
import Text.PrettyPrint.HughesPJClass (prettyShow)
import Control.Monad.Except           (throwError)
import Control.Monad.RWS.Lazy         (get)

--------------------------------
-- helpers to simplify parsec --
--------------------------------

-- Some are from the Parsec tutorial here:
-- https://jakewheat.github.io/intro_to_parsing/#functions-and-types-for-parsing

type Parser  = Parsec String ()
type OpTable = [[Operator String () Identity ParsedExpr]]

parseWithError :: Parser a -> String -> Either CutError a
parseWithError psr str = case parse psr "" str of
  Left  err -> Left $ InvalidSyntax err
  Right res -> Right res

regularParse :: Parser a -> String -> Either CutError a
regularParse p = parseWithError p

parseWithEof :: Parser a -> String -> Either CutError a
parseWithEof p = parseWithError (p <* eof)

parseAndShow :: (Show a) => Parser a -> String -> String
parseAndShow p s = case regularParse p s of
  Left err -> show err
  Right s2 -> show s2

parseWithLeftOver :: Parser a -> String -> Either CutError (a,String)
parseWithLeftOver p = parseWithError ((,) <$> p <*> leftOver)
  where
    leftOver = manyTill anyToken eof

-- this works fine, but isn't used so far:
-- parseWithWSEof :: Parser a -> String -> Either CutError a
-- parseWithWSEof p = parseWithEof (whiteSpace *> p)
--   where
--     whiteSpace = void $ many $ oneOf " \n\t"

-- There's a convention in parsers that each one should consume whitespace
-- after itself (handled by this function), and you only skip leading
-- whitespace at the top level. That way all the whitespace gets skipped
-- exactly once.
lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

whitespaceChars :: [Char]
whitespaceChars = " \t\n"

-- this is like spaces, except it requires at least one
-- TODO: can this be replaced with something from Text.Parsec.Token?
spaces1 :: Parser ()
spaces1 = void $ many1 $ oneOf whitespaceChars

pSym :: Char -> Parser ()
pSym c = void $ lexeme $ char c

-----------------
-- identifiers --
-----------------

pVarName :: Parser VarName
pVarName = lexeme (iden <$> first <*> many rest)
  where
    iden c cs = VarName (c:cs)
    first = letter <|> char '_'
    rest  = digit  <|> first

-- TODO: should this include the Var itself?
pRef :: Parser ParsedExpr
pRef = Ref <$> pVarName

-------------
-- numbers --
-------------

-- TODO: allow negative numbers? (would currently conflict with minus op)
pNum :: Parser ParsedExpr
pNum = do
  n  <- digit
  ns <- lexeme $ many (digit <|> oneOf ".e-")
  return $ Num $ read (n:ns)

---------------------
-- string literals --
---------------------

-- list of chars which can be escaped in ShortCut
-- (they're also escaped in Haskell, so need extra backslashes here)
-- TODO: any others needed? check what people use in Windows filenames
escapeChars :: [Char]
escapeChars = "\\\""

literalChars :: [Char]
literalChars = filter valid $ map toEnum [0..127]
  where
    valid c = isPrint c && not (elem c escapeChars)

-- Tricky bit: the first quote char needs to NOT be a lexeme,
-- because that would consume spaces inside the string literal.
-- see stackoverflow.com/questions/24106314
-- TODO: can the between part be replaced with something from Text.Parsec.Token?
pQuoted :: Parser String
pQuoted = lexeme $ between (char '"') (char '"') $ many (lit <|> esc)
  where
    lit = oneOf literalChars
    esc = char '\\' *> oneOf escapeChars

pFil :: Parser ParsedExpr
pFil = Fil <$> pQuoted

-----------------------------
-- (assignment) statements --
-----------------------------

pVarEq :: Parser ParsedVar
pVarEq = pVarName <* (pSym '=')

pAssign :: Parser ParsedAssign
pAssign = lexeme ((,) <$> pVarEq <*> pExpr <* optional newline)

--------------
-- commands --
--------------

-- This is a kludge to make my "interesting" preference for spaces as function
-- application work right. It's used to test whether we've reached the end of a
-- list of arguments for the function currently being parsed.
pEnd :: Parser ()
pEnd = lookAhead $ void $ choice
  [ eof, pComment, pSym ')'
  , void $ try $ choice $ map pSym operatorChars
  , void $ try pVarEq
  ]

-- TODO: load names from somewhere else?
fnNames :: [String]
fnNames =
  [ "load_aa_seqs"
  , "load_na_seqs"
  , "load_genes"
  , "load_genomes"
  , "filter_genomes"
  , "filter_genes"
  , "worst_best_evalue"
  ]

-- TODO: put this in terms of "keyword" or something?
pName :: Parser String
pName = choice $ map (try . str) fnNames
  where
    str s = string s <* (void spaces1 <|> eof)

pCmd :: Parser ParsedExpr
pCmd = Cmd <$> pName <*> manyTill pTerm pEnd

-----------------
-- expressions --
-----------------

pParens :: Parser ParsedExpr
pParens = between (pSym '(') (pSym ')') pExpr

pTerm :: Parser ParsedExpr
pTerm = pParens <|> pCmd <|> try pNum <|> pFil <|> pRef

operatorChars :: [Char]
operatorChars = "+-*/&|~"

-- for now, I think all binary operators at the same precedence should work.
-- but it gets more complicated I'll write out an actual table here as
-- expected, with a prefix function too etc. see the jake wheat tutorial
operatorTable :: OpTable
operatorTable = [map binary operatorChars]
  where
    binary c = Infix (Bop c <$ pSym c) AssocLeft

-- This function automates building complicated nested grammars that parse
-- operators correctly. I'm using it in a very basic way though: one short list
-- of three operators with the same properties. I think it's good to have in
-- here though, because theoretically I could add fancier things like boolean
-- expressions by just changing the operatorTable.
-- see:
-- jakewheat.github.io/intro_to_parsing/#_operator_table_and_the_first_value_expression_parser
pExpr :: Parser ParsedExpr
pExpr = buildExpressionParser operatorTable pTerm

--------------
-- comments --
--------------

-- Tricky bit: # should NOT be a lexeme
pComment :: Parser ()
pComment = lexeme $ void $ char '#' >> restOfLine
  where
    restOfLine = manyTill anyChar (void newline <|> eof)

-------------
-- scripts --
-------------

pScript :: Parser ParsedScript
pScript = optional spaces *> many pComment *> many (pAssign <* many pComment)

------------------------------------------------
-- everything below this is from TypeCheck.hs --
------------------------------------------------

--------------------------
-- main check functions --
--------------------------

tExpr :: ParsedExpr -> CutM TypedExpr
tExpr (Fil s)       = tFil s
tExpr (Num n)       = tNum n
tExpr (Ref v)       = tRef v
tExpr (Cmd s es)    = tCmd s es
tExpr (Bop c e1 e2) = tBop c (e1,e2)

tAssign :: ParsedAssign -> CutM TypedAssign
tAssign ((VarName var), expr) = do
  cexpr <- tExpr expr
  return (TypedVar var, cexpr) -- TODO is the var always going to be OK?

-- I'm not sure what this is supposed to mean design-wise,
-- but it has the type required by foldM for use in tScript
foldAssign :: TypedScript -> ParsedAssign -> CutM TypedScript
foldAssign script assign = do
  let (cassign, _, _) = runCutM (tAssign assign) [] script
  case cassign of
    Left err -> throwError err
    Right c  -> return $ script ++ [c]

tScript :: ParsedScript -> CutM TypedScript
tScript = foldM foldAssign []

-----------------
-- basic types --
-----------------

tFil :: String -> CutM TypedExpr
tFil s = return $ TypedExpr RFile $ File s

tNum :: Scientific -> CutM TypedExpr
tNum n = return $ TypedExpr RNumber $ Number n

----------------------
-- binary operators --
----------------------

tBop :: Char -> (ParsedExpr, ParsedExpr) -> CutM TypedExpr
tBop c (e1,e2) = case c of
  '+' -> tPlus  (e1,e2)
  '|' -> tUni   (e1,e2)
  '-' -> tDash  (e1,e2)
  '~' -> tDiff  (e1,e2)
  '*' -> tStar  (e1,e2)
  '/' -> tSlash (e1,e2)
  '&' -> tAmp   (e1,e2)
  _   -> throwError $ NoSuchFunction [c]

tPlus :: (ParsedExpr, ParsedExpr) -> CutM TypedExpr
tPlus (e1, e2) = do
  TypedExpr r1 c1 <- tExpr e1
  TypedExpr r2 c2 <- tExpr e2
  case (r1, r2) of
    (RNumber , RNumber ) -> tAdd      (c1,c2)
    _ -> throwError $ WrongArgTypes "+" ["set", "same type of set"]
          [prettyShow r1, prettyShow r2]

tUni :: (ParsedExpr, ParsedExpr) -> CutM TypedExpr
tUni (e1, e2) = do
  TypedExpr r1 c1 <- tExpr e1
  TypedExpr r2 c2 <- tExpr e2
  case (r1, r2) of
    (RGenes  , RGenes  ) -> tUnion r1 (c1,c2)
    (RGenomes, RGenomes) -> tUnion r1 (c1,c2)
    _ -> throwError $ WrongArgTypes "|" ["set", "same type of set"]
          [prettyShow r1, prettyShow r2]

tDash :: (ParsedExpr, ParsedExpr) -> CutM TypedExpr
tDash (e1,e2) = do
  TypedExpr r1 c1 <- tExpr e1
  TypedExpr r2 c2 <- tExpr e2
  case (r1, r2) of
    (RNumber , RNumber ) -> tSubtract      (c1,c2)
    (RGenes  , RGenes  ) -> tDifference r1 (c1,c2)
    (RGenomes, RGenomes) -> tDifference r1 (c1,c2)
    _ -> throwError $ WrongArgTypes "-" ["set", "same type of set"]
          [prettyShow r1, prettyShow r2]

tDiff :: (ParsedExpr, ParsedExpr) -> CutM TypedExpr
tDiff (e1,e2) = do
  TypedExpr r1 c1 <- tExpr e1
  TypedExpr r2 c2 <- tExpr e2
  case (r1, r2) of
    (RGenes  , RGenes  ) -> tDifference r1 (c1,c2)
    (RGenomes, RGenomes) -> tDifference r1 (c1,c2)
    _ -> throwError $ WrongArgTypes "~" ["set", "same type of set"]
          [prettyShow r1, prettyShow r2]

tAmp :: (ParsedExpr, ParsedExpr) -> CutM TypedExpr
tAmp (e1,e2) = do
  TypedExpr r1 c1 <- tExpr e1
  TypedExpr r2 c2 <- tExpr e2
  case (r1, r2) of
    (RGenes  , RGenes  ) -> tIntersect r1 (c1,c2)
    (RGenomes, RGenomes) -> tIntersect r1 (c1,c2)
    _ -> throwError $ WrongArgTypes "*" ["set", "same type of set"]
          [prettyShow r1, prettyShow r2]

tStar :: (ParsedExpr, ParsedExpr) -> CutM TypedExpr
tStar (e1,e2) = do
  TypedExpr r1 c1 <- tExpr e1
  TypedExpr r2 c2 <- tExpr e2
  case (r1, r2) of
    (RNumber, RNumber) -> tMultiply (c1,c2)
    _ -> throwError $ WrongArgTypes "*" ["number", "number"]
          [prettyShow r1, prettyShow r2]

tSlash :: (ParsedExpr, ParsedExpr) -> CutM TypedExpr
tSlash (e1,e2) = do
  TypedExpr r1 c1 <- tExpr e1
  TypedExpr r2 c2 <- tExpr e2
  case (r1, r2) of
    (RNumber, RNumber) -> tDivide (c1,c2)
    _ -> throwError $ WrongArgTypes "/" ["number", "number"]
          [prettyShow r1, prettyShow r2]

----------
-- math --
----------

tMathOp :: ((Typed Scientific, Typed Scientific) -> Typed Scientific)
        ->  (Typed Scientific, Typed Scientific) -> CutM TypedExpr
tMathOp fn (c1,c2) = return $ TypedExpr RNumber $ fn (c1,c2)

tAdd :: (Typed Scientific, Typed Scientific) -> CutM TypedExpr
tAdd = tMathOp Add

tSubtract :: (Typed Scientific, Typed Scientific) -> CutM TypedExpr
tSubtract = tMathOp Subtract

tMultiply :: (Typed Scientific, Typed Scientific) -> CutM TypedExpr
tMultiply = tMathOp Multiply

-- TODO what about division by zero??
tDivide :: (Typed Scientific, Typed Scientific) -> CutM TypedExpr
tDivide = tMathOp Divide

----------
-- sets --
----------

tSetOp :: ((Typed [a], Typed [a]) -> Typed [a])
       -> Returns [a] -> (Typed [a], Typed [a]) -> CutM TypedExpr
tSetOp fn r (c1,c2) = return $ TypedExpr r $ fn (c1,c2)

tUnion :: Ord a => Returns [a] -> (Typed [a], Typed [a]) -> CutM TypedExpr
tUnion = tSetOp Union

tIntersect :: Ord a => Returns [a] -> (Typed [a], Typed [a]) -> CutM TypedExpr
tIntersect = tSetOp Intersect

tDifference :: Ord a => Returns [a] -> (Typed [a], Typed [a]) -> CutM TypedExpr
tDifference = tSetOp Difference

--------------
-- commands --
--------------

tCmd :: String -> [ParsedExpr] -> CutM TypedExpr
tCmd s es = case s of
  "load_aa_seqs"      -> tLoadFAA       es
  "load_na_seqs"      -> tLoadFNA       es
  "load_genes"        -> tLoadGenes     es
  "load_genomes"      -> tLoadGenomes   es
  "filter_genomes"    -> tFilterGenomes es
  "filter_genes"      -> tFilterGenes   es
  "worst_best_evalue" -> tWorstBest     es
  _ -> throwError $ NoSuchFunction s

-- tLoadFile :: (FilePath -> Typed a) -> Returns a
          -- -> [ParsedExpr] -> CutM TypedExpr
-- tLoadFile fn r [e] = do
--   TypedExpr s c <- tExpr e
--   case s of
--     RFile -> return $ TypedExpr r $ fn c
--     _ -> throwError "type error!"
-- tLoadFile _ _ _ = throwError "type error!"

-- TODO why the weird type error when combining these into tLoadFile?
tLoadFAA :: [ParsedExpr] -> CutM TypedExpr
tLoadFAA [e] = do
  TypedExpr s c <- tExpr e
  case s of
    RFile -> return $ TypedExpr RFastaAA $ LoadFAA c
    w -> throwError $ WrongArgTypes "load_aa_seqs" [prettyShow RFile] [prettyShow w]
tLoadFAA es = throwError $ WrongArgNumber "load_aa_seqs" 1 (length es)

-- TODO why the weird type error when combining these into tLoadFile?
tLoadFNA :: [ParsedExpr] -> CutM TypedExpr
tLoadFNA [e] = do
  TypedExpr s c <- tExpr e
  case s of
    RFile -> return $ TypedExpr RFastaNA $ LoadFNA c
    w -> throwError $ WrongArgTypes "load_na_seqs" [prettyShow RFile] [prettyShow w]
tLoadFNA es = throwError $ WrongArgNumber "load_na_seqs" 1 (length es)

-- TODO why the weird type error when combining these into tLoadFile?
tLoadGenomes :: [ParsedExpr] -> CutM TypedExpr
tLoadGenomes [e] = do
  TypedExpr s c <- tExpr e
  case s of
    RFile -> return $ TypedExpr RGenomes $ LoadGenomes c
    w -> throwError $ WrongArgTypes "load_genomes" ["string"] [prettyShow w]
tLoadGenomes es = throwError $ WrongArgNumber "load_genomes" 1 (length es)

-- TODO why the weird type error when combining these into tLoadFile?
tLoadGenes :: [ParsedExpr] -> CutM TypedExpr
tLoadGenes [e] = do
  TypedExpr s c <- tExpr e
  case s of
    RFile -> return $ TypedExpr RGenes $ LoadGenes c
    w -> throwError $ WrongArgTypes "load_genes" ["string"] [prettyShow w]
tLoadGenes es = throwError $ WrongArgNumber "load_genes" 1 (length es)

tFilterGenomes :: [ParsedExpr] -> CutM TypedExpr
tFilterGenomes [genomes, genes, cutoff] = do
  TypedExpr r1 c1 <- tExpr genomes
  TypedExpr r2 c2 <- tExpr genes
  TypedExpr r3 c3 <- tExpr cutoff
  case (r1, r2, r3) of
    (RGenomes, RGenes, RNumber) ->
      return $ TypedExpr RGenomes $ FilterGenomes (c1,c2,c3)
    _ -> throwError $ WrongArgTypes
      "filter_genomes"
      [prettyShow RGenomes, prettyShow RGenes, prettyShow RNumber]
      ["<can't show yet>"]
tFilterGenomes es = throwError $ WrongArgNumber "filter_genomes" 1 (length es)

tFilterGenes :: [ParsedExpr] -> CutM TypedExpr
tFilterGenes [genes, genomes, cutoff] = do
  TypedExpr r1 c1 <- tExpr genes
  TypedExpr r2 c2 <- tExpr genomes
  TypedExpr r3 c3 <- tExpr cutoff
  case (r1, r2, r3) of
    (RGenes, RGenomes, RNumber) ->
      return $ TypedExpr RGenes $ FilterGenes (c1,c2,c3)
    _ -> throwError $ WrongArgTypes
      "filter_genes"
      [prettyShow RGenes, prettyShow RGenomes, prettyShow RNumber]
      [prettyShow r1, prettyShow r2, prettyShow r3]
tFilterGenes es = throwError $ WrongArgNumber "filter_genes" 1 (length es)

tWorstBest :: [ParsedExpr] -> CutM TypedExpr
tWorstBest [genes, genomes] = do
  TypedExpr r1 c1 <- tExpr genes
  TypedExpr r2 c2 <- tExpr genomes
  case (r1, r2) of
    (RGenes, RGenomes) -> return $ TypedExpr RNumber $ WorstBest (c1,c2)
    _ -> throwError $ WrongArgTypes
      "tWorstBest"
      [prettyShow RGenes, prettyShow RGenomes]
      [prettyShow r1, prettyShow r2]
tWorstBest es = throwError $ WrongArgNumber "worst_best_evalue" 1 (length es)

----------
-- refs --
----------

tRef :: VarName -> CutM TypedExpr
tRef (VarName v) = do
  script <- get
  case lookup (TypedVar v) script of
    Just (TypedExpr r _) -> return $ TypedExpr r $ Reference r v
    Nothing -> throwError $ NoSuchVariable v

---------------------------------------------------
-- "un-checkers" to convert typed back to parsed --
---------------------------------------------------

-- This is a temporary function to "undo" the GADT-based typechecking.
-- Hopefully it'll let me remove GADts without breaking anything in the process.

uExpr :: TypedExpr -> ParsedExpr
uExpr (TypedExpr _ (Reference _ s)) = Ref $ VarName s
uExpr (TypedExpr _ (File        s)) = Fil s
uExpr (TypedExpr _ (Number      n)) = Num n
uExpr (TypedExpr _ (LoadFNA     f)) = Cmd "load_fasta_na" [(uExpr $ TypedExpr RFile f)]
uExpr (TypedExpr _ (LoadFAA     f)) = Cmd "load_fasta_aa" [(uExpr $ TypedExpr RFile f)]
uExpr (TypedExpr _ (LoadGenes   f)) = Cmd "load_genes"    [(uExpr $ TypedExpr RFile f)] -- or is this a string?
uExpr (TypedExpr _ (LoadGenomes f)) = Cmd "load_genomes"  [(uExpr $ TypedExpr RFile f)] -- or is this a string?
uExpr (TypedExpr r (Add      (n1, n2))) = Bop '+' (uExpr $ TypedExpr r n1) (uExpr $ TypedExpr r n2)
uExpr (TypedExpr r (Subtract (n1, n2))) = Bop '-' (uExpr $ TypedExpr r n1) (uExpr $ TypedExpr r n2)
uExpr (TypedExpr r (Multiply (n1, n2))) = Bop '*' (uExpr $ TypedExpr r n1) (uExpr $ TypedExpr r n2)
uExpr (TypedExpr r (Divide   (n1, n2))) = Bop '/' (uExpr $ TypedExpr r n1) (uExpr $ TypedExpr r n2)
uExpr (TypedExpr r (Union      (s1, s2))) = Bop '|' (uExpr $ TypedExpr r s1) (uExpr $ TypedExpr r s2)
uExpr (TypedExpr r (Difference (s1, s2))) = Bop '~' (uExpr $ TypedExpr r s1) (uExpr $ TypedExpr r s2)
uExpr (TypedExpr r (Intersect  (s1, s2))) = Bop '&' (uExpr $ TypedExpr r s1) (uExpr $ TypedExpr r s2)
uExpr (TypedExpr _ (FilterGenes   (a1, a2, a3))) = Cmd "filter_genes"   [(uExpr $ TypedExpr RGenes   a1), (uExpr $ TypedExpr RGenomes a2), (uExpr $ TypedExpr RNumber a3)]
uExpr (TypedExpr _ (FilterGenomes (a1, a2, a3))) = Cmd "filter_genomes" [(uExpr $ TypedExpr RGenomes a1), (uExpr $ TypedExpr RGenes   a2), (uExpr $ TypedExpr RNumber a3)]
uExpr (TypedExpr _ (WorstBest     (a1, a2))) = Cmd "worst_best_evalue" [(uExpr $ TypedExpr RGenes   a1), (uExpr $ TypedExpr RGenomes a2)]

uAssign :: TypedAssign -> ParsedAssign
uAssign (TypedVar v, e) = (VarName v, uExpr e)

uScript :: TypedScript -> ParsedScript
uScript = map uAssign
