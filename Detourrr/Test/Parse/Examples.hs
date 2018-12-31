module Detourrr.Test.Parse.Examples
  ( exFuns
  , exTerms
  , exExprs
  , exStatements
  )
  where

-- TODO test nested Bops! (A ~ B) | (B ~ A) for example

import Detourrr.Core.Types
import Detourrr.Modules.SeqIO (faa)
-- import Detourrr.Core.Parse

-- TODO example function calls
-- TODO example bop expressions
-- TODO everything with and without parens
-- TODO test operator precedence

---------------------------------
-- ASTs to use in the examples --
---------------------------------

n1, n2 :: CutExpr
n1 = CutLit num 0 "1.0"
n2 = CutLit num 0 "2.0"

lst0, lst1, lst2, lst3 :: CutExpr
lst0  = CutList Empty 0 [] [] -- ah, this is getting empty.list instead of empty
lst1  = CutList num 0 [] [n1]
lst2  = CutList num 0 [] [n1, n2]
lst3  = CutList (ListOf num) 0 [] [lst1]

s4, lst4 :: CutExpr
s4   = CutLit str 0 "four"
lst4 = CutList str 0 [] [s4]

-- TODO why do bops use the list type while lists use the elem type?
bop00, bop10, bop01, bop40, bop04 :: CutExpr
bop00 = CutBop (ListOf Empty) 0 [] "|" lst0 lst0
bop10 = CutBop (ListOf num  ) 0 [] "|" lst1 lst0
bop01 = CutBop (ListOf num  ) 0 [] "|" lst0 lst1
bop40 = CutBop (ListOf str  ) 0 [] "|" lst4 lst0
bop04 = CutBop (ListOf str  ) 0 [] "|" lst0 lst4

len :: [CutExpr] -> CutExpr
len es = CutFun num 0 [] "length" es

addParens :: (String, a) -> (String, a)
addParens (s, a) = ("(" ++ s ++ ")", a)

faa0 :: CutExpr
faa0  = CutFun (ListOf faa) 0 [] "load_faa_each" [lst0]

-- TODO fix pretty-printing to write (ListOf faa) instead of faa.lst here
somefaa, loadsome, loadbop1 :: CutExpr
somefaa  = CutList str 0 [] [CutLit str 0 "some.faa"]
loadsome = (CutFun (ListOf faa) 0 [] "load_faa_each" [somefaa])
loadbop1 = CutBop (ListOf faa) 0 [] "~" loadsome loadsome

--------------
-- examples --
--------------

exFuns :: [(String, CutExpr)]
exFuns =
  [ ("length [   ]", len [lst0])
  , ("length [1  ]", len [lst1])
  , ("length [1,2]", len [lst2])
  , ("length [[1]]", len [lst3])
  ]

-- TODO can this be done with the weird lambda thing? is it worth it?
-- exBops :: [(String, CutExpr)]
-- exBops = undefined

exTerms :: [(String, CutExpr)]
exTerms = exFuns ++ map addParens exFuns ++
  [ ("[1]"       , lst1)
  , ("[\"four\"]", lst4)
  , ("length [ ]", len [lst0])
  , ("length [1]", len [lst1])

  -- empty lsts are cast to whatever lst type is required
  -- TODO check that this holds for all the custom typecheck fns
  , ("load_faa_each []", faa0)
  , ("load_faa_each [\"some.faa\"]", loadsome)
  ]

exExprs :: [(String, CutExpr)]
exExprs = exTerms ++ map addParens exTerms ++
  -- empty lsts have a special type
  [ ("[] | []", bop00)

  -- if only one is empty, the bop should pick up the other's type
  , ("[1] | [ ]", bop10)
  , ("[ ] | [1]", bop01)
  , ("[\"four\"] | []", bop40)
  , ("[] | [\"four\"]", bop04)

  -- bops should be left-associative, and type-picking-up should still work
  , ("[1] | [] | []", CutBop (ListOf num) 0 [] "|" bop10 lst0)
  , ("[\"four\"] | [] | []", CutBop (ListOf str) 0 [] "|" bop40 lst0)

  -- can put fn calls in lsts, with or without separators
  , ("[ length [] ]", CutList num 0 [] [len [lst0]])
  , ("[(length [])]", CutList num 0 [] [len [lst0]])
  , ("[length [],  length []]", CutList num 0 [] [len [lst0], len [lst0]])
  , ("[length [],\nlength []]", CutList num 0 [] [len [lst0], len [lst0]])

  -- should be able to put fn calls in bops, with or without parens
  , ("(load_faa_each [\"some.faa\"]) ~ (load_faa_each [\"some.faa\"])", loadbop1)
  , ( "load_faa_each [\"some.faa\"]  ~  load_faa_each [\"some.faa\"] ", loadbop1)
  ]

-- TODO list of pairs of expressions that should parse the same;
--      then you don't have to actually write out the AST

exStatements :: [(String, CutAssign)]
exStatements = []
