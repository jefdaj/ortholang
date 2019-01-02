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

n1, n2 :: DtrExpr
n1 = DtrLit num 0 "1.0"
n2 = DtrLit num 0 "2.0"

lst0, lst1, lst2, lst3 :: DtrExpr
lst0  = DtrList Empty 0 [] [] -- ah, this is getting empty.list instead of empty
lst1  = DtrList num 0 [] [n1]
lst2  = DtrList num 0 [] [n1, n2]
lst3  = DtrList (ListOf num) 0 [] [lst1]

s4, lst4 :: DtrExpr
s4   = DtrLit str 0 "four"
lst4 = DtrList str 0 [] [s4]

-- TODO why do bops use the list type while lists use the elem type?
bop00, bop10, bop01, bop40, bop04 :: DtrExpr
bop00 = DtrBop (ListOf Empty) 0 [] "|" lst0 lst0
bop10 = DtrBop (ListOf num  ) 0 [] "|" lst1 lst0
bop01 = DtrBop (ListOf num  ) 0 [] "|" lst0 lst1
bop40 = DtrBop (ListOf str  ) 0 [] "|" lst4 lst0
bop04 = DtrBop (ListOf str  ) 0 [] "|" lst0 lst4

len :: [DtrExpr] -> DtrExpr
len es = DtrFun num 0 [] "length" es

addParens :: (String, a) -> (String, a)
addParens (s, a) = ("(" ++ s ++ ")", a)

faa0 :: DtrExpr
faa0  = DtrFun (ListOf faa) 0 [] "load_faa_each" [lst0]

-- TODO fix pretty-printing to write (ListOf faa) instead of faa.lst here
somefaa, loadsome, loadbop1 :: DtrExpr
somefaa  = DtrList str 0 [] [DtrLit str 0 "some.faa"]
loadsome = (DtrFun (ListOf faa) 0 [] "load_faa_each" [somefaa])
loadbop1 = DtrBop (ListOf faa) 0 [] "~" loadsome loadsome

--------------
-- examples --
--------------

exFuns :: [(String, DtrExpr)]
exFuns =
  [ ("length [   ]", len [lst0])
  , ("length [1  ]", len [lst1])
  , ("length [1,2]", len [lst2])
  , ("length [[1]]", len [lst3])
  ]

-- TODO can this be done with the weird lambda thing? is it worth it?
-- exBops :: [(String, DtrExpr)]
-- exBops = undefined

exTerms :: [(String, DtrExpr)]
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

exExprs :: [(String, DtrExpr)]
exExprs = exTerms ++ map addParens exTerms ++
  -- empty lsts have a special type
  [ ("[] | []", bop00)

  -- if only one is empty, the bop should pick up the other's type
  , ("[1] | [ ]", bop10)
  , ("[ ] | [1]", bop01)
  , ("[\"four\"] | []", bop40)
  , ("[] | [\"four\"]", bop04)

  -- bops should be left-associative, and type-picking-up should still work
  , ("[1] | [] | []", DtrBop (ListOf num) 0 [] "|" bop10 lst0)
  , ("[\"four\"] | [] | []", DtrBop (ListOf str) 0 [] "|" bop40 lst0)

  -- can put fn calls in lsts, with or without separators
  , ("[ length [] ]", DtrList num 0 [] [len [lst0]])
  , ("[(length [])]", DtrList num 0 [] [len [lst0]])
  , ("[length [],  length []]", DtrList num 0 [] [len [lst0], len [lst0]])
  , ("[length [],\nlength []]", DtrList num 0 [] [len [lst0], len [lst0]])

  -- should be able to put fn calls in bops, with or without parens
  , ("(load_faa_each [\"some.faa\"]) ~ (load_faa_each [\"some.faa\"])", loadbop1)
  , ( "load_faa_each [\"some.faa\"]  ~  load_faa_each [\"some.faa\"] ", loadbop1)
  ]

-- TODO list of pairs of expressions that should parse the same;
--      then you don't have to actually write out the AST

exStatements :: [(String, DtrAssign)]
exStatements = []
