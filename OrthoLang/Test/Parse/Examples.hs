module OrthoLang.Test.Parse.Examples
  -- ( exFuns
  -- , exFuns
  -- , exTerms
  -- , exExprs
  -- , exStatements
  -- )
  where

-- TODO test nested Bops! (A ~ B) | (B ~ A) for example

import OrthoLang.Core
import OrthoLang.Modules.SeqIO (faa)

---------------------------------
-- ASTs to use in the examples --
---------------------------------

n1, n2 :: Expr
n1 = Lit num "1.0"
n2 = Lit num "2.0"

lst0, lst1, lst2, lst3 :: Expr
lst0  = Lst Empty [] [] -- ah, this is getting empty.list instead of empty
lst1  = Lst num [] [n1]
lst2  = Lst num [] [n1, n2]
lst3  = Lst (ListOf num) [] [lst1]

s4, lst4 :: Expr
s4   = Lit str "four"
lst4 = Lst str [] [s4]

-- TODO why do bops use the list type while lists use the elem type?
bop00, bop10, bop01, bop40, bop04 :: Expr
bop00 = Bop (ListOf Empty) Nothing [] "|" lst0 lst0
bop10 = Bop (ListOf num  ) Nothing [] "|" lst1 lst0
bop01 = Bop (ListOf num  ) Nothing [] "|" lst0 lst1
bop40 = Bop (ListOf str  ) Nothing [] "|" lst4 lst0
bop04 = Bop (ListOf str  ) Nothing [] "|" lst0 lst4

len :: [Expr] -> Expr
len es = Fun num Nothing [] "length" es

addParens :: (String, a) -> (String, a)
addParens (s, a) = ("(" ++ s ++ ")", a)

faa0 :: Expr
faa0  = Fun (ListOf faa) Nothing [] "load_faa_each" [lst0]

-- TODO fix pretty-printing to write (ListOf faa) instead of faa.lst here
somefaa, loadsome, loadbop1 :: Expr
somefaa  = Lst str [] [Lit str "some.faa"]
loadsome = (Fun (ListOf faa) Nothing [] "load_faa_each" [somefaa])
loadbop1 = Bop (ListOf faa) Nothing [] "~" loadsome loadsome

--------------
-- examples --
--------------

exFuns :: [(String, Expr)]
exFuns =
  [ ("length [   ]", len [lst0])
  , ("length [1  ]", len [lst1])
  , ("length [1,2]", len [lst2])
  , ("length [[1]]", len [lst3])
  ]

-- TODO can this be done with the weird lambda thing? is it worth it?
-- exBops :: [(String, Expr)]
-- exBops = undefined

exTerms :: [(String, Expr)]
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

exExprs :: [(String, Expr)]
exExprs = exTerms ++ map addParens exTerms ++
  -- empty lsts have a special type
  [ ("[] | []", bop00)

  -- if only one is empty, the bop should pick up the other's type
  , ("[1] | [ ]", bop10)
  , ("[ ] | [1]", bop01)
  , ("[\"four\"] | []", bop40)
  , ("[] | [\"four\"]", bop04)

  -- bops should be left-associative, and type-picking-up should still work
  , ("[1] | [] | []", Bop (ListOf num) Nothing [] "|" bop10 lst0)
  , ("[\"four\"] | [] | []", Bop (ListOf str) Nothing [] "|" bop40 lst0)

  -- can put fn calls in lsts, with or without separators
  , ("[ length [] ]", Lst num [] [len [lst0]])
  , ("[(length [])]", Lst num [] [len [lst0]])
  , ("[length [],  length []]", Lst num [] [len [lst0], len [lst0]])
  , ("[length [],\nlength []]", Lst num [] [len [lst0], len [lst0]])

  -- should be able to put fn calls in bops, with or without parens
  , ("(load_faa_each [\"some.faa\"]) ~ (load_faa_each [\"some.faa\"])", loadbop1)
  , ( "load_faa_each [\"some.faa\"]  ~  load_faa_each [\"some.faa\"] ", loadbop1)
  ]

-- TODO list of pairs of expressions that should parse the same;
--      then you don't have to actually write out the AST

exStatements :: [(String, Assign)]
exStatements = []
