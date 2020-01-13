module OrthoLang.Test.Parse.Examples
  ( exFuns
  , exTerms
  , exExprs
  , exStatements
  )
  where

-- TODO test nested Bops! (A ~ B) | (B ~ A) for example

import OrthoLang.Core.Types
import OrthoLang.Modules.SeqIO (faa)

-- TODO example function calls
-- TODO example bop expressions
-- TODO everything with and without parens
-- TODO test operator precedence

---------------------------------
-- ASTs to use in the examples --
---------------------------------

n1, n2 :: OrthoLangExpr
n1 = OrthoLangLit num (RepeatSalt 0) "1.0"
n2 = OrthoLangLit num (RepeatSalt 0) "2.0"

lst0, lst1, lst2, lst3 :: OrthoLangExpr
lst0  = OrthoLangList Empty (RepeatSalt 0) [] [] -- ah, this is getting empty.list instead of empty
lst1  = OrthoLangList num (RepeatSalt 0) [] [n1]
lst2  = OrthoLangList num (RepeatSalt 0) [] [n1, n2]
lst3  = OrthoLangList (ListOf num) (RepeatSalt 0) [] [lst1]

s4, lst4 :: OrthoLangExpr
s4   = OrthoLangLit str (RepeatSalt 0) "four"
lst4 = OrthoLangList str (RepeatSalt 0) [] [s4]

-- TODO why do bops use the list type while lists use the elem type?
bop00, bop10, bop01, bop40, bop04 :: OrthoLangExpr
bop00 = OrthoLangBop (ListOf Empty) (RepeatSalt 0) [] "|" lst0 lst0
bop10 = OrthoLangBop (ListOf num  ) (RepeatSalt 0) [] "|" lst1 lst0
bop01 = OrthoLangBop (ListOf num  ) (RepeatSalt 0) [] "|" lst0 lst1
bop40 = OrthoLangBop (ListOf str  ) (RepeatSalt 0) [] "|" lst4 lst0
bop04 = OrthoLangBop (ListOf str  ) (RepeatSalt 0) [] "|" lst0 lst4

len :: [OrthoLangExpr] -> OrthoLangExpr
len es = OrthoLangFun num (RepeatSalt 0) [] "length" es

addParens :: (String, a) -> (String, a)
addParens (s, a) = ("(" ++ s ++ ")", a)

faa0 :: OrthoLangExpr
faa0  = OrthoLangFun (ListOf faa) (RepeatSalt 0) [] "load_faa_each" [lst0]

-- TODO fix pretty-printing to write (ListOf faa) instead of faa.lst here
somefaa, loadsome, loadbop1 :: OrthoLangExpr
somefaa  = OrthoLangList str (RepeatSalt 0) [] [OrthoLangLit str (RepeatSalt 0) "some.faa"]
loadsome = (OrthoLangFun (ListOf faa) (RepeatSalt 0) [] "load_faa_each" [somefaa])
loadbop1 = OrthoLangBop (ListOf faa) (RepeatSalt 0) [] "~" loadsome loadsome

--------------
-- examples --
--------------

exFuns :: [(String, OrthoLangExpr)]
exFuns =
  [ ("length [   ]", len [lst0])
  , ("length [1  ]", len [lst1])
  , ("length [1,2]", len [lst2])
  , ("length [[1]]", len [lst3])
  ]

-- TODO can this be done with the weird lambda thing? is it worth it?
-- exBops :: [(String, OrthoLangExpr)]
-- exBops = undefined

exTerms :: [(String, OrthoLangExpr)]
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

exExprs :: [(String, OrthoLangExpr)]
exExprs = exTerms ++ map addParens exTerms ++
  -- empty lsts have a special type
  [ ("[] | []", bop00)

  -- if only one is empty, the bop should pick up the other's type
  , ("[1] | [ ]", bop10)
  , ("[ ] | [1]", bop01)
  , ("[\"four\"] | []", bop40)
  , ("[] | [\"four\"]", bop04)

  -- bops should be left-associative, and type-picking-up should still work
  , ("[1] | [] | []", OrthoLangBop (ListOf num) (RepeatSalt 0) [] "|" bop10 lst0)
  , ("[\"four\"] | [] | []", OrthoLangBop (ListOf str) (RepeatSalt 0) [] "|" bop40 lst0)

  -- can put fn calls in lsts, with or without separators
  , ("[ length [] ]", OrthoLangList num (RepeatSalt 0) [] [len [lst0]])
  , ("[(length [])]", OrthoLangList num (RepeatSalt 0) [] [len [lst0]])
  , ("[length [],  length []]", OrthoLangList num (RepeatSalt 0) [] [len [lst0], len [lst0]])
  , ("[length [],\nlength []]", OrthoLangList num (RepeatSalt 0) [] [len [lst0], len [lst0]])

  -- should be able to put fn calls in bops, with or without parens
  , ("(load_faa_each [\"some.faa\"]) ~ (load_faa_each [\"some.faa\"])", loadbop1)
  , ( "load_faa_each [\"some.faa\"]  ~  load_faa_each [\"some.faa\"] ", loadbop1)
  ]

-- TODO list of pairs of expressions that should parse the same;
--      then you don't have to actually write out the AST

exStatements :: [(String, OrthoLangAssign)]
exStatements = []
