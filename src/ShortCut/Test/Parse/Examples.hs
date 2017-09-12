module ShortCut.Test.Parse.Examples
  ( exFuns
  , exTerms
  , exExprs
  , exStatements
  )
  where

import ShortCut.Core.Types
import ShortCut.Modules.SeqIO (faa)
-- import ShortCut.Core.Parse

-- TODO example function calls
-- TODO example bop expressions
-- TODO everything with and without parens
-- TODO everything with comments and newlines inturrupting
-- TODO test operator precedence

---------------------------------
-- ASTs to use in the examples --
---------------------------------

n1, n2 :: CutExpr
n1 = CutLit num 0 "1.0"
n2 = CutLit num 0 "2.0"

set0, set1, set2, set3 :: CutExpr
set0  = CutSet EmptySet 0 [] []
set1  = CutSet num 0 [] [n1]
set2  = CutSet num 0 [] [n1, n2]
set3  = CutSet (SetOf num) 0 [] [set1]

s4, set4 :: CutExpr
s4   = CutLit str 0 "four"
set4 = CutSet str 0 [] [s4]

bop00, bop10, bop01, bop40, bop04 :: CutExpr
bop00 = CutBop EmptySet 0 [] "|" set0 set0
bop10 = CutBop (SetOf num) 0 [] "|" set1 set0
bop01 = CutBop (SetOf num) 0 [] "|" set0 set1
bop40 = CutBop (SetOf str) 0 [] "|" set4 set0
bop04 = CutBop (SetOf str) 0 [] "|" set0 set4

len :: [CutExpr] -> CutExpr
len es = CutFun num 0 [] "length" es

addParens :: (String, a) -> (String, a)
addParens (s, a) = ("(" ++ s ++ ")", a)

faa0 :: CutExpr
faa0  = CutFun (SetOf faa) 0 [] "load_faa_each" [set0]

-- TODO fix pretty-printing to write (SetOf faa) instead of faa.set here
somefaa, loadsome, loadbop1 :: CutExpr
somefaa  = CutSet str 0 [] [CutLit str 0 "some.faa"]
loadsome = (CutFun (SetOf faa) 0 [] "load_faa_each" [somefaa])
loadbop1 = CutBop (SetOf faa) 0 [] "~" loadsome loadsome

--------------
-- examples --
--------------

exFuns :: [(String, CutExpr)]
exFuns =
  [ ("length {   }", len [set0])
  , ("length {1  }", len [set1])
  , ("length {1,2}", len [set2])
  , ("length {{1}}", len [set3])
  ]

-- TODO can this be done with the weird lambda thing? is it worth it?
-- exBops :: [(String, CutExpr)]
-- exBops = undefined

exTerms :: [(String, CutExpr)]
exTerms = exFuns ++ map addParens exFuns ++
  [ ("{1}"       , set1)
  , ("{\"four\"}", set4)
  , ("length { }", len [set0])
  , ("length {1}", len [set1])

  -- empty sets are cast to whatever set type is required
  -- TODO check that this holds for all the custom typecheck fns
  , ("load_faa_each {}", faa0)
  , ("load_faa_each {\"some.faa\"}", loadsome)
  ]

exExprs :: [(String, CutExpr)]
exExprs = exTerms ++ map addParens exTerms ++
  -- empty sets have a special type
  [ ("{} | {}", bop00)

  -- if only one is empty, the bop should pick up the other's type
  , ("{1} | { }", bop10)
  , ("{ } | {1}", bop01)
  , ("{\"four\"} | {}", bop40)
  , ("{} | {\"four\"}", bop04)

  -- bops should be left-associative, and type-picking-up should still work
  , ("{1} | {} | {}", CutBop (SetOf num) 0 [] "|" bop10 set0)
  , ("{\"four\"} | {} | {}", CutBop (SetOf str) 0 [] "|" bop40 set0)

  -- can put fn calls in sets, with or without separators
  , ("{ length {} }", CutSet num 0 [] [len [set0]])
  , ("{(length {})}", CutSet num 0 [] [len [set0]])
  , ("{length {},  length {}}", CutSet num 0 [] [len [set0], len [set0]])
  , ("{length {},\nlength {}}", CutSet num 0 [] [len [set0], len [set0]])

  -- should be able to put fn calls in bops, with or without parens
  , ("(load_faa_each {\"some.faa\"}) ~ (load_faa_each {\"some.faa\"})", loadbop1)
  , ( "load_faa_each {\"some.faa\"}  ~  load_faa_each {\"some.faa\"} ", loadbop1)

  -- comments are allowed in sets
  , ("{length {}, # comment\nlength {}}" , CutSet num 0 [] [len [set0], len [set0]])
  , ("{length {} # comment\n, length {}}", CutSet num 0 [] [len [set0], len [set0]])
  ]

-- TODO list of pairs of expressions that should parse the same;
--      then you don't have to actually write out the AST

exStatements :: [(String, CutAssign)]
exStatements = []
