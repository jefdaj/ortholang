{- Macros are mainly intended to support my idea for "universal bootstrapping"
 - of ShortCut programs (there's probably a better word for it). I want to be
 - able to write functions to assess the robustness of results using brute
 - force rather than statistics, because I'm lazy! Functions that will answer
 - questions like:
 -
 - * "Which of my results would change if I started from a different list of
 -    model genomes?"
 -
 - * "If I removed the known PSII assembly factors one at a time, which of them
 -    would be rediscovered in my results?"
 -
 - * "Does adding more genomes increase the number of known assembly factors
 -    I rediscover, or is it just a waste of CPU cycles?"
 -
 - These and other similar questions should be answerable using only a
 - moderately complicated algorithm:
 -
 - 1. Permute the "independent" list into a list of lists (there will be
 -    various functions to do this in different ways)
 -
 - 2. Calculate the "dependent" list for each of the permuted inputs
 -
 - 3. Summarize results back down to a single value (there will be a couple ways
 -    to do this, maybe by keeping or removing the common elements, or just
 -    counting them as a score of robustness
 -
 - Theoretically that can be done using just Haskell list/set functions and
 - Shake's cool ability to decide what to build by reading files. It might get
 - pretty tricky in practice though, so it will also be necessary to develop a
 - good set of tests along the way.
 -
 - It also might take a while to compute that many Shake rules? Not sure yet.
 - But Shake is supposedly optimized for large rulesets, and the author seems
 - interested in supporting this kind of bioinformatics experimentation. I
 - can't imagine it being the bottleneck at runtime since every permutation
 - will have to be computed with BLAST searches or something, but it might
 - noticably slow down the REPL.
 -}

-- TODO most of this goes in a module right? only put macro handling itself here

{- How should macros be processed overall? I guess they could be another type
 - of CutFunction. But when they're being parsed they'll need to be given a
 - chance to transform the whole script up to where they were called. This
 - module needs to provide pMacro, then. But it also requires some new compile
 - functions to split and join lists. Maybe it's not a module then, but just
 - some functions spread around Core? Types, Parse, Compile...
 -}

{- The first step in the process will be to create a bunch of copies of the
 - whole abstract syntax tree. Shake doesn't normally allow overlapping rules
 - though, so I need to distinguish each expression somehow. Is an extra
 - prefix/suffix field in the cut expression type needed? Or can it be done by
 - straight up filtering out duplicate expressions?
 -
 - The hash of each expression should include all its inputs, which means
 - everything actually changed by the macro will be unique. That leaves the
 - stuff "before" and "after" it that gets copied unneccessarily. I can think
 - of 3 ways to deal with that:
 -
 - 1. Maybe Shake is already set to ignore duplicates via `alternatives`?
 - 2. Filter out duplicates before compilation (sort and dedup the AST first)
 - 3. Filter out duplicates during compilation (keep a list of hashes compiled)
 -
 - If I take the third option, maybe checking for duplicates in cAssign and
 - cExpr would be enough? If I try the first and it doesn't work, could hack
 - `priority` to make sure none of the rules overlap. That would be a horrible
 - hack though!
 -}

module ShortCut.Core.Macros where

import ShortCut.Core.Types
-- import Data.List (intersect)
-- import Data.Maybe (fromJust)


--------------------------------------------------------
-- prefix variable names so duplicates don't conflict --
--------------------------------------------------------

-- TODO only mangle the specific vars we want changed!

mangleExpr :: (CutVar -> CutVar) -> CutExpr -> CutExpr
mangleExpr _ e@(CutLit  _ _) = e
mangleExpr fn (CutRef  t vs v      ) = CutRef  t (map fn vs)   (fn v)
mangleExpr fn (CutBop  t vs n e1 e2) = CutBop  t (map fn vs) n (mangleExpr fn e1) (mangleExpr fn e2)
mangleExpr fn (CutFun  t vs n es   ) = CutFun  t (map fn vs) n (map (mangleExpr fn) es)
mangleExpr fn (CutList t vs   es   ) = CutList t (map fn vs)   (map (mangleExpr fn) es)
mangleExpr fn (CutSubs r ss v as) = CutSubs (mangleExpr fn r) (mangleExpr fn ss) (fn v) (mangleScript fn as)
-- CutSubs CutExpr CutExpr CutVar [CutAssign] -- dep, ind, ind', cxt

mangleAssign :: (CutVar -> CutVar) -> CutAssign -> CutAssign
mangleAssign fn (var, expr) = (fn var, mangleExpr fn expr)

mangleScript :: (CutVar -> CutVar) -> CutScript -> CutScript
mangleScript fn = map (mangleAssign fn)

-- TODO pad with zeros?
-- Add a "dupN." prefix to each variable name in the path from independent
-- -> dependent variable, using a list of those varnames
addPrefix :: Int -> (CutVar -> CutVar)
addPrefix n (CutVar s) = CutVar $ s ++ "." ++ show n

-- TODO should be able to just apply this to a duplicate script section right?
addPrefixes :: Int -> CutScript -> CutScript
addPrefixes n = mangleScript (addPrefix n)


-------------------------------------------------------
-- duplicate path from independent -> dependent vars --
-------------------------------------------------------

-- varsToPermute :: CutScript -> CutVar -> CutExpr -> [CutVar]
-- varsToPermute scr ind dep = intersect (rDepsOf scr ind) (depsOf dep)
-- 
-- -- Note that this is NOT a working script on its own,
-- -- just a list of assignment statements to add to a larger script
-- permuteAssigns :: CutScript -> CutVar -> CutExpr -> Int -> [CutAssign]
-- permuteAssigns scr ind dep n = addPrefixes n vars $ filter keep scr
--   where
--     vars = varsToPermute scr ind dep
--     keep asn = elem (fst asn) vars

-- permuteIndList :: [CutVar] -> [CutExpr] -> [CutExpr]
-- permuteIndList vars exprs = map (\(n,e) -> addPrefix n vars e) (zip [1..] exprs)


--------------------------
-- main macro expansion --
--------------------------

-- TODO make sure the variables duplicated include the independent + dependent ones themselves
-- TODO draw this stuff on the windows?
-- TODO rename Macro -> Expand, and put the Summary stuff in a different module
--      then you can also have a Core.Expand module and call it from Modules.Expand
-- TODO = script ++ [splitInd, repeatDep]

-- TODO simpler way of thinking through the algorithm, shake-style:
-- 1. need the final list of result permutations
-- 2. to make a result permutation:
--    a. make a copy of the relevant region of the script
--    b. substitute one of the input permutations for the original input

-- expandMacro
--   :: CutConfig -- context to operate in
--   -> CutScript -- initial script before expansion
--   -> CutVar    -- independent/input list to expand into a list of lists
--                -- (must be a var because the output expression must depend on it)
--                -- (and the var must point to a list)
--   -> CutExpr   -- dependent/output expression to recalculate multiple times
--                -- (doesn't necessarily have to be a list)
--                -- (does need to depend on the input var)
--   -> CutScript -- final script with the macro expanded
-- expandMacro cfg script indVar depExpr = undefined
--   where
--     splitFn    = undefined
--     repeatFn   = undefined
--     joinFn     = undefined
