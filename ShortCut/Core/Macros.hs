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

module ShortCut.Core.Macros where

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

import ShortCut.Core.Types


--------------------------------------------------------
-- prefix variable names so duplicates don't conflict --
--------------------------------------------------------

mangleVar :: (String -> String) -> CutVar -> CutVar
mangleVar fn (CutVar v) = CutVar (fn v)

mangleExpr :: (String -> String) -> CutExpr -> CutExpr
mangleExpr fn e@(CutLit  _ _) = e
mangleExpr fn (CutRef  t vs v      ) = CutRef  t (map (mangleVar fn) vs)   (mangleVar fn v)
mangleExpr fn (CutBop  t vs n e1 e2) = CutBop  t (map (mangleVar fn) vs) n (mangleExpr fn e1) (mangleExpr fn e2)
mangleExpr fn (CutFun  t vs n es   ) = CutFun  t (map (mangleVar fn) vs) n (map (mangleExpr fn) es)
mangleExpr fn (CutList t vs   es   ) = CutList t (map (mangleVar fn) vs)   (map (mangleExpr fn) es)

mangleAssign :: (String -> String) -> CutAssign -> CutAssign
mangleAssign fn (CutVar var, expr) = (CutVar $ fn var, mangleExpr fn expr)

mangleScript :: (String -> String) -> CutScript -> CutScript
mangleScript fn = map (mangleAssign fn)

-- TODO pad with zeros?
addDupPrefix :: Int -> CutScript -> CutScript
addDupPrefix n = mangleScript $ \s -> "dup" ++ show n ++ "_" ++ s


------------------------------------------------------------
-- duplicate steps between independent and dependent vars --
------------------------------------------------------------

splitIndVar :: CutVar -> CutScript -> CutScript
splitIndVar var script = undefined

-- TODO will this need to run inside the Action monad?
addDups = undefined


--------------------------
-- main macro expansion --
--------------------------

expandMacro
  :: CutConfig -- context to operate in
  -> CutScript -- initial script before expansion
  -> CutExpr   -- independent/input list to expand into a list of lists
  -> CutExpr   -- dependent/output variable to recalculate multiple times
               -- (doesn't necessarily have to be a list?)
  -> CutScript -- final script with the macro expanded
expandMacro config script inlist outlist = undefined
-- expandMacro _ _ _ _ = error "bad arguments to expandMacro"
