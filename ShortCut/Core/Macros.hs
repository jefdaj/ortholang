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

module ShortCut.Core.Macros () where

-- TODO sample :: num -> [whatever] -> [[whatever]]
--      (if n > 1 it takes that many items, and if < 1 it takes that fraction.
--       negative numbers remove that many items)

-- TODO actually sample should be a more basic function that does the same thing
--      as its R counterpart. make the one above a different name and it uses this
--      one (always? sometimes?)

-- TODO shared :: num -> [[whatever]] -> [whatever]
--      kind of the opposite of sample; reports the elements shared by at least
--      n permutations or that fraction. what do negatives mean... error?

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

{- What should the final ShortCut code look like? Can it be split up by
 - combination functions and summary functions, or are the two linked too
 - closely? This could be easily solved in Haskell style with higher-order
 - functions, but hopefully I can avoid implementing + explaining that!
 -
 - I guess the simplest way would be do do most things in the "split" part, and
 - then summarizing can be pretty simple. "split" could actually mean more like
 - "repeat for all combinations" and would involve both the independent and
 - dependent variables. Internally it could be broken down further than that,
 - but I don't think there's any reason a user would want to split without
 - repeating.... right?
 -
 - The module could be called "Every" or "Permutations"
 -}

{- The other thing to consider is the user's perspective: what will they expect
 - or want? Would they find it simpler to stick with an "all possibilities at
 - once" approach that doesn't consider state, or would they want to do quick
 - single samples from a list? Maybe that's `samples` vs `sample`?
 -
 - many_lists = sample 3 [1,2,3,4,5]
 -}
