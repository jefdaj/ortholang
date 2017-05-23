{- Macros are mainly intended to support my idea for "universal bootstrapping"
 - of ShortCut programs (there's probably a better word for it). I want to be
 - able to write functions to assess the robustness of results using brute
 - force rather than statistics, because I'm lazy! Functions that will answer
 - questions like:
 -
 - * "If I removed half the known PSII assembly factors one at a time, how
 -    many of them would be rediscovered in the cut?"
 -
 - * "Which of my results would change if I started from a different list of
 -    model genomes?"
 -
 - These and other similar questions should be answerable using only a
 - moderately complicated algorithm:
 -
 - <fill in here from old slides>
 -
 - It might get pretty tricky in practice though, so it will be necessary to
 - develop a good set of tests along the way.
 -
 - It also might take a while to compute that many Shake rules? Not sure yet.
 - But Shake is supposedly optimized for large rulesets, and the author seems
 - interested in supporting this kind of bioinformatics experimentation. I
 - can't imagine it being the bottleneck at runtime since every permutation
 - will have to be computed with BLAST searches or something, but it might
 - noticably slow down the REPL.
 -}

module ShortCut.Core.Macros () where
