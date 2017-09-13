module ShortCut.Core.Paths2 where

{- TODO come up with a scheme for map fns to reuse same paths as non-mapped!
 -      (can also have additional ones for the lists)
 -
 - TODO update old overview. It's mostly good, except:
 -      * var dir per salt?
 -      * use just paths, salt, etc explicitly rather than showing
 -      * how are symlinks, rel vs abs paths etc handled?
 -      * include the above scheme
 -
 - TODO test that exprPath* fns are deterministic apart from the rest
 -}

import Path
