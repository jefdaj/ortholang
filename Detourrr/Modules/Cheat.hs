{- This module lets you "cheat" the type system by calling your own script and
 - specifying the return type as a string. Why would you want that, when the
 - whole idea of Detourrr is that it makes sure scripts fit together smoothly?
 - I can think of two situations in which it might be helpful:
 -
 - 1. You need to extend Detourrr, but can't figure out Haskell or don't want
 -    to bother writing a module.
 -
 - 2. You are developing a Detourrr module, but want to make sure the "useful"
 -    parts work before fiddling with the wrapper code. (This is me sometimes)
 -
 - More generally, I think it's important never to lock people into using a
 - specific set of abstractions. Especially in science! Odds are your research
 - includes complicated issues I can't forsee, and you might need an "escape
 - hatch" to get around my clever rules.
 -
 - Anyway, this provides one function called "cheat". It takes two strings: the
 - script itself (relative path, absolute path, or basename of something on
 - your PATH) and the name of the Detourrr type to assign to the result.
 -
 - You can use a nonexistent Detourrr type here if you want. The only problem
 - is it won't be usable by any other (non-cheat) functions afterward.
 -
 - After those two strings, any number of other arguments are allowed. Their
 - filenames will be passed to the script along with the standard return path
 - and temporary directory.
 -}

-- TODO write examples
-- TODO write cheatTypeCheck
-- TODO write rCheat
-- TODO come up with more neutral word than cheat?
-- TODO have a help menu that lists current types? not until after meeting!

module Detourrr.Modules.Cheat where

import Development.Shake
-- import Detourrr.Core.Parse (typeError)
import Detourrr.Core.Types

dtrModule :: DtrModule
dtrModule = DtrModule
  { mName = "Cheat"
  , mDesc = "Run your own script and Detourrr will assume the types are valid"
  , mTypes = []
  , mFunctions = [cheat]
  }

cheat :: DtrFunction
cheat = DtrFunction
  { fName      = "cheat"
  , fTypeCheck = cheatTypeCheck
  , fDesc      = Nothing
  , fTypeDesc  = "cheat : ??? (implement this)"
  , fFixity    = Prefix
  , fRules  = rCheat
  }

-- TODO detect return type based on string contents,
--      and make a new temporary type if the given one doesn't exist
-- TODO guess that requires either compile-time or runtime list of types?
--      do you keep a global runtime list of them, or have separate "cheat" types?
cheatTypeCheck :: [DtrType] -> Either String DtrType
cheatTypeCheck (script : rtype : _)
  | script == str && rtype == str = findOrMake $ tExt rtype
  where
    findOrMake _ = undefined
cheatTypeCheck _ = Left $ "error! the first two arguments to cheat should \
                          \be strings specifying the script path and return type"

rCheat :: DtrState -> DtrExpr -> Rules ExprPath
rCheat = undefined
