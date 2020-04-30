module Main where

import OrthoLang.Types
import OrthoLang.Modules (modules)
import Data.List
import Data.List.Utils
import Data.Maybe

-- TODO name type variables, but how? think about replace_each first then generalize
--      fold left -> right with an accumulator of current names
--      when encountering...
--        something with an exact
-- TODO convert the acc to a final string description

---------
-- map --
---------

type VarName = String
type VarDesc = String
type VarIndex = Int
type VarMap = [(VarName, [(VarIndex, VarDesc)])]

typeSigVarNames :: Function -> VarMap
typeSigVarNames f = typeSigVarNames' [] $ fInputs f -- ++ [fOutput f] (output has to be one of the inputs right?)

typeSigVarNames' :: VarMap -> [TypeSig] -> VarMap
typeSigVarNames' acc [] = acc
typeSigVarNames' acc (s:ss) = typeSigVarNames' (addSig acc s) ss

addSig :: VarMap -> TypeSig -> VarMap
addSig vm (ListSigs     t) = addSig vm t
addSig vm (ScoresSigs   t) = addSig vm t
addSig vm (EncodedSig e t) = let vm' = addSig vm t
                                     in        addName vm' (ext e, desc e)
addSig vm (AnyType      s) = addName vm ("any", s) -- TODO need s as a key right?
addSig vm (Some       g s) = addGroup vm g s       -- TODO need s as a key right?

addSig vm (Exactly (ListOf      t)) = addType vm t
addSig vm (Exactly (ScoresOf    t)) = addType vm t
addSig vm (Exactly (EncodedAs e t)) = let vm' = addName vm  (ext e, desc e)
                                              in addType vm' t
addSig vm (Exactly t) = addType vm t

addGroup :: VarMap -> TypeGroup -> String -> VarMap
addGroup vm g s = addName vm (ext g, desc g ++ ": " ++ exts ++ " (" ++ s ++ ")")
  where
    exts = intercalate ", " $ map ext $ tgMembers g

addType :: VarMap -> Type -> VarMap
addType vm (ListOf      t) = addName vm (ext t, desc t)
addType vm (ScoresOf    t) = addName vm (ext t, desc t)
addType vm (EncodedAs e t) = let vm' = addName vm  (ext e, desc e)
                                 in        addName vm' (ext t, desc t)
addType vm t = addName vm (ext t, desc t)

-- TODO are some getting overwritten?
addName :: VarMap -> (VarName, VarDesc) -> VarMap
addName vm (tvn, tvd) = case lookup tvn vm of
  Nothing -> addToAL vm tvn [(1, tvd)]
  Just ds -> let ds' = if tvd `elem` (map snd ds)
                         then ds
                         else ds ++ [(length ds + 1, tvd)]
             in addToAL vm tvn ds'

------------
-- render --
------------

{-|
Var naming rules:

* Regular types are shown in the type signature as their extensions (no index)
* TypeGroups or AnyTypes are shown with an index too (only if more than one?)
* Same variables need to go in the signature and where clause of course

Idea: the things that need an index are the same ones that need a custom description, so combine
-}

-- | Renders the entire type signature help block (not counting custom help file text)
renderHelp :: Function -> String
renderHelp = undefined
-- renderHelp f = fName f ++ " : " ++ ins ++ " -> " ++ out ++ "\n" ++ wClause
--   where
--    ins = unwords $ map (\(i, t) -> renderType vm) (fInputs f)
--    out = renderType vm $ fOutput f
--    vm = typeSigVarNames f
--    wClause = unlines $ "where..." : map (\(n, (i, d)) -> renderDesc n i d) names
--    names = concatMap (\(n, ds) -> zip (repeat n) ds) vm

-- | Renders the type variable name: faa, og, any1, etc.
-- TODO how to fold over this?
renderType :: VarName -> VarIndex -> String
renderType = undefined
-- renderType vm name index = case lookup name vm of
--   Nothing -> error "renderType failed :("
--   Just (name, descs) -> if index < 0 || index >= length descs
--     then error "renderType failed :("
--     else let (_, desc) = descs !! index
--          in name ++ " " ++ desc

-- | Renders the type variable description (everything after "where...")
-- TODO should be doable by zipping the exts onto the indexed descriptions,
--      and dropping some of the indexes at the same time
renderDesc :: VarName -> VarIndex -> VarDesc -> String
renderDesc name index desc = "  " ++ name ++ show index ++ " is " ++ desc

----------
-- main --
----------

fs :: [Function]
fs = filter (\f -> not $ Hidden `elem` fTags f) $ concatMap mFunctions modules

fsByName :: [(String, Function)]
fsByName = map (\f -> (fName f, f)) fs

is :: [[TypeSig]]
is = map fInputs fs

os :: [TypeSig]
os = map fOutput fs

f90 :: Function
f90 = fs !! 90

s10 :: [TypeSig]
s10 = fInputs f90 ++ [fOutput f90]

main :: IO ()
main = do
  let names = map (\f -> (fName f, typeSigVarNames f)) fs

      -- longnames = filter (\(n, ds) -> length ds > 2) names
      -- keys = map (\(n, m) -> (n, map fst m)) names
      -- vals = map (\(n, m) -> (n, map snd m)) names
      -- longvals = filter (\(n, m) -> length m > 1) vals
  -- mapM_ print keys
  -- mapM_ print names

  print $ fromJust $ lookup "replace" names
  -- putStrLn $ renderHelp $ fromJust $ lookup "replace" fsByName

  print $ fromJust $ lookup "replace_each" names
  -- putStrLn $ renderHelp $ fromJust $ lookup "replace_each" fsByName

  print $ fromJust $ lookup "repeat" names
  -- putStrLn $ renderHelp $ fromJust $ lookup "repeat" fsByName

  print $ fromJust $ lookup "score_repeats" names
  -- putStrLn $ renderHelp $ fromJust $ lookup "score_repeats" fsByName

  -- print $ lookup "replace" names
  -- print $ lookup "replace_each" names
  -- print $ lookup "repeat" names
  -- print $ lookup "score_repeats" names
  return ()
