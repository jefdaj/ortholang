{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import OrthoLang.Types
import OrthoLang.Modules (modules)
import Data.List
import Data.List.Utils
import Data.Maybe
import Data.Either
-- import Control.Exception

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
type VarMap = [(VarName, [(VarDesc, VarIndex)])]

-- VarMap formatted for use when folding over a type signature
-- Does not include types that don't need an index
-- type VarMap2 = [(VarName, VarIndex), VarDesc]

-- TODO remove?
-- type NiceVarMap = [(VarName,   VarIndex, VarDesc)]

-- TODO remove?
-- flattenMap :: VarMap -> NiceVarMap
-- flattenMap [] = []
-- flattenMap ((n, ds):xs) = map (flattenOne n) ds ++ flattenMap xs

-- TODO remove?
-- flattenOne :: VarName -> (VarIndex, VarDesc) -> (VarName, VarIndex, VarDesc)
-- flattenOne n (i, d) = (n ++ show i, i, d)

-- type VarExt = String
-- type VarKey = (VarExt, VarIndex)
-- type VarInfo = (VarName, VarDesc)
-- type VarMap2 = [(VarKey, VarInfo)]

-- no need to map the output because we require it to match one of the inputs
inputNames :: Function -> VarMap
inputNames f = inputNames' [] $ fInputs f

inputNames' :: VarMap -> [TypeSig] -> VarMap
inputNames' acc [] = acc
inputNames' acc (s:ss) = inputNames' (addSig acc s) ss

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
addGroup vm g s = addName vm (ext g, s) -- TODO fix this? desc g ++ ": " ++ exts ++ " (" ++ s ++ ")")
  where
    exts = intercalate ", " $ map ext $ tgMembers g

addType :: VarMap -> Type -> VarMap
addType vm (ListOf      t) = addName vm (ext t, desc t)
addType vm (ScoresOf    t) = addName vm (ext t, desc t)
addType vm (EncodedAs e t) = let vm' = addName vm  (ext e, desc e)
                                 in        addName vm' (ext t, desc t)
addType vm t = addName vm (ext t, desc t)

addName :: VarMap -> (VarName, VarDesc) -> VarMap
addName vm (tvn, tvd) = case lookup tvn vm of
  Nothing -> addToAL vm tvn [(tvd, 1)]
  Just ds -> case lookup tvd ds of
    Just  _ -> vm -- already recorded it
    Nothing -> addToAL vm tvn $ ds ++ [(tvd, length ds + 1)]

------------
-- render --
------------

{-|
Var naming rules:

* Regular types are shown in the type signature as their extensions (no index)
* TypeGroups or AnyTypes are shown with an index too (only if more than one?)
* Same variables need to go in the signature and where clause of course

Idea: the things that need an index are the same ones that need a custom description, so combine
Idea: only things where the desc list > 1 long need a custom description right? use that!
-}

-- tryRender :: Function -> IO (Either String String)
-- tryRender f = catch (return $ Right $ renderSig f)
--                     (\(e :: SomeException) -> return $ Left $ show e)

-- | Renders the entire type signature help block (not counting custom help file text)
renderSig :: Function -> String
renderSig f = unwords $ [fName f, ":"] ++ inSigs ++ ["->", outSig]
  where
    names  = inputNames f
    inSigs = map (renderExt names) $ fInputs f
    outSig = renderExt names $ fOutput f

-- TODO sig + where clause
renderHelp :: Function -> String
renderHelp f = undefined

-- like ext combined with renderName... how should that work?
-- ext Empty             = "empty" -- special case for empty lists with no element type
-- ext (ListOf        t) = ext t ++ ".list"
-- ext (ScoresOf      t) = ext t ++ ".scores"
-- ext (EncodedAs   e t) = ext t ++ "." ++ ext e
-- ext (Type {tExt = e}) = e

renderExt :: VarMap -> TypeSig -> String
renderExt vm (ListSigs     s) = renderExt vm s ++ ".list"
renderExt vm (ScoresSigs   s) = renderExt vm s ++ ".scores"
renderExt vm (EncodedSig e s) = renderExt vm s ++ "." ++ renderName vm (ext e) Nothing
renderExt vm (AnyType      s) = renderName vm "any" (Just s)
renderExt vm (Some       g s) = renderName vm (ext g) (Just s)
renderExt vm (Exactly      t) = renderName vm (ext t) Nothing -- TODO another function here for Types

-- Renders the type variable name: faa, og, any1, etc.
-- TODO remove the raw errors?
renderName :: VarMap -> VarName -> Maybe VarDesc -> String
renderName names name mDesc = case lookup name names of

  -- this can happen when the output type isn't one of the inputs.
  -- that's only a problem if it's ambiguous.
  -- TODO check for that here? or separately maybe
  -- Nothing -> error $ "no such typesig name: '" ++ name ++ "'
  -- options are:\n" ++ show names
  Nothing -> name

  Just indexMap -> case mDesc of
    Nothing -> name -- not something that needs to be indexed anyway (TODO remove this?)
    Just  d -> case lookup d indexMap of

                 -- this can happen when the output type isn't one of the inputs.
                 -- that's only a problem if it's ambiguous.
                 -- TODO check for that here? or separately maybe
                 -- Nothing -> error $ "no such typesig description: '" ++ d ++ "'.
                 -- options are:\n" ++ show indexMap
                 Nothing -> name
                 Just  i -> if length indexMap < 2
                              then name
                              else name ++ show i

-- | Renders the type variable description (everything after "where...")
-- TODO should be doable by zipping the exts onto the indexed descriptions,
--      and dropping some of the indexes at the same time
renderDesc :: VarName -> VarIndex -> VarDesc -> String
renderDesc name index dsc = "  " ++ name ++ show index ++ " is " ++ dsc

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

n90 :: String
n90 = fName f90

s10 :: [TypeSig]
s10 = fInputs f90 ++ [fOutput f90]

names :: [(String, VarMap)]
names = map (\f -> (fName f, inputNames f)) fs

m90 :: VarMap
m90 = fromJust $ lookup n90 names

v90 = renderName m90 "fna" Nothing

sr :: Function
sr = head $ filter (\f -> fName f == "score_repeats") fs

srNames :: VarMap
srNames = inputNames sr

-- test :: Function -> String
-- test f = undefined
--   where
--     names = inputNames f
--     name  = fName f

main :: IO ()
main = do
  -- check that they all finish without errors:
  mapM_ (putStrLn . renderSig) fs
  putStrLn ""

  -- then print just the ones we want to inspect
  let testfns = ["repeat", "replace", "replace_each", "scatterplot", "venndiagram"]
  mapM_ (putStrLn . renderSig) $ filter (\f -> fName f `elem` testfns) fs
