{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import OrthoLang.Types
import OrthoLang.Modules (modules)
import Data.List
import Data.List.Utils
import Data.Maybe
import Data.Either

---------
-- map --
---------

type VarName = String
type VarDesc = String
type VarIndex = Int
type VarMap = [(VarName, [(VarDesc, VarIndex)])]

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

-- | Renders the entire type signature help block (not counting custom help file text)
renderSig :: Function -> String
renderSig f = unwords $ [name, ":"] ++ inSigs ++ ["->", outSig]
  where
    name   = fName f
    names  = inputNames f
    inSigs = map (renderExt names) $ fInputs f
    outSig = renderExt names $ fOutput f

renderHelp :: Function -> String
renderHelp f = renderSig f ++ "\n" ++ renderWhere names (fInputs f)
  where
    name   = fName f -- TODO move to renderWhere?
    names  = inputNames f -- TODO move to renderWhere?
    -- inSigs = map (renderExt names) $ fInputs f
    -- outSig = renderExt names $ fOutput f

renderExt :: VarMap -> TypeSig -> String
renderExt vm (ListSigs     s) = renderExt vm s ++ ".list"
renderExt vm (ScoresSigs   s) = renderExt vm s ++ ".scores"
renderExt vm (EncodedSig e s) = renderExt vm s ++ "." ++ renderName vm (ext e) Nothing
renderExt vm (AnyType      s) = renderName vm "any" (Just s)
renderExt vm (Some       g s) = renderName vm (ext g) (Just s)
renderExt vm (Exactly      t) = renderName vm (ext t) Nothing -- TODO another function here for Types

renderExtOnly :: VarMap -> TypeSig -> String
renderExtOnly vm (ListSigs     s) = renderExtOnly vm s
renderExtOnly vm (ScoresSigs   s) = renderExtOnly vm s
renderExtOnly vm (EncodedSig e s) = renderExtOnly vm s ++ "." ++ renderName vm (ext e) Nothing -- TODO what to do?
renderExtOnly vm (AnyType      s) = renderName vm "any" (Just s)
renderExtOnly vm (Some       g s) = renderName vm (ext g) (Just s)
renderExtOnly vm (Exactly      t) = renderName vm (ext t) Nothing -- TODO another function here for Types

-- TODO should this be shown for all types, or just the ambiguous ones? start with those
renderDesc :: TypeSig -> Maybe String
renderDesc (AnyType s) = Just s
renderDesc (Some  _ s) = Just s
renderDesc (Exactly _) = Nothing
renderDesc (ListSigs     s) = renderDesc s
renderDesc (ScoresSigs   s) = renderDesc s
renderDesc (EncodedSig _ s) = renderDesc s

-- output should never need descibing because it's either an exact type or also one of the inputs
renderWhere :: VarMap -> [TypeSig] -> String
renderWhere names inSigs = if length descs == 0 then "" else unlines $ "where" : descs
  where
    descs = nub $ catMaybes $ map (\i -> fmap (withExt i) $ renderDesc i) inSigs
    withExt i d = "  " ++ unwords [renderExtOnly names i, "=", d]

-- Renders the type variable name: faa, og, any1, etc.
-- TODO remove the raw errors?
-- TODO have this return the where... descriptions too?
renderName :: VarMap -> VarName -> Maybe VarDesc -> String
renderName names name mDsc = case lookup name names of

  -- this can happen when the output type isn't one of the inputs.
  -- that's only a problem if it's ambiguous.
  -- TODO check for that here? or separately maybe
  -- Nothing -> error $ "no such typesig name: '" ++ name ++ "'
  -- options are:\n" ++ show names
  Nothing -> name

  Just indexMap -> case mDsc of
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
-- renderDesc :: VarMap -> VarName -> Maybe VarDesc -> String
-- renderDesc names name mDsc = unwords $ [name']
--   where
--     name' = renderName names name mDsc

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

main :: IO ()
main = do
  -- check that they all finish without errors:
  mapM_ (putStrLn . renderHelp) fs
  putStrLn ""
  putStrLn ""

  -- then print just the ones we want to inspect
  let testfns = ["repeat", "replace", "replace_each", "scatterplot", "venndiagram"]
  mapM_ (putStrLn . renderHelp) $ filter (\f -> fName f `elem` testfns) fs
