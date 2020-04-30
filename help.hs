module Main where

import OrthoLang.Types
import OrthoLang.Modules (modules)
import OrthoLang.Interpreter.Repl.Help
import Data.List
import Data.List.Utils

fs :: [Function]
fs = filter (\f -> not $ Hidden `elem` fTags f) $ concatMap mFunctions modules

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
  mapM_ print $ map (\f -> (fName f, typeSigVarNames f)) fs

-- TODO name type variables, but how? think about replace_each first then generalize
--      fold left -> right with an accumulator of current names
--      when encountering...
--        something with an exact
-- TODO convert the acc to a final string description

type VarName = String
type VarDesc = String
type VarIndex = Int
type VarMap = [(VarName, [(VarIndex, VarDesc)])]

typeSigVarNames :: Function -> VarMap
typeSigVarNames f = typeSigVarNames' [] $ fInputs f -- ++ [fOutput f] (output has to be one of the inputs right?)

typeSigVarNames' :: VarMap -> [TypeSig] -> VarMap
typeSigVarNames' acc [] = acc
typeSigVarNames' acc (s:ss) = typeSigVarNames' (addSigName acc s) ss

addSigName :: VarMap -> TypeSig -> VarMap
addSigName vm (ListSigs     t) = addSigName vm t
addSigName vm (ScoresSigs   t) = addSigName vm t
addSigName vm (EncodedSig e t) = let vm' = addSigName vm t
                                     in        addName' vm' (ext e, desc e)
addSigName vm (AnyType      s) = addName' vm ("any", s)
addSigName vm (Some       g s) = addGroupName vm g s

addSigName vm (Exactly (ListOf      t)) = addTypeName vm t
addSigName vm (Exactly (ScoresOf    t)) = addTypeName vm t
addSigName vm (Exactly (EncodedAs e t)) = let vm' = addName' vm  (ext e, desc e)
                                              in addTypeName vm' t
addSigName vm (Exactly t) = addTypeName vm t

addGroupName :: VarMap -> TypeGroup -> String -> VarMap
addGroupName vm g s = addName' vm (ext g, desc g ++ ": " ++ exts ++ " (" ++ s ++ ")")
  where
    exts = intercalate ", " $ map ext $ tgMembers g

addTypeName :: VarMap -> Type -> VarMap
addTypeName vm (ListOf      t) = addName' vm (ext t, desc t)
addTypeName vm (ScoresOf    t) = addName' vm (ext t, desc t)
addTypeName vm (EncodedAs e t) = let vm' = addName' vm  (ext e, desc e)
                                 in        addName' vm' (ext t, desc t)
addTypeName vm              t  = addName' vm (ext t, desc t)

-- addEncName :: VarMap -> Encoding -> VarMap
-- addEncName = undefined

addName' :: VarMap -> (VarName, VarDesc) -> VarMap
addName' vm (tvn, tvd) = case lookup tvn vm of
  Nothing -> addToAL vm tvn [(1, tvd)]
  Just ds -> let ds' = if tvd `elem` (map snd ds) then ds else ds ++ [(length ds + 1, tvd)]
             in addToAL vm tvn ds'

-- data TypeSig
--   -- these are analagous to their Type equivalents above:
--   = ListSigs   TypeSig        -- ^ like ListOf with possibly ambiguous sigs inside
--   | ScoresSigs TypeSig        -- ^ like ScoresOf with possibly ambiguous sigs inside
--   | EncodedSig Encoding TypeSig -- ^ like EncodedAs with possibly ambiguous sigs inside
--   -- these are new:
--   | AnyType String        -- ^ generic placeholder. string used like in Some
--   | Some TypeGroup String -- ^ the string is used for equality and in the help text
--   | Exactly Type          -- ^ one regular Type wrapped for use in type signatures
--   deriving (Eq, Show)
-- 
-- instance Pretty TypeSig where
--   pPrint (ListSigs s)     = pPrint s <> PP.text ".list"
--   pPrint (ScoresSigs s)   = pPrint s <> PP.text ".scores"
--   pPrint (EncodedSig e s) = pPrint s <> PP.text ("." ++ enExt e)
--   pPrint (AnyType _)      = PP.text "any" -- TODO does this make sense? might have to do variables
--   pPrint (Some g _)       = pPrint g
--   pPrint (Exactly t)      = pPrint t
--
--data Type
--   = Empty -- ^ used in (ListOf Empty) to denote empty lists
--   | ListOf    Type
--   | ScoresOf  Type
--   | EncodedAs Encoding Type
--   | Type
--     { tExt  :: String
--     , tDesc :: String -- TODO include a longer help text too
--     , tShow :: Config -> LocksRef -> FilePath -> IO String
--     }
--   -- deriving (Eq, Show, Read)
