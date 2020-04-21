module OrthoLang.Interpreter.Repl.Help
  (

  -- * Functions used in Core.Repl
    help -- also used in Test.Repl
  , helpTopics
  , renderTypeSig

  -- * HelpDoc typeclass (TODO don't export?)
  -- , HelpDoc()

  )
  where

import OrthoLang.Types

import OrthoLang.Interpreter.Config (getDoc)
import OrthoLang.Interpreter.Pretty -- (pPrint, render)
import OrthoLang.Util (headOrDie)

import Data.Char             (toLower)
import Data.List             (nub, sort, isInfixOf)
import Data.List.Split       (splitOn)
import Data.Maybe            (catMaybes)
import System.FilePath.Posix ((</>))

help :: [Module] -> String -> IO String
help mods line = case words (map toLower line) of
  [w] -> head $ catMaybes
           [ fmap fHelp $ findFunction mods w
           , fmap mHelp $ findModule   mods w
           , fmap (extHelp mods) $ findType  mods w
           , Just $ return $ fallbackHelp mods w
           ]
  _ -> getDoc ["repl"]

-- TODO make sure lowercase names of everything are unique! got about 10 overlaps here...
-- TODO include something notfound?
-- TODO add bop infix operators (by mapping them to prefix equivalents)
helpTopics :: [Module] -> [String]
helpTopics mods = sort $ nub $ map (map toLower) $ ts ++ gs ++ es ++ fs ++ ms
  where
    ms = map mName mods
    ts = map ext $ nub $ concatMap mTypes     mods
    gs = map ext $ nub $ concatMap mGroups    mods
    es = map ext $ nub $ concatMap mEncodings mods
    fs = map fName $ nub $ concatMap mFunctions mods

fallbackHelp :: [Module] -> String -> String
fallbackHelp mods wrd = init $ unlines $ nohelp : didyou : suggestions
  where
    nohelp = "No help topics found for '" ++ wrd ++ "'."
    didyou = "Did you mean one of these?"
    suggestions = filter (map toLower wrd `isInfixOf`) $ helpTopics mods

mHelp :: Module -> IO String
mHelp m = getDoc ["modules" </> map toLower (mName m)]

-- TODO move somewhere better
fHelp :: Function -> IO String
fHelp f = do
  doc <- getDoc ["functions" </> map toLower (fName f)]
  let msg = "\n" ++ renderTypeSig f ++ "\n\n" ++ doc
  return msg

-- TODO move somewhere better
-- TODO think of something better than either here!
extHelp :: Ext a => [Module] -> a -> IO String
extHelp mods e = do
  doc <- getDoc ["types" </> ext e]
  let msg = "The " ++ ext e ++ " extension is for " ++ desc e ++ " files.\n\n"
            ++ doc ++ "\n\n"
            ++ tFnList
  return msg
  where
    outputs = listFunctionTypesWithOutput mods e
    inputs  = listFunctionTypesWithInput  mods e
    tFnList = unlines
                 $ ["You can create them with these functions:"] ++ outputs
                ++ ["", "And use them with these functions:"   ] ++ inputs

-- TODO move somewhere better
listFunctionTypesWithInput :: Ext e => [Module] -> e -> [String]
listFunctionTypesWithInput mods thing = filter matches descs
  where
    -- TODO match more carefully because it should have to be an entire word
    matches d = (ext thing) `elem` (words $ headOrDie "listFuncionTypesWithInput failed" $
                                       splitOn ">" $ unwords $ tail $ splitOn ":" d)
    descs = map (\f -> "  " ++ renderTypeSig f) (listFunctions mods)

-- TODO move somewhere better
listFunctionTypesWithOutput :: Ext e => [Module] -> e -> [String]
listFunctionTypesWithOutput mods thing = filter matches descs
  where
    matches d = (ext thing) `elem` (words $ unwords $ tail $
                                       splitOn ">" $ unwords $ tail $ splitOn ":" d)
    descs = map (\f -> "  " ++ renderTypeSig f) (listFunctions mods)

-- TODO move somewhere. Pretty?
renderTypeSig :: Function -> String
renderTypeSig f = render (pPrint $ fInputs f) ++ " -> " ++ render (pPrint $ fOutput f)
