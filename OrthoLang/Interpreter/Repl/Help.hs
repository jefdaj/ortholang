{-# LANGUAGE ScopedTypeVariables #-}

module OrthoLang.Interpreter.Repl.Help
  (

  -- * Functions used in Core.Repl
    help -- also used in Test.Repl
  , helpTopics
  , renderTypeSig

  -- * HelpDoc typeclass (TODO don't export?)
  , mHelp
  , fHelp
  -- , eHelp

  )
  where

import OrthoLang.Types

import OrthoLang.Interpreter.Config (getDoc)
import OrthoLang.Interpreter.Pretty -- (pPrint, render)
import OrthoLang.Util (headOrDie)
import OrthoLang.Debug (trace)

import Data.Char             (toLower)
import Data.List             (nub, sort, isInfixOf)
import Data.List.Split       (splitOn)
import Data.Maybe            (catMaybes, fromJust)

-- note: head should be OK here because of the fallback
help :: Config -> [Module] -> String -> IO String
help cfg mods line = case words (map toLower line) of
  [w] -> sequence [m, f, b] >>= \ms -> let ms' = trace "interpreter.repl.help" ("ms: " ++ show ms) ms
                                       in (return . head . catMaybes) ms'
           where
             m = mHelp mods w
             f = fHelp mods w
             b = return $ Just $ fallbackHelp cfg mods w
             -- es = eHelp mods w -- TODO write the rest of this

  _ -> getDoc "repl" >>= return . fromJust

-- TODO make sure lowercase names of everything are unique! got about 10 overlaps here...
-- TODO include something notfound?
-- TODO add bop infix operators (by mapping them to prefix equivalents)
helpTopics :: Config -> [Module] -> [String]
helpTopics cfg mods = sort $ nub $ map (map toLower) $ ts ++ gs ++ es ++ fs ++ ms
  where
    ms = map mName mods
    ts = map ext $ nub $ concatMap mTypes     mods
    gs = map ext $ nub $ concatMap mGroups    mods
    es = map ext $ nub $ concatMap mEncodings mods
    -- TODO no filtering here, right? fs = map fName $ nub $ filter (\f -> showhidden cfg || not (Hidden `elem` fTags f))
    fs = map fName $ nub $ concatMap mFunctions mods

-- TODO also add the suggestions with something like "See also:" when help is found
fallbackHelp :: Config -> [Module] -> String -> String
fallbackHelp cfg mods wrd = init $ unlines $ nohelp : suggestions
  where
    nohelp = "No help topics found for '" ++ wrd ++ "'. Maybe you mean one of these?\n"
    suggestions = sort $ filter (map toLower wrd `isInfixOf`) $ helpTopics cfg mods

-- TODO disambiguate from function names somehow. for example muscle
-- TODO figure out how to `less` the output
-- TODO show function types by default in module help too?
mHelp :: [Module] -> String -> IO (Maybe String)
mHelp mods name = case findModule mods (map toLower name) of 
  Nothing -> return Nothing
  Just m -> do
    let mfnames = map fName $ mFunctions m
        basics = mDesc m ++ "\n\nFunctions:\n\n" ++ unlines mfnames -- TODO strip markdown backticks?
    doc <- getDoc $ map toLower (mName m)
    return $ case doc of
      Nothing -> Just basics
      Just d -> Just $ d ++ "\n\n" ++ basics

-- TODO this is the way to go! just have to rewrite the rest to return stuff even without the doc found
-- TODO is there any fDesc? guess that's the type sig mostly
fHelp :: [Module] -> String -> IO (Maybe String)
fHelp mods name = case findFunction mods name of
  Nothing -> return Nothing
  Just f -> do
    let tsig = renderTypeSig f
    doc <- getDoc $ map toLower (fName f)
    return $ case doc of
      Nothing -> Just tsig
      Just d  -> Just $ tsig ++ "\n\n" ++ d

-- TODO write this one like the ones above that work: find from the list, then describe
-- TODO what should we say if they want help on an encoding by itself, like blastdb, vs encoded type?
--      for one thing, probably ignore the encoded type if it's not in any module lists, since it might be invalid
-- eHelp :: [Module] -> String -> IO (Maybe String)
-- eHelp mods name = do
--   doc <- getDoc $ ext e
--   return $ case doc of
--     Nothing -> Nothing
--     -- TODO you can say this either way, right?
--     Just d -> Just $ "The " ++ ext e ++ " extension is for " ++ desc e ++ " files.\n\n"
--                      ++ d ++ "\n\n" ++ tFnList
--   where
--     -- TODO add this stuff to t,g,or e help
--     outputs = listFunctionTypesWithOutput mods e
--     inputs  = listFunctionTypesWithInput  mods e
--     tFnList = unlines
--                  $ ["You can create them with these functions:"] ++ outputs
--                 ++ ["", "And use them with these functions:"   ] ++ inputs

listFunctionTypesWithInput :: Ext e => [Module] -> e -> [String]
listFunctionTypesWithInput mods thing = filter matches descs
  where
    -- TODO match more carefully because it should have to be an entire word
    matches d = (ext thing) `elem` (words $ headOrDie "listFuncionTypesWithInput failed" $
                                       splitOn ">" $ unwords $ tail $ splitOn ":" d)
    descs = map (\f -> "  " ++ renderTypeSig f) (listFunctions mods)

listFunctionTypesWithOutput :: Ext e => [Module] -> e -> [String]
listFunctionTypesWithOutput mods thing = filter matches descs
  where
    matches d = (ext thing) `elem` (words $ unwords $ tail $
                                       splitOn ">" $ unwords $ tail $ splitOn ":" d)
    descs = map (\f -> "  " ++ renderTypeSig f) (listFunctions mods)

-- TODO move somewhere. Pretty?
renderTypeSig :: Function -> String
renderTypeSig f = render (pPrint $ fInputs f) ++ " -> " ++ render (pPrint $ fOutput f)
