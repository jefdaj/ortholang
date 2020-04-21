module OrthoLang.Interpreter.Repl.Help
  (

  -- * Functions used in Core.Repl
    help -- also used in Test.Repl
  , renderTypeSig

  -- * HelpDoc typeclass (TODO don't export?)
  -- , HelpDoc()

  )
  where

import OrthoLang.Types

import OrthoLang.Interpreter.Config (getDoc)
import OrthoLang.Interpreter.Pretty -- (pPrint, render)
import OrthoLang.Util (headOrDie)

import Data.List.Split       (splitOn)
import Data.Maybe            (catMaybes)
import System.FilePath.Posix ((</>))

help :: [Module] -> String -> IO String
help mods line = case words line of
  [w] -> head $ catMaybes
           [ fmap fHelp $ findFunction mods w
           , fmap mHelp $ findModule   mods w
           , fmap (extHelp mods) $ findType  mods w
           , Just $ fallbackHelp w
           ]
  _ -> getDoc ["repl"]

-- TODO suggest anything that contains wrd as an infix
fallbackHelp :: String -> IO String
fallbackHelp wrd = return $ "No help topics found for '" ++ wrd ++ "'."

mHelp :: Module -> IO String
mHelp m = getDoc ["modules" </> mName m]

-- TODO move somewhere better
fHelp :: Function -> IO String
fHelp f = do
  doc <- getDoc ["functions" </> fName f]
  let msg = "\n" ++ renderTypeSig f ++ "\n\n" ++ doc
  return msg

-- TODO move somewhere better
-- TODO think of something better than either here!
extHelp :: Ext a => [Module] -> a -> IO String
extHelp mods e = do
  doc <- getDoc ["types" </> ext e]
  let msg = "The ." ++ ext e ++ " extension is for " ++ desc e ++ " files.\n\n"
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
