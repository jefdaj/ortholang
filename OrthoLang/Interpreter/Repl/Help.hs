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

import Data.List.Split       (splitOn)
import Data.Maybe            (catMaybes)
import OrthoLang.Util        (headOrDie)
import System.FilePath.Posix ((</>))

help :: [Module] -> String -> IO String
help mods line = case words line of
  [w] -> headOrDie "failed to look up cmdHelp content" $ catMaybes
           [ fmap fHelp $ findFunction mods w
           , fmap mHelp $ findModule   mods w
           , fmap (tHelp mods) $ findType mods w
           -- TODO fix this , fmap (tHelp cfg) $ findGroup cfg w
           , Just $ getDoc ["notfound"] -- TODO remove?
           ]
  _ -> getDoc ["repl"]

-- class HelpDoc a where
--   helpDoc :: a -> IO (Maybe String)

-- instance HelpDoc Module where
--   helpDoc = mHelp

mHelp :: Module -> IO String
mHelp m = getDoc ["modules" </> mName m]

-- TODO move somewhere better
fHelp :: Function -> IO String
fHelp f = do
  doc <- getDoc ["functions" </> fName f]
  let msg = "\n" ++ renderTypeSig f ++ "\n\n" ++ doc
  return msg

-- TODO move somewhere better
tHelp :: [Module] -> Type -> IO String
tHelp mods t = do
  doc <- getDoc ["types" </> tExtOf t]
  let msg = "The ." ++ tExtOf t ++ " extension is for " ++ descOf t ++ " files.\n\n"
            ++ doc ++ "\n\n"
            ++ tFnList
  return msg
  where
    outputs = listFunctionTypesWithOutput mods t
    inputs  = listFunctionTypesWithInput  mods t
    tFnList = unlines
                 $ ["You can create them with these functions:"] ++ outputs
                ++ ["", "And use them with these functions:"   ] ++ inputs

-- TODO move somewhere better
listFunctionTypesWithInput :: [Module] -> Type -> [String]
listFunctionTypesWithInput mods cType = filter matches descs
  where
    -- TODO match more carefully because it should have to be an entire word
    matches d = (tExtOf cType) `elem` (words $ headOrDie "listFuncionTypesWithInput failed" $
                                       splitOn ">" $ unwords $ tail $ splitOn ":" d)
    descs = map (\f -> "  " ++ renderTypeSig f) (listFunctions mods)

-- TODO move somewhere better
listFunctionTypesWithOutput :: [Module] -> Type -> [String]
listFunctionTypesWithOutput mods cType = filter matches descs
  where
    matches d = (tExtOf cType) `elem` (words $ unwords $ tail $
                                       splitOn ">" $ unwords $ tail $ splitOn ":" d)
    descs = map (\f -> "  " ++ renderTypeSig f) (listFunctions mods)

-- TODO move somewhere. Pretty?
renderTypeSig :: Function -> String
renderTypeSig f = render (pPrint $ fInputs f) ++ " -> " ++ render (pPrint $ fOutput f)
