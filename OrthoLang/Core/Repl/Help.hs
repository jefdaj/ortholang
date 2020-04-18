module OrthoLang.Core.Repl.Help
  (

  -- * Functions used in Core.Repl
    help -- also used in Test.Repl
  , renderTypeSig

  -- * HelpDoc typeclass (TODO don't export?)
  -- , HelpDoc()

  )
  where

import OrthoLang.Core.Types

import OrthoLang.Core.Config (getDoc)
import OrthoLang.Core.Pretty -- (pPrint, render)

import Data.List.Split       (splitOn)
import Data.Maybe            (fromJust, catMaybes)
import OrthoLang.Util        (headOrDie)
import System.FilePath.Posix ((</>))

help :: Config -> String -> IO String
help cfg line = case words line of
  [w] -> headOrDie "failed to look up cmdHelp content" $ catMaybes
           [ fmap fHelp $ findFunction cfg w
           , fmap mHelp $ findModule   cfg w
           , fmap (tHelp cfg) $ findType cfg w
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
tHelp :: Config -> Type -> IO String
tHelp cfg t = do
  doc <- getDoc ["types" </> tExtOf t]
  let msg = "The ." ++ tExtOf t ++ " extension is for " ++ descOf t ++ " files.\n\n"
            ++ doc ++ "\n\n"
            ++ tFnList
  return msg
  where
    outputs = listFunctionTypesWithOutput cfg t
    inputs  = listFunctionTypesWithInput  cfg t
    tFnList = unlines
                 $ ["You can create them with these functions:"] ++ outputs
                ++ ["", "And use them with these functions:"   ] ++ inputs

-- TODO move somewhere better
listFunctionTypesWithInput :: Config -> Type -> [String]
listFunctionTypesWithInput cfg cType = filter matches descs
  where
    -- TODO match more carefully because it should have to be an entire word
    matches d = (tExtOf cType) `elem` (words $ headOrDie "listFuncionTypesWithInput failed" $
                                       splitOn ">" $ unwords $ tail $ splitOn ":" d)
    descs = map (\f -> "  " ++ renderTypeSig f) (listFunctions cfg)

-- TODO move somewhere better
listFunctionTypesWithOutput :: Config -> Type -> [String]
listFunctionTypesWithOutput cfg cType = filter matches descs
  where
    matches d = (tExtOf cType) `elem` (words $ unwords $ tail $
                                       splitOn ">" $ unwords $ tail $ splitOn ":" d)
    descs = map (\f -> "  " ++ renderTypeSig f) (listFunctions cfg)

-- TODO move somewhere. Pretty?
renderTypeSig :: Function -> String
renderTypeSig f = render (pPrint $ fInputs f) ++ " -> " ++ render (pPrint $ fOutput f)
