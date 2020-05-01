module OrthoLang.Interpreter.Repl.Help
  (

  -- * Functions used in Core.Repl
    help -- also used in Test.Repl
  , helpTopics
  , renderSig -- signature only
  , renderSigLong -- signature + where clauses
  , findExtDesc
  -- , descBaseOnly

  -- * HelpDoc typeclass (TODO don't export?)
  , mHelp
  , fHelp
  -- , eHelp

  )
  where

import OrthoLang.Types

import OrthoLang.Interpreter.Config (getDoc)
import OrthoLang.Util (headOrDie, stripWhiteSpace)
import OrthoLang.Debug (trace)

import Data.Char             (toLower)
import Data.List             (nub, sort, isInfixOf, intercalate)
import Data.List.Utils       (addToAL)
import Data.List.Split       (splitOn)
import Data.Maybe            (catMaybes, fromJust, listToMaybe)

-- note: head should be OK here because of the fallback
help :: Config -> [Module] -> String -> IO String
help cfg mods line = case words (map toLower line) of
  [w] -> sequence [e, f, m, b] >>= \ms ->
           let ms' = trace "interpreter.repl.help" ("ms: " ++ show ms) ms
           in (return . stripWhiteSpace . head . catMaybes) ms'
           where
             f = fHelp mods w
             m = mHelp mods w
             e = eHelp mods w
             b = return $ Just $ fallbackHelp cfg mods w
  _ -> getDoc "repl" >>= return . fromJust

-- TODO make sure lowercase names of everything are unique! got about 10 overlaps here...
-- TODO include something notfound?
-- TODO add bop infix operators (by mapping them to prefix equivalents)
helpTopics :: Config -> [Module] -> [String]
helpTopics _ mods = sort $ nub $ map (map toLower) $ ts ++ gs ++ es ++ fs ++ ms
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
    let tsig = renderSigLong f
    doc <- getDoc $ map toLower (fName f)
    return $ case doc of
      Nothing -> Just tsig
      Just d  -> Just $ tsig ++ "\n" ++ d

-- TODO what should we say if they want help on an encoding by itself, like blastdb, vs encoded type?
--      for one thing, probably ignore the encoded type if it's not in any module lists, since it might be invalid
eHelp :: [Module] -> String -> IO (Maybe String)
eHelp mods name = case findExtDesc mods name of
  Nothing -> return Nothing
  Just (te, td) -> do
    let outputs  = listFunctionTypesWithOutput mods te
        inputs   = listFunctionTypesWithInput  mods te
        nShown   = 10
        outputs' = if length outputs > nShown then take nShown outputs ++ ["  ..."] else outputs
        inputs'  = if length inputs  > nShown then take nShown inputs  ++ ["  ..."] else inputs
        tFnList = unlines
                    $ ["You can create them with these " ++ show (length outputs)++ " functions:"] ++ outputs'
                   ++ ["", "And use them with these "    ++ show (length inputs) ++ " functions:"] ++ inputs'
    doc <- getDoc te
    return $ let txt d = "The " ++ te ++ " extension is for " ++ td ++ "." ++ d ++ "\n\n" ++ tFnList
             in case doc of
                  Nothing -> Just $ txt ""
                  Just  d -> Just $ txt ("\n\n" ++ d)

findExtDesc :: [Module] -> String -> Maybe (String, String)
findExtDesc mods name = listToMaybe $ catMaybes [fmap ed t, fmap ed g, fmap ed e]
  where
    ed :: (Ext a, Desc a) => a -> (String, String)
    ed x = (ext x, desc x)
    t = findType     mods name
    g = findGroup    mods name
    e = findEncoding mods name

listFunctionTypesWithInput :: [Module] -> String -> [String]
listFunctionTypesWithInput mods e = filter matches descs
  where
    -- TODO match more carefully because it should have to be an entire word
    matches d = e `elem` (words $ headOrDie "listFunctionTypesWithInput failed" $
                                       splitOn ">" $ unwords $ tail $ splitOn ":" d)
    descs = map (\f -> "  " ++ renderSig f) (listFunctions mods)

listFunctionTypesWithOutput :: [Module] -> String -> [String]
listFunctionTypesWithOutput mods e = filter matches descs
  where
    matches d = e `elem` (words $ unwords $ tail $ splitOn ">" $ unwords $ tail $ splitOn ":" d)
    descs = map (\f -> "  " ++ renderSig f) (listFunctions mods)


-------------------
-- map type sigs --
-------------------

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
addSig vm (AnyType      s) = addName vm ("any", s)
addSig vm (Some       g s) = addGroup vm g s

addSig vm (Exactly (ListOf      t)) = addType vm t
addSig vm (Exactly (ScoresOf    t)) = addType vm t
addSig vm (Exactly (EncodedAs e t)) = let vm' = addName vm  (ext e, desc e)
                                              in addType vm' t
addSig vm (Exactly t) = addType vm t

addGroup :: VarMap -> TypeGroup -> String -> VarMap
addGroup vm g s = addName vm (ext g, s)
  -- where
    -- exts = intercalate ", " $ map ext $ tgMembers g

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


----------------------
-- render type sigs --
----------------------

-- Renders the entire type signature help block (not counting custom help file text)
renderSig :: Function -> String
renderSig f = unwords $ [name, ":"] ++ inSigs ++ ["->", outSig]
  where
    name   = fName f
    names  = inputNames f
    inSigs = map (renderExt names) $ fInputs f
    outSig = renderExt names $ fOutput f

renderSigLong :: Function -> String
renderSigLong f = renderSig f ++ "\n" ++ renderWhere names (fInputs f ++ [fOutput f])
  where
    names  = inputNames f

renderExt :: VarMap -> TypeSig -> String
renderExt vm (ListSigs     s) = renderExt vm s ++ ".list"
renderExt vm (ScoresSigs   s) = renderExt vm s ++ ".scores"
renderExt vm (EncodedSig e s) = renderExt vm s ++ "." ++ renderName vm (ext e) Nothing
renderExt vm (AnyType      s) = renderName vm "any" (Just s)
renderExt vm (Some       g s) = renderName vm (ext g) (Just s)
renderExt vm (Exactly      t) = renderName vm (ext t) Nothing

renderWhereExt :: VarMap -> TypeSig -> String
renderWhereExt vm (ListSigs     s) = renderWhereExt vm s
renderWhereExt vm (ScoresSigs   s) = renderWhereExt vm s
renderWhereExt vm (EncodedSig e s) = renderWhereExt vm s ++ "." ++ renderName vm (ext e) Nothing -- TODO what to do?
renderWhereExt vm (AnyType      s) = renderName vm "any" (Just s)
renderWhereExt vm (Some       g s) = renderName vm (ext g) (Just s)
renderWhereExt vm (Exactly      t) = renderName vm (extBaseOnly t) Nothing

-- TODO convert to [String] for encodings
renderWhereDesc :: TypeSig -> Maybe String
renderWhereDesc (AnyType s) = Just s
renderWhereDesc (Some  g s) = Just $ s ++ " (" ++ renderGroupMembers g ++ ")"
renderWhereDesc (Exactly t) = Just $ descBaseOnly t
renderWhereDesc (ListSigs     s) = renderWhereDesc s
renderWhereDesc (ScoresSigs   s) = renderWhereDesc s
renderWhereDesc (EncodedSig _ s) = renderWhereDesc s -- TODO return a list of both? only if doing non-ambig ones

-- TODO convert to [String] for encodings
extBaseOnly :: Type -> String
extBaseOnly (ListOf      t) = extBaseOnly t
extBaseOnly (ScoresOf    t) = extBaseOnly t
extBaseOnly (EncodedAs _ t) = extBaseOnly t
extBaseOnly t = ext t

-- TODO convert to [String] for encodings
descBaseOnly :: Type -> String
descBaseOnly (ListOf      t) = descBaseOnly t
descBaseOnly (ScoresOf    t) = descBaseOnly t
descBaseOnly (EncodedAs _ t) = descBaseOnly t
descBaseOnly t = desc t

renderGroupMembers :: TypeGroup -> String
renderGroupMembers g = withCommas (init $ tgMembers g) ++ " or " ++ ext (last $ tgMembers g)
  where
    withCommas [ ] = ""
    withCommas [m] = ext m
    withCommas  ms = intercalate ", " (map ext ms) ++ ","

renderWhere :: VarMap -> [TypeSig] -> String
renderWhere names inSigs = if length descs == 0 then "" else unlines $ "where" : descs
  where
    descs = nub $ catMaybes $ map (\i -> fmap (withExt i) $ renderWhereDesc i) inSigs
    withExt i d = "  " ++ unwords [renderWhereExt names i, "=", d]

-- Renders the type variable name: faa, og, any1, etc.
-- TODO remove the raw errors?
-- TODO have this return the where... descriptions too?
renderName :: VarMap -> VarName -> Maybe VarDesc -> String
renderName names name mDsc = case lookup name names of

  -- this can happen when the output type isnt one of the inputs.
  -- that's only a problem if its ambiguous.
  -- TODO check for that here? or separately maybe
  Nothing -> name

  Just indexMap -> case mDsc of
    Nothing -> name -- not something that needs to be indexed anyway (TODO remove this?)
    Just  d -> case lookup d indexMap of

                 -- this can happen when the output type isnt one of the inputs.
                 -- thats only a problem if it's ambiguous.
                 -- TODO check for that here? or separately maybe
                 Nothing -> name
                 Just  i -> if length indexMap < 2
                              then name
                              else name ++ show i
