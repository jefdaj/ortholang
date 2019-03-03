module ShortCut.Core.Compile.Replace where

-- TODO is an initial var to start from needed when doing replace, or can that be the whole script?

{- Why can't we use an obvious var mangling scheme again?
 -   vars/<varname>.<salt>.<ext>
 -   vars/<salt>/<varname>.<ext>
 -   vars/<varname>/<varname>.<salt>.<ext>
 - Maybe just have to delete all var links when starting?
 - Start by moving to prefix dirs, then rename to ints if possible
 -}

import Development.Shake
import ShortCut.Core.Types

import ShortCut.Core.Actions (writeLits, writePaths, readLit, debugA)
import ShortCut.Core.Paths  (exprPath, fromCutPath,
                             CutPath, toCutPath, fromCutPath)
import ShortCut.Core.Compile.Basic (rExpr, compileScript, debugRules)
-- import ShortCut.Core.Debug   (debugRules, debugA)
import ShortCut.Core.Util    (digest, stripWhiteSpace)

--------------------------------------------------------
-- prefix variable names so duplicates don't conflict --
--------------------------------------------------------

-- TODO only mangle the specific vars we want changed!
-- TODO can all this silliness be avoided using random salts?

mangleExpr :: (CutVar -> CutVar) -> CutExpr -> CutExpr
mangleExpr _ e@(CutLit  _ _ _) = e
mangleExpr fn (CutRef  t n vs v      ) = CutRef  t n (map fn vs)   (fn v)
mangleExpr fn (CutBop  t n vs s e1 e2) = CutBop  t n (map fn vs) s (mangleExpr fn e1) (mangleExpr fn e2)
mangleExpr fn (CutFun  t n vs s es   ) = CutFun  t n (map fn vs) s (map (mangleExpr fn) es)
mangleExpr fn (CutList t n vs   es   ) = CutList t n (map fn vs)   (map (mangleExpr fn) es)
mangleExpr _ (CutRules _) = error "implement this!"

mangleAssign :: (CutVar -> CutVar) -> CutAssign -> CutAssign
mangleAssign fn (var, expr) = (fn var, mangleExpr fn expr)

mangleScript :: (CutVar -> CutVar) -> CutScript -> CutScript
mangleScript fn = map (mangleAssign fn)

-- TODO should this all go in Types?
setReplaceID :: ReplaceID -> CutVar -> CutVar
setReplaceID newID (CutVar _ name) = CutVar newID name

-- TODO pad with zeros?
-- Add a "dupN." prefix to each variable name in the path from independent
-- -> dependent variable, using a list of those varnames
-- changeReplaceID :: String -> (CutVar -> CutVar)
-- changeReplaceID p (CutVar (ReplaceID Nothing)  s) = CutVar (ReplaceID $ Just p) s
-- changeReplaceID p (CutVar (ReplaceID (Just r)) s) = CutVar (ReplaceID $ Just $ digest $ p ++ r) s
-- changeReplaceID salt (CutVar _ s) = CutVar (ReplaceID salt) s

-- TODO should be able to just apply this to a duplicate script section right?
setReplaceIDs :: ReplaceID -> CutScript -> CutScript
setReplaceIDs newID = mangleScript (setReplaceID newID)

-----------------------------------
-- main replace function for PRS --
-----------------------------------

replaceEach :: CutFunction
replaceEach = CutFunction
  { fName      = "replace_each"
  , fFixity    = Prefix
  , fTypeCheck = tReplaceEach
  , fDesc = Nothing, fTypeDesc  = dReplaceEach
  , fRules  = rReplaceEach
  }

tReplaceEach :: [CutType] -> Either String CutType
tReplaceEach (res:sub:(ListOf sub'):[]) | sub == sub' = Right $ ListOf res
tReplaceEach _ = Left "invalid args to replace_each" -- TODO better errors here

dReplaceEach :: String
dReplaceEach = "replace_each : <outputvar> <inputvar> <inputvars> -> <output>.list"

-- TODO how unique should this be?? want as much deduplication as possible while staying safe
calcReplaceID :: CutState -> CutExpr -> CutVar -> CutExpr -> ReplaceID
calcReplaceID (scr, _, _, _) resExpr subVar subExpr = ReplaceID $ Just $ digest
  [ show scr
  , show resExpr
  , show subVar
  , show subExpr
  ]

-- TODO ideally, this shouldn't need any custom digesting? but whatever no
--      messing with it for now
-- TODO can this be parallelized better?
cReplace :: CutState -> CutExpr -> CutVar -> CutExpr -> Rules ExprPath
cReplace st@(script, cfg, ref, ids) resExpr subVar@(CutVar _ _) subExpr = do
  let res  = (CutVar (ReplaceID Nothing) "result", resExpr) -- TODO is this the right salt?
      sub  = (subVar, subExpr)
      deps = filter (\(v,_) -> (elem v $ depsOf resExpr ++ depsOf subExpr)) script
      -- TODO this should be very different right?
      -- pre  = digest $ map show $ res:sub:deps
      -- (ReplaceID s) = saltOf resExpr
             -- Just r  -> digest $ ReplaceID $ show $ fst $ next $ (read r :: StdGen)
      -- newID = case rep of
      --           Nothing -> fail "bad argument to cReplace (no ReplaceID)"
      --           Just r  -> r
      newID = calcReplaceID st resExpr subVar subExpr
      scr' = (setReplaceIDs newID ([sub] ++ deps ++ [res]))
  (ResPath resPath) <- compileScript (scr', cfg, ref, ids) newID -- TODO remove the ID here, or is it useful?
  return (ExprPath resPath) -- TODO this is supposed to convert result -> expr right?

rReplaceEach :: CutState -> CutExpr -> Rules ExprPath
rReplaceEach s@(scr, cfg, ref, _) expr@(CutFun _ _ _ _ (resExpr:(CutRef _ _ _ subVar):subList:[])) = do
  subPaths <- rExpr s subList
  let subExprs = extractExprs scr subList
  resPaths <- mapM (cReplace s resExpr subVar) subExprs
  let subPaths' = (\(ExprPath p) -> toCutPath cfg p) subPaths
      resPaths' = map (\(ExprPath p) -> toCutPath cfg p) resPaths
      outPath   = exprPath s expr
      outPath'  = debugRules cfg "rReplaceEach" expr $ fromCutPath cfg outPath
  outPath' %> \_ ->
    let actFn = if typeOf expr `elem` [ListOf str, ListOf num]
                  then aReplaceEachLits (typeOf expr)
                  else aReplaceEachLinks
    in actFn cfg ref outPath subPaths' resPaths'
  return (ExprPath outPath')
rReplaceEach _ expr = error $ "bad argument to rReplaceEach: " ++ show expr

-- TODO factor out, and maybe unify with rListLits
-- TODO subPaths is only one path? if so, rename it
aReplaceEachLits :: CutType -> CutConfig -> Locks
                -> CutPath -> CutPath -> [CutPath] -> Action ()
aReplaceEachLits _ cfg ref outPath subPaths resPaths = do
  lits <- mapM (readLit cfg ref) resPaths'
  let lits' = map stripWhiteSpace lits
  writeLits cfg ref out lits'
  where
    outPath'  = fromCutPath cfg outPath
    subPaths' = fromCutPath cfg subPaths
    resPaths' = map (fromCutPath cfg) resPaths
    out = debugA cfg "aReplaceEachLits" outPath' (outPath':subPaths':resPaths')

-- TODO factor out, and maybe unify with rListLinks
aReplaceEachLinks :: CutConfig -> Locks -> CutPath -> CutPath -> [CutPath] -> Action ()
aReplaceEachLinks cfg ref outPath subPaths resPaths = do
  need (subPaths':resPaths') -- TODO is needing subPaths required?
  writePaths cfg ref out resPaths
  where
    outPath'  = fromCutPath cfg outPath
    subPaths' = fromCutPath cfg subPaths
    resPaths' = map (fromCutPath cfg) resPaths
    out = debugA cfg "aReplaceEachLinks" outPath' (outPath':subPaths':resPaths')
