{-|
This is a work in progress. It exports everything used by the current code,
and I'm working on simplifying that down to a more principled module API.
-}

module OrthoLang.Interpreter
  (

  -- * New module API
    newBop
  , newFnA1
  , newFnA2
  , newFnA3
  , newFnA4
  , newFnS1
  , newFnS2
  , newFnS3
  , newFnS4
  , aNewRulesS1
  , aNewRulesS2
  , aNewRulesS3
  , newExprExpansion
  -- , ActionR
  , NewAction1
  , NewAction2
  , NewAction3
  , NewAction4
  , newRules
  -- , Function(..)
  -- , FnTag(..)
  -- , NewRules(..)
  , newMap1of1
  , newMap2of2
  , newMap2of3
  , newMap3of3
  , newMap4of4
  , newDate1of1
  , newDate1of2
  , newDate1of3
  , newDate1of4
  , path2info
  , info2path

  , ExprExpansion
  , addDigest
  , decodeNewRulesType
  , decodeNewRulesDeps
  , countLines
  , hidden
  , aLoadIDs

  -- * Transitional stuff
  -- , applyList2

  -- * Things used in Main
  , runRepl
  , evalFile

  -- * Things used in Test
  , help
  , helpTopics
  , spaceChars
  , escapeChars
  , literalChars
  -- , ParseM
  -- , ReplM
  -- , DigestsRef
  , toGeneric
  , promptArrow
  , mkRepl
  , parseFileIO
  -- , emptyScript
  , pQuoted
  , pNum
  , pFunName
  , pVarEq
  , pSym
  , pVar
  , pStatement
  , pExpr
  , pFun
  , pTerm
  -- , parseWithLeftOver -- TODO move to Test/Parse?
  , parseWithEof -- TODO move to Test/Parse?

  -- * Old module API: Types
--   , Action1
--   , Action2
--   , Action3
--   , Assign
  , CmdDesc(..)
--   , Config(..)
--   , Expr(..)
--   , ExprPath(..)
--   , GlobalEnv
--   , IDs(..)
--   , IDsRef
--   , LocksRef
--   , Module(..)
--   , Path(..)
  -- , Pretty
--   , RepID(..)
--   , ResPath(..)
--   , RulesFn
--   , Seed(..)
--   , Script
--   , Type(..)
--   , Encoding(..)
--   , TypeGroup(..)
--   , TypeSig(..)
--   -- , TypeChecker
--   , Var(..)
--   , VarPath(..)
--   -- , askConfig
--   -- , askLocks
--   , defaultShow
--   , depsOf
--   , ext
--   , extractExprs
--   , lookupIDsFile
--   -- , lookupHashesFile
--   , num
--   , seedOf
--   , setSeed
--   , str
--   , lit
--   , typeOf

  -- * Old module API: Actions
  -- , MappedExpr(..)
  , absolutizePaths
  , cachedLinesPath
  , debugA
  , hashContent
  , lookupID
  , need'
  , readFileStrict'
  , readLit
  , readLitPath
  , readLitPaths
  , readLits
  , readPath
  , readPaths
  , readString
  , readStrings
  , runCmd
  , sanitizeFileInPlace
  , symlink
  , traceA
  , trackWrite'
  , withBinHash
  , writeCachedLines
  , writeCachedVersion
  , writeLit
  , writeLits
  , writePath
  , writePaths
  , writeStrings

  -- * Old module API: Compile
  , aSimpleScript
  , aSimpleScriptNoFix
  , aSimpleScriptPar
  , compileScript
  , compose1
  , debugRules
  -- , defaultTypeCheck
  , map3of3
  , rExpr
  , rFun3
  , rMap
  , rMapSimpleScript
  , rMapTmps
  , rSimple
  , rSimpleScript
  , rSimpleScriptPar
  , rSimpleTmp
  -- , debug TODO disambiguate with Util.debug?

  -- * Old module API: Paths
  , cacheDir
  , exprPath
  , unsafeExprPathExplicit
  -- , argHashes
  , fromGeneric
  , fromPath
  , toPath
  , pathString
  , stringPath
  , upBy
  , isURL

  -- * Old module API: Locks
  -- , withReadLock
  -- , withWriteLockEmpty
  -- , withWriteLock'

  -- * Old module API: Sanitize
  , unhashIDs
  , unhashIDsFile

  -- * Old module API: Util
  , digest
  , headOrDie
  , justOrDie
  , readFileStrict
  , removeIfExists
  , resolveSymlinks
  , stripWhiteSpace
  , trace
  , unlessExists
  , debug -- TODO disambiguate the others

  -- * Old module API: Sanitize
  , hashIDsFile
  , readIDs

  )
  where

import OrthoLang.Debug
import OrthoLang.Interpreter.Actions
import OrthoLang.Interpreter.Compile
import OrthoLang.Interpreter.Compile.NewRules
import OrthoLang.Interpreter.Paths
import OrthoLang.Interpreter.Sanitize
import OrthoLang.Interpreter.Parse
import OrthoLang.Interpreter.Parse.Util
import OrthoLang.Interpreter.Repl hiding (runCmd) -- TODO disambiguate

import OrthoLang.Util -- TODO remove?

import OrthoLang.Interpreter.Eval (evalFile)
import OrthoLang.Interpreter.Repl (runRepl)
