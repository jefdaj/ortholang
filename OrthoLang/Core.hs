{-|
This is a work in progress. It exports everything used by the current code,
and I'm working on simplifying that down to a more principled module API.
-}

module OrthoLang.Core
  (

  -- * New module API
    newBop
  , newFnA1
  , newFnA2
  , newFnA3
  , newFnAT1
  , newFnAT2
  , newFnAT3
  , newFnS1
  , newFnS2
  , newFnS3
  , newFnST1
  , newFnST2
  , newFnST3
  , aNewRulesS1
  , aNewRulesS2
  , aNewRulesS3
  , newMacro
  -- , ActionR
  , NewAction1
  , NewAction2
  , NewAction3
  , newRules
  , Function(..)
  , FnTag(..)
  , NewRules(..)
  , MacroExpansion

  -- * Transitional stuff
  -- , applyList2

  -- * Things used in Main
  , runRepl
  , evalFile
  , prettyShow

  -- * Things used in Test
  , spaceChars
  , escapeChars
  , literalChars
  , ParseM
  , ReplM
  , DigestsRef
  , toGeneric
  , promptArrow
  , mkRepl
  , parseFileIO
  , emptyScript
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
  , parseWithLeftOver -- TODO move to Test/Parse?
  , parseWithEof -- TODO move to Test/Parse?

  -- * Old module API: Types
  , Action1
  , Action2
  , Action3
  , Assign
  , CmdDesc(..)
  , Config(..)
  , Expr(..)
  , ExprPath(..)
  , GlobalEnv
  , IDsRef
  , LocksRef
  , Module(..)
  , Path(..)
  , Pretty
  , RepID(..)
  , ResPath(..)
  , RulesFn
  , Salt(..)
  , Script
  , Type(..)
  , TypeChecker
  , Var(..)
  , VarPath(..)
  -- , askConfig
  -- , askLocks
  , defaultShow
  , depsOf
  , extOf
  , extractExprs
  , lookupIDsFile
  , mkTypeDesc
  , num
  , saltOf
  , setSalt
  , str
  , typeOf

  -- * Old module API: Actions
  , CompiledExpr(..)
  , absolutizePaths
  , cachedLinesPath
  , debugA
  , hashContent
  , lookupID
  , need'
  , readFileStrict'
  , readLit
  , readLits
  , readPath
  , readPaths
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
  , curl
  , debugRules
  , defaultTypeCheck
  , map3of3
  , mkLoad
  , mkLoadList
  , rExpr
  , rFun3
  , rMap
  , rMapSimpleScript
  , rMapTmps
  , rSimple
  , rSimpleScript
  , rSimpleScriptPar
  , rSimpleTmp
  , typeError
  -- , debug TODO disambiguate with Util.debug?

  -- * Old module API: Paths
  , cacheDir
  , exprPath
  , unsafeExprPathExplicit
  , fromGeneric
  , fromPath
  , toPath
  , upBy

  -- * Old module API: Locks
  -- , withReadLock
  -- , withWriteLockEmpty
  -- , withWriteLock'

  -- * Old module API: Sanitize
  , unhashIDs

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

  )
  where

import OrthoLang.Debug
import OrthoLang.Core.Actions
import OrthoLang.Core.Compile
import OrthoLang.Core.Compile.NewRules
import OrthoLang.Core.Paths
import OrthoLang.Core.Pretty
import OrthoLang.Core.Sanitize
import OrthoLang.Core.Types
import OrthoLang.Core.Parse
import OrthoLang.Core.Parse.Util
import OrthoLang.Core.Repl hiding (runCmd) -- TODO disambiguate

import OrthoLang.Util -- TODO remove?

import OrthoLang.Core.Eval (evalFile)
import OrthoLang.Core.Repl (runRepl)
