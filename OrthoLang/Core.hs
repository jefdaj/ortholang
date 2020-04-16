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
  , newFnS1
  , newFnS2
  , newFnS3
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
  , expandMacros
  , prettyShow

  -- * Things used in Test
  , help
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
  , IDs(..)
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
  , Encoding(..)
  , TypeGroup(..)
  , TypeSig(..)
  -- , TypeChecker
  , Var(..)
  , VarPath(..)
  -- , askConfig
  -- , askLocks
  , defaultShow
  , depsOf
  , tExtOf
  , extractExprs
  , lookupIDsFile
  -- , lookupHashesFile
  , mkTypeDesc
  , num
  , saltOf
  , setSalt
  , str
  , lit
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
  , fromGeneric
  , fromPath
  , toPath
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
  , hashIDsFile2
  , readIDs

  )
  where

import OrthoLang.Debug
import OrthoLang.Core.Actions
import OrthoLang.Core.Expand
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
