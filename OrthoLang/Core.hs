module OrthoLang.Core
  (

  -- * New module API
    mkNewBop
  , mkNewFn1
  , mkNewFn2
  , mkNewFn3
  , ActionR
  , ActionR1
  , ActionR2
  , ActionR3
  , newCoreRules
  , newFunctionRules

  -- * Things used in Main
  , runRepl
  , evalFile
  , prettyShow

  -- * Old module API: Types
  , Action1
  , Action2
  , Action3
  , Assign
  , CmdDesc(..)
  , Config(..)
  , Expr(..)
  , ExprPath(..)
  , Function(..)
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
  , Script(..)
  , Type(..)
  , TypeChecker
  , Var(..)
  , VarPath(..)
  , askConfig
  , askLocks
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
  , singleton
  , typeError
  -- , debug TODO disambiguate with Util.debug?

  -- * Old module API: Paths
  , cacheDir
  , exprPath
  , exprPathExplicit
  , fromGeneric
  , fromPath
  , toPath
  , upBy

  -- * Old module API: Locks
  , withReadLock
  , withWriteLock
  , withWriteLock'

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
  -- , debug TODO disambiguate

  )
  where

import OrthoLang.Core.Actions
import OrthoLang.Core.Compile
import OrthoLang.Core.Locks
import OrthoLang.Core.Paths
import OrthoLang.Core.Pretty
import OrthoLang.Core.Sanitize
import OrthoLang.Core.Types
import OrthoLang.Core.Util

import OrthoLang.Core.Eval (evalFile)
import OrthoLang.Core.Repl (runRepl)
