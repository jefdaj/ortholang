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

  -- * Things used in Test
  , spaceChars
  , escapeChars
  , literalChars
  , ParseM
  , ReplM
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
  -- , withReadLock
  -- , withWriteLock
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
  , U.debug -- TODO disambiguate the others

  )
  where

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

import OrthoLang.Util hiding (debug)
import qualified OrthoLang.Util as U

import OrthoLang.Core.Eval (evalFile)
import OrthoLang.Core.Repl (runRepl)
