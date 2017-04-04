module ShortCut.Core.Repl.Tests where

import ShortCut.Core.Types (CutConfig)
import ShortCut.Core.Util  (mkTestGroup)
import Test.Tasty          (TestTree)

mkTests :: CutConfig -> IO TestTree
mkTests cfg = mkTestGroup cfg "Repl" []

-- TODO have to fix eLine before getting anything out of the repl at all!

-- TODO then, the plan is:
-- * paste entire repl sessions in "golden" repl files
-- * split them into alternating blocks of stdin, stdout
-- * require that both together match the first string
-- * set tmpdir, but script is Nothing for now
