module ShortCut.ReplSpec where

-- This module tests only the REPL, not:
--   the interpreter functions called
--   the scripts called
--   the monad functions called
-- TODO write separate tests for each of those!

import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "loads files, same as the "
  describe "cmdLoad"
  describe "cmdSave" -- TODO rename to write?
  describe "cmdDrop"
  describe "cmdShow"
  describe "cmdType"
  describe ""
  -- TODO assign
  -- TODO print

  -- TODO: remove everything below this
--   describe "finds the [t]ypes of expressions" $ do
-- 
--     describe "tExpr" $ do
--       it "gets correct types of example ParsedExprs" pending
--       prop "gets correct types of generated ParsedExprs" pending
-- 
--   describe "find type [s]ignatures of commands" $ do
-- 
--     -- TODO: rename to tArgs
--     describe "sCmd" $ do
--       it "finds the correct type signature for each command" pending
-- 
--     -- TODO: rename... to what?
--     describe "sCmds" $ do
--       it "finds correct type signatures for example commands" pending
--       prop "finds correct type signatures for generated commands" pending
-- 
--   describe "asserts that types [m]atch" $ do
-- 
--     -- TODO: rename to mBop
--     describe "mExprs" $ do
--       it "tests whether two expressions have matching types" pending
--       prop "is always true when passed the same one twice" pending
-- 
--     describe "mArgs" $ do
--       it "tests whether a list of args has the correct types" pending
--       prop "aborts if the lists are different lengths" pending
--       prop "suceeds with empty lists" pending
-- 
--   describe "[c]hecks ParsedExprs and builds CheckedExprs" $ do
-- 
--     describe "cLit" $ do
--       it "reconstructs the example CheckedLits" pending
--       prop "constructs LitNumbers from random Nums" pending
--       prop "constructs LitFiles from random Strs" pending
-- 
--     describe "cRef" $ do
--       it "reconstructs the example CheckedRefs" pending
--       prop "constructs CheckedRsfs from valid generated Refs" pending
--       prop "fails if given a non-Ref" pending
--       prop "fails when no vars are defined" pending
-- 
--     -- TODO: rename this or cBop so it's obvious that creates CheckedCmds too?
--     describe "cCmd" $ do
--       it "reconstructs the example CheckedCmds" pending
--       prop "constructs CheckedCmds from random valid Cmds" pending
--       prop "fails if given a non-Cmd" pending
--       prop "fails on invalid generated Cmds" pending
-- 
--     describe "cBop" $ do
--       it "reconstructs the example Bop CheckedCmds" pending
--       prop "constructs CheckedCmds from random valid Bops" pending
--       prop "fails if given a non-Bop" pending
--       prop "fails on invalid generated Bops" pending
-- 
--     describe "cExpr" $ do
--       it "reconstructs the example CheckedExprs" pending
--       prop "constructs CheckedExprs from random valid ParsedExprs" pending
--       prop "fails on random invalid ParsedExprs" pending
-- 
--     describe "cScript" $ do
--       it "reconstructs the example TypedScripts" pending
--       prop "concstructs TypedScripts from random valid ParsedScripts" pending
--       prop "fails on random invalid ParsedScripts" pending
