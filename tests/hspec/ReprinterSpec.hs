{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module ReprinterSpec where

import Text.Reprinter
import Text.Reprinter.Example

import Test.Hspec

-- These tests use definitions from the example module 'Text.Reprinter.Example'.

-- Note that 'unlines' appends a newline on to _every_ string, including the
-- last one.
spec :: Spec
spec = do
  describe "refactor" $ do
    it "removes additions of zeroes" $ do
      refactor exPaper `shouldBe` unlines
        [ "x = +(1,2)"
        , "y  =  x"
        , "// Calculate z"
        , "z  =  +( 1,  +(x ,y) )"
        ]
    it "removes additions of zeroes" $ do
      refactor input_simple `shouldBe` "x  = 1\n"

  describe "refactorComment" $ do
    it "appends evaluated variables in comments" $ do
      refactorComment exPaper `shouldBe` unlines
        [ "x = +(1,2) // x = 3"
        , "y  =  +(x,0) // y = 3"
        , "// Calculate z"
        , "z  =  +( 1,  +(+(0,x) ,y) ) // z = 7"
        ]
    it "appends evaluated variables in comments" $ do
      refactorComment input_simple `shouldBe` "x  = +(1,0) // x = 1\n"

input_simple :: String
input_simple = "x  = +(1,0)\n"

type AST' = AST Bool

-- Apply zero-refactoring in a loop: deals with +(0, 0) subexpressions
refactorZeroLoop :: AST' -> AST'
refactorZeroLoop = refactorLoop refactorZero

-- Apply the refactoring in a loop until a pass makes no changes.
refactorLoop :: (AST' -> AST') -> AST' -> AST'
refactorLoop refactoring ast
  | refactoring ast == ast = ast
  | otherwise              = refactorLoop refactoring (refactoring ast)
