{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module ReprinterSpec where

import Control.Monad.State
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

  describe "splicer" $ do
    it "50 column line limit" $ do
      refactorSplicer input_long
        `shouldBe` "x = +(+(long_name_one, 0)\\\n\
                   \    , long_name_two)\n"

  describe "Context reprinter" $ do
    it "Different approach to appended comments" $
      refactor4 ctxInput `shouldBe` ctxOutput

ctxInput :: String
ctxInput = "x = +(1,2)\n\
           \y  =  +(x,0)\n\
           \w = 2\n\
           \// Calculate z\n\
           \z  =  +( 1,  +(+(0,x)  ,y) )\n"

ctxOutput :: String
ctxOutput = "x = +(1,2) // x = 3\n\
            \y  =  +(x,0) // y = 3\n\
            \w = 2\n\
            \// Calculate z\n\
            \z  =  +( 1,  +(+(0,x)  ,y) ) // z = 7\n"

input_simple :: String
input_simple = "x  = +(1,0)\n"

input_long :: String
input_long = "x = +(long_name_one, long_name_two)\n"

type AST' = AST Bool

-- Apply zero-refactoring in a loop: deals with +(0, 0) subexpressions
refactorZeroLoop :: AST' -> AST'
refactorZeroLoop = refactorLoop refactorZero

-- Apply the refactoring in a loop until a pass makes no changes.
refactorLoop :: (AST' -> AST') -> AST' -> AST'
refactorLoop refactoring ast
  | refactoring ast == ast = ast
  | otherwise              = refactorLoop refactoring (refactoring ast)

-- We then run this through a parser to get an AST, transform the AST,
-- and run this through the reprinter to get:
refactorSplicer :: String -> String
refactorSplicer input =
  ( flip evalState                            initPosition
    . flip (reprint exprReprinter $ splicer 30) input
    . addZero
    . parse
    )
    input

-- Checks that we're looking at a top level expression
commentPrinter2 :: ZipperReprinting String AST' (State [(String, Int)])
commentPrinter2 z = case getHole @(Expr Bool) z of
  Just Plus{} -> addComment
  Just Var{} -> addComment
  _ -> pure Nothing
 where
  addComment = case up z >>= getHole @(Decl Bool) of
    Nothing -> pure Nothing
    Just (Decl _ s v e) -> do
      val <- eval e
      case val of
        Nothing -> pure Nothing
        Just val -> do
          modify ((v, val) :)
          let msg = " // " ++ v ++ " = " ++ show val
          return (Just (After, msg, s))

refactor4 :: String -> String
refactor4 input =
  ( flip evalState []
  . flip (reprintSort commentPrinter2 pure) input
  . parse
  ) input

-- This is pointless but it demonstrates the splicer by taking lines over a limit
addZero :: AST' -> AST'
addZero = map (\(Decl b s n e) -> (Decl b s n (go e)))
 where
  go (Var u sp var   ) = Plus True sp (Var u sp var) (Const u sp 0)
  go (Plus u sp e1 e2) = Plus u sp (go e1) e2
  go e                 = e

-- Splicer that makes anything over 50 a newline
splicer :: Int -> String -> State Position String
splicer n source = do
  let (pre, post) = span (/= '\n') source
      preLen      = length pre
      postLen     = length . takeWhile (/= '\n') . reverse $ post
      nl          = "\\\n    "
  (Line x, Col y) <- get
  res             <- if y + preLen <= n
    then pre <$ put (Line x, Col $ preLen + y)
    else nl <> pre <$ put (Line $ x + 1, Col 4)
  if postLen == 0
    then pure $ res <> post
    else do
      post' <- splicer n post
      pure $ res <> post'