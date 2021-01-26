{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module ReprinterSpec where

import Control.Monad.State
import qualified Data.Text.Lazy as Text
import Data.Char
import Data.Monoid ((<>))
import Text.Reprinter
import Data.String (IsString(..))

import Test.Hspec

spec :: Spec
spec = do
  describe "refactor" $ do
    it "removes additions of zeroes" $ do
      refactor input `shouldBe` "x = +(1,2)\n\
                                \y  =  x\n\
                                \// Calculate z\n\
                                \z  =  +( 1,  +(x  ,y) )\n"
    it "removes additions of zeroes" $ do
      refactor input_simple `shouldBe` "x  = 1\n"

  describe "refactor2" $ do
    it "appends evaluated variables in comments" $ do
      refactor2 input `shouldBe` "x = +(1,2) // x = 3\n\
                                  \y  =  +(x,0) // y = 3\n\
                                  \// Calculate z\n\
                                  \z  =  +( 1,  +(+(0,x)  ,y) ) // z = 7\n"
    it "appends evaluated variables in comments" $ do
      refactor2 input_simple `shouldBe` "x  = +(1,0) // x = 1\n"


input :: Text.Text
input = "x = +(1,2)\n\
        \y  =  +(x,0)\n\
        \// Calculate z\n\
        \z  =  +( 1,  +(+(0,x)  ,y) )\n"

input_simple :: Text.Text
input_simple = "x  = +(1,0)\n"


-- We then run this through a parser to get an AST, transform the AST,
-- and run this through the reprinter to get:

test = putStrLn . Text.unpack . refactor


type AST = [Decl]

data Decl = Decl Span String Expr
  deriving (Data, Eq, Typeable)

data Expr
  = Plus Bool Span Expr Expr
  | Var Bool Span String
  | Const Bool Span Int
  deriving (Data, Eq, Typeable)


refactor :: StringLike a => a -> a
refactor input = runIdentity
               . (\ast -> reprint exprReprinter ast input)
               . refactorZero
               . parse  $ input

{- This achieves the desired effect:

*Main> tryMe
x  =  +(1,2)
y  =  x
// Calculate z
z  =  +( 1,  +(x ,y) )
-}
instance Refactorable Expr where
  isRefactored (Plus True _ _ _)  = Just Replace
  isRefactored (Var True _ _)     = Just Replace
  isRefactored (Const True _ _)   = Just Replace
  isRefactored _               = Nothing

  getSpan (Plus _ s _ _)  = s
  getSpan (Var _ s _)     = s
  getSpan (Const _ s _)   = s

exprReprinter :: Reprinting Identity
exprReprinter = catchAll `extQ` reprintExpr
  where   reprintExpr x =
            genReprinting  (return . prettyExpr) (x :: Expr)

-- Expressions can be pretty-printed as
prettyExpr :: StringLike a => Expr -> a
prettyExpr (Plus _ _ e1 e2) = "+(" <> prettyExpr e1 <> ", " <> prettyExpr e2 <> ")"
prettyExpr (Var _ _ n)      = fromString n
prettyExpr (Const _ _ n)    = fromString $ show n

-- Note we are *not* defining a pretty printer for declarations
-- as we are never going to need to regenerate these

-- Here is a simple AST transformer for replacing both +(e, 0) and +(0, e) with e
refactorZero :: AST -> AST
refactorZero = refactorLoop refactorZeroOnce

-- Note that we mark refactored nodes with True in their annotation and the source
-- span of the original node
refactorZeroOnce :: AST -> AST
refactorZeroOnce = map (\(Decl s n e) -> (Decl s n (go e)))
  where
    go (Plus _ s e (Const _ _ 0)) = markRefactored (go e) s
    go (Plus _ s (Const _ _ 0) e) = markRefactored (go e) s
    go (Plus b s e1 e2) = Plus b s (go e1) (go e2)
    go e = e

    markRefactored (Plus _ _ e1 e2) s = Plus True s e1 e2
    markRefactored (Var _ _ n) s      = Var True s n
    markRefactored (Const _ _ i) s    = Const True s i

-- Apply the refactoring in a loop until all zeroes are eliminated, this successfully
-- deals with +(0, 0) subexpressions
refactorLoop :: (AST -> AST) -> AST -> AST
refactorLoop refactoring ast = if refactoring ast == ast
    then ast
    else refactorLoop refactoring (refactoring ast)



eval :: Expr -> State [(String, Int)] (Maybe Int)
eval (Plus _ _ e1 e2) = do
  e1 <- eval e1
  e2 <- eval e2
  return ((+) <$> e1 <*> e2)
eval (Const _ _ i) = (return . Just) i
eval (Var _ _ s) = do
  l <- get
  return (lookup s l)

commentPrinter :: Reprinting (State [(String, Int)])
commentPrinter = catchAll `extQ` decl
  where
  decl (Decl s v e) = do
    val <- eval (e :: Expr)
    case val of
      Nothing -> return Nothing
      Just val -> do
        modify ((v,val) :)
        let msg = " // " ++ v ++ " = " ++ show val
        return (Just (After, fromString msg, s))

refactor2 :: StringLike a => a -> a
refactor2 input =
  (  flip evalState []
  .  flip (reprint commentPrinter) input
  .  parse
  ) input

test2 = putStrLn . Text.unpack . refactor2







-- The rest is a simple monadic parser for the language.
-- The parser inserts span information into the tree, i.e,
-- the start and end positions of a syntactic fragment
-- this is needed for reprint algorithm and its shape
-- satisfies the WELL-FORMEDNESS-CONDITION for ASTs representing source text

parse :: StringLike a => a -> AST
parse s = evalState parseDecl (slToList s, initPosition)

type Parser = State (String, Position)

parseDecl :: Parser AST
parseDecl = do
    (xs, p1) <- get
    case xs of
      [] -> return []
      ('\n':xs) -> do
        put (xs, advanceLine p1)
        parseDecl
      _ -> do
        case commentPrefix xs of
          Just (comment, rest) -> do
            put (rest, p1)
            parseDecl
          Nothing -> do
            name <- many isAlpha
            spaces
            char '='
            spaces
            expr <- parseExpr
            p2 <- getPos
            char '\n'
            (xs, p') <- get
            put (xs, advanceLine p')
            rest <- parseDecl
            return $ (Decl (p1, p2) name expr) : rest

commentPrefix :: String -> Maybe (String, String)
commentPrefix [] = Nothing
commentPrefix (' ':xs) = commentPrefix xs
commentPrefix ('/':'/':xs) = Just $ break (== '\n') xs
commentPrefix _ = Nothing

parseExpr :: Parser Expr
parseExpr = do
    p1 <- getPos
    isPlus <- charP '+'
    if isPlus then do
      char '('
      spaces
      n <- parseExpr
      spaces
      charP ','
      spaces
      m <- parseExpr
      spaces
      char ')'
      p2 <- getPos
      return $ Plus False (p1, p2) n m
    else do
       isVar <- peekChar isAlpha
       if isVar then do
           name <- many isAlpha
           p2 <- getPos
           return $ Var False (p1, p2) name
       else do
           num <- many isDigit
           p2 <- getPos
           return $ Const False (p1, p2) $ read num

-- Some monadic parser helpers (standard)


getPos :: Parser Position
getPos = do
   (_, p) <- get
   return p

many :: (Char -> Bool) -> Parser String
many p = do
    (xs, pos) <- get
    case xs of
      (x:xs) | p x -> do
          put (xs, advanceCol pos)
          ys <- many p
          return $ x : ys
      _ -> return ""

spaces = many (==' ')

char :: Char -> Parser ()
char c = do
    (xs, pos) <- get
    case xs of
       (x:xs') -> if x == c
                then do
                  put (xs', advanceCol pos)
                  return ()
                else error $ "Expecting " ++ [c] ++ " but got " ++ [x]
       _ -> error $ "Expecting " ++ [c] ++ " but got empty"

charP :: Char -> Parser Bool
charP c =  do
    (xs, pos) <- get
    case xs of
       (x:xs') -> if x == c
                then do
                   put (xs', advanceCol pos)
                   return True
                else return False
       _ -> error $ "Expecting " ++ (c : " but got empty")

peekChar :: (Char -> Bool) -> Parser Bool
peekChar p =  do
    (xs, pos) <- get
    case xs of
       (x:_) -> if p x
                then return True
                else return False
