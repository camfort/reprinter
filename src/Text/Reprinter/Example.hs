{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Reprinter.Example where

import Control.Monad.State
import qualified Data.Text.Lazy as Text
import Data.Char
import Data.Functor.Identity
import Data.Generics
import Data.Generics.Uniplate.Data
import Data.Monoid ((<>))
import Text.Reprinter

{- Demonstration of the reprinter algorithm on a simple
   SSA-like language with integer addition, variables, and constants -}


-- Here is the AST for the language


data AST a =
     Seq a (Decl a) (AST a)
  |  Nil a
  deriving (Data, Typeable)

data Decl a =
    Decl a Span String (Expr a)
  deriving (Data, Typeable)

data Expr a =
     Plus a Span (Expr a) (Expr a)
  |  Var a Span String
  |  Const a Span Int
  deriving (Data, Typeable)



main = tryMe

exampleSource = "x = +(1,2)\n\
                \y  =  +(x,0)\n\
                \// Calculate z\n\
                \z  =  +( 1,  +(+(0,x)  ,y) )\n"

-- We then run this through a parser to get an AST, transform the AST,
-- and run this through the reprinter to get:

tryMe = putStrLn . Text.unpack . refactor $ exampleSource

refactor :: Source -> Source
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
instance Refactorable (Expr Bool) where
  isRefactored (Plus True _ _ _)  = Just Replace
  isRefactored (Var True _ _)     = Just Replace
  isRefactored (Const True _ _)   = Just Replace
  isRefactored _                  = Nothing

  getSpan (Plus _ s _ _)  = s
  getSpan (Var _ s _)     = s
  getSpan (Const _ s _)   = s

exprReprinter :: Reprinting Identity
exprReprinter = catchAll `extQ` reprintExpr
  where   reprintExpr x =
            genReprinting  (return . prettyExpr) (x :: Expr Bool)

-- Expressions can be pretty-printed as
prettyExpr :: Expr a -> Source
prettyExpr (Plus _ _ e1 e2) = "+(" <> prettyExpr e1 <> ", " <> prettyExpr e2 <> ")"
prettyExpr (Var _ _ n)      = Text.pack n
prettyExpr (Const _ _ n)    = Text.pack $ show n

-- Note we are *not* defining a pretty printer for declarations
-- as we are never going to need to regenerate these

-- Here is a simple AST transformer for replace +(e, 0) with e and +(0, e) with e
refactorZero :: AST Bool -> AST Bool
refactorZero (Nil a) = Nil a
refactorZero (Seq a (Decl a' s n e) d) =
    Seq a (Decl a' s n (refactorExpr e)) (refactorZero d)
  where
    refactorExpr (Plus a s e (Const _ _ 0)) = markRefactored (refactorExpr e) s
    refactorExpr (Plus a s (Const _ _ 0) e) = markRefactored (refactorExpr e) s
    refactorExpr (Plus a s e1 e2) = Plus a s (refactorExpr e1) (refactorExpr e2)
    refactorExpr e = e

    markRefactored (Plus _ _ e1 e2) s = Plus True s e1 e2
    markRefactored (Var _ _ n) s      = Var True s n
    markRefactored (Const _ _ i) s    = Const True s i

-- Note that we mark refactored nodes with True in their annotation and the source
-- span of the original node

-- The rest is a simple monadic parser for the language.
-- The parser inserts span information into the tree, i.e,
-- the start and end positions of a syntactic fragment
-- this is needed for reprint algorithm and its shape
-- satisfies the WELL-FORMEDNESS-CONDITION for ASTs representing source text

parse :: Source -> AST Bool
parse s = evalState parseDecl (Text.unpack s, initPosition)

type Parser = State (String, Position)

parseDecl :: Parser (AST Bool)
parseDecl = do
   (xs, p1) <- get
   case xs of
       [] -> return $ Nil False
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
             return $ Seq False (Decl False (p1, p2) name expr) rest

commentPrefix :: String -> Maybe (String, String)
commentPrefix [] = Nothing
commentPrefix (' ':xs) = commentPrefix xs
commentPrefix ('/':'/':xs) = Just $ break (== '\n') xs
commentPrefix _ = Nothing

parseExpr :: Parser (Expr Bool)
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
