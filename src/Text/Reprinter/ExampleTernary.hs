{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Reprinter.ExampleTernary where

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

type AST = [Decl]

data Decl = Decl Span String Expr
  deriving (Data, Show, Typeable)

data Expr =
    App Bool Span Op Expr Expr
  | Var Bool Span String
  | Const Bool Span Int
  deriving (Data, Show, Typeable)

data Op = Add Bool Span deriving (Data, Show, Typeable)

main = go ex1

ex1 = "y = 1   +       x + 0\n" :: Source
-- "y = 1   +       x"

ex2 = "y = 1   +       0    + x\n" :: Source
-- "y = 1   +       x"

ex3 = "y = 0   +       0    + x\n" :: Source
-- "y = x"

ex4 = "y = 1   +       0    + x+w\n" :: Source
--    "y = 1   +       x + w"

-- We then run this through a parser to get an AST, transform the AST,
-- and run this through the reprinter to get:

go src = putStrLn . Text.unpack . refactor $ src

refactor :: Source -> Source
refactor input = runIdentity
               . (\ast -> reprintSort exprReprinter ast input)
               . refactorTouch
               . parse  $ input

{- This achieves the desired effect:

*Main> tryMe
x  =  +(1,2)
y  =  x
// Calculate z
z  =  +( 1,  +(x ,y) )
-}
instance Refactorable Expr where
  isRefactored (App True _ _ _ _)  = Just Replace
  isRefactored (Var True _ _)     = Just Replace
  isRefactored (Const True _ _)   = Just Replace
  isRefactored _               = Nothing

  getSpan (App _ s _ _ _)  = s
  getSpan (Var _ s _)     = s
  getSpan (Const _ s _)   = s

instance Refactorable Op where
  isRefactored (Add True _) = Just Replace
  isRefactored _ = Nothing

  getSpan (Add _ s) = s

exprReprinter :: Reprinting Identity
exprReprinter = catchAll `extQ` reprintExpr `extQ` reprintOp
  where   reprintExpr x =
            genReprinting  (return . Text.pack . pretty) (x :: Expr)
          reprintOp x =
            genReprinting  (return . Text.pack . pretty) (x :: Op)

class Pretty a where
  pretty :: a -> String

-- Expressions can be pretty-printed as
instance Pretty Expr where
    -- pretty (App _ _ op e1 e2) = pretty e1 <> pretty op <> pretty e2
    pretty (Var _ _ n)      = n
    pretty (Const _ _ n)    = show n

instance Pretty Op where
    pretty (Add _ _) = " `add` "

-- Note we are *not* defining a pretty printer for declarations
-- as we are never going to need to regenerate these
--
-- -- Here is a simple AST transformer for replacing both +(e, 0) and +(0, e) with e
-- refactorZero :: AST -> AST
-- refactorZero = refactorLoop refactorZeroOnce
--
-- -- Note that we mark refactored nodes with True in their annotation and the source
-- -- span of the original node
-- refactorZero :: AST -> AST
-- refactorZero = map (\(Decl s n e) -> (Decl s n (go e)))
--   where
--     go (App _ s (Add _ _) e (Const _ _ 0)) = markRefactored (go e) s
--     go (App _ s (Add _ _) (Const _ _ 0) e) = markRefactored (go e) s
--     go (App b s op e1 e2) = App b s op (go e1) (go e2)
--     go e = e
--
--     markRefactored (App _ _ op e1 e2) s = App True s op e1 e2
--     markRefactored (Var _ _ n) s      = Var True s n
--     markRefactored (Const _ _ i) s    = Const True s i
--
-- -- Apply the refactoring in a loop until all zeroes are eliminated, this successfully
-- -- deals with +(0, 0) subexpressions
-- refactorLoop :: (AST -> AST) -> AST -> AST
-- refactorLoop refactoring ast = if refactoring ast == ast
--     then ast
--     else refactorLoop refactoring (refactoring ast)

refactorTouch :: AST -> AST
refactorTouch = map (\(Decl s n e) -> (Decl s n (go e)))
  where
    go (App r s (Add _ s') e1 e2) = App r s (Add True s') (go e1) (go e2)
    go (Var r s n) = Var True s n
    go (Const r s v) = Const True s v

-- eval :: Expr -> State [(String, Int)] (Maybe Int)
-- eval (Plus _ _ e1 e2) = do
--   e1 <- eval e1
--   e2 <- eval e2
--   return ((+) <$> e1 <*> e2)
-- eval (Const _ _ i) = (return . Just) i
-- eval (Var _ _ s) = do
--   l <- get
--   return (lookup s l)





-- The rest is a simple monadic parser for the language.
-- The parser inserts span information into the tree, i.e,
-- the start and end positions of a syntactic fragment
-- this is needed for reprint algorithm and its shape
-- satisfies the WELL-FORMEDNESS-CONDITION for ASTs representing source text

parse :: Source -> AST
parse s = evalState parseDecl (Text.unpack s, initPosition)

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
    isVar <- peekChar isAlpha
    if isVar then do
      name <- many isAlpha
      p2 <- getPos
      continueExpr (Var False (p1, p2) name)
    else do
      num <- many isDigit
      p2 <- getPos
      continueExpr (Const False (p1, p2) $ read num)

continueExpr :: Expr -> Parser Expr
continueExpr expr = do
  spaces
  p1 <- getPos
  isPlus <- charP '+'
  if isPlus then do
    p2 <- getPos
    spaces
    expr' <- parseExpr
    let (_,ub) = getSpan expr'
    let (lb,_) = getSpan expr
    return $ App False (lb,ub) (Add False (p1,p2)) expr expr'
  else return expr




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
