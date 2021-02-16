Scrap Your Reprinter: Example
=============================

Reprinting takes a source file and its (possible transformed) AST and
"stitches" them together into a new source file. This library provides
a generic reprinting algorithm that works on any AST with some modest
requirements. Where any changes to the AST have been made the
reprinting algorithm can be parameterised to hook into
application-specific functionality for handling nodes in the AST that
have been marked as transformed (e.g., applying a pretty printer to
these parts).

This module gives an introduction to library usage. For a better view
of the library itself, [the 2017
paper](https://www.cs.kent.ac.uk/people/staff/dao7/publ/reprinter2017.pdf)
goes over implementation in depth. (This module is adapted from
Section 3.4.)

We demonstrate the library on a limited integer expression language (reused for
the library tests). This is a literate Haskell/Markdown file, so feel free to
follow along in GHCi or your favourite text viewer.

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}

module Text.Reprinter.Example where

import Text.Reprinter
import Control.Monad.State      -- for later example
import Data.Char                -- for parsing
\end{code}

Introduction
------------
*(Section 1 of the 2017 paper covers this in better detail.)*

A compiler translates source code to a target language. Sometimes when writing
language tools, you may find yourself writing a compiler where the source and
target languages are the same; automated code refactoring tools in IDEs
provide a common set of examples. Such tools must be careful not to remove
*secondary notation* like whitespace and comments. This, in short, can be a
pain to do well.

The reprinter library allows you to write a reprinter for any algebraic data
type supporting a minimal interface the algorithm needs to track changes.

This module designs a whitespace-flexible language with comments, and uses the
library to allow reprinting that preserves such secondary notation.

Language definition
-------------------
Let's take a language targeting integer addition, plus variable assignments. Our
top-level type will be an SSA-like list of *variable declaration-assignments*:

\begin{code}
type AST a = [Decl a]
data Decl a = Decl a Span String (Expr a)
    deriving (Eq, Data, Typeable, Show)
\end{code}

A `Decl a span var expr` represents the assignment of the value of an
expression `expr` to a variable `var`. The AST is composed of a sequence
(list) of these `Decl`s.

Expressions are formed of variables, literals, and additions over expressions:

\begin{code}
data Expr a
  = Plus a Span (Expr a) (Expr a)
  | Var a Span String
  | Const a Span Int
    deriving (Eq, Data, Typeable, Show)
\end{code}

For our reprinting algorithm, every refactorable node in the AST must
store position information (`Span`, i.e., the start and end point of
this piece of syntax in the source code text) and whether it's been
refactored (and thus needs reprinting). In this case, we've
parameterised our AST over an arbitrary type `a`, which we specialise
in the rest of this file to `Bool` to represent whether this node has
been refactored or not. In a more complex AST, you could add this as a
field to an existing node annotation record type.

Note that the algorithm requires ASTs to have `Data` and `Typeable` instances.
Deriving these automatically requires the `DeriveDataTypeable` language pragma.

*(Section 1.1 in the 2017 paper gives an illustrated step-by-step example of a
transformation and reprint.)*

Concrete syntax and goals
-------------------------
Let's digress for a while to discuss our language's concrete syntax, since
reprinting uses abstract and concrete syntax simultaneously. Our language is
going to look something like this:

\begin{code}
exBasic :: String
exBasic = "x = +(0,1)\n"
\end{code}

We permit arbitrary spacing for prettier code, like so:

\begin{code}
exPrettier :: String
exPrettier = unlines
  [ "var = +(0  , 1)"
  , "x   = +(var, 2)"
  ]
\end{code}

And lines can be empty, or comments:

\begin{code}
exComment :: String
exComment = unlines
  [ "// slightly superfluous variable"
  , "zero = 0"
  , ""
  , "// somewhat useful variable"
  , "x = +(zero, 1)"
  ]
\end{code}

Knowing all this, our aim is to take a formatted program source, parse it, apply
a transformation to the AST, then reprint the program while keeping the original
formatting. Starting with the given source

\begin{code}
exPaper :: String
exPaper = unlines
  [ "x = +(1,2)"
  , "y  =  +(x,0)"
  , "// Calculate z"
  , "z  =  +( 1,  +(+(0,x) ,y) )"
  ]
\end{code}

We'll produce the following refactored and reprinted output:

    > putStr exPaper
    x = +(1,2)
    y  =  +(x,0)
    // Calculate z
    z  =  +( 1,  +(+(0,x) ,y) )
    > (putStr . refactor) exPaper
    x = +(1,2)
    y  =  x
    // Calculate z
    z  =  +( 1,  +(x ,y) )
    >

Writing a transformation
------------------------
Putting concrete syntax aside, let's write a transformation for our AST - a
refactoring. A nice obvious one is replacing `x+0` (and `0+x`) expressions with
just `x`.

\begin{code}
refactorZero :: AST Bool -> AST Bool
refactorZero = map $ \(Decl a s n e) -> Decl a s n (go e)
  where
    go (Plus _ s e (Const _ _ 0)) = markRefactored (go e) s
    go (Plus _ s (Const _ _ 0) e) = markRefactored (go e) s
    go (Plus a s e1 e2) = Plus a s (go e1) (go e2)
    go e = e

    markRefactored (Plus _ _ e1 e2) s = Plus True s e1 e2
    markRefactored (Var _ _ n) s      = Var True s n
    markRefactored (Const _ _ i) s    = Const True s i
\end{code}

Note that when marking nodes as refactored (`markRefactored`), we
replace the `Span` of the refactored node with the span of the
original `x+0` node- this allows the reprinting algorithm to replace
the original part of the source code with the new refactored node.

In concrete syntax, we're making changes like:

    + ( x , 0 )    becomes
    x

See how `x` is pulled out. The `+(x,y)` expression is directly replaced with
`x`, so we make sure to use the original span. Any comments following the
expression will be `shifted' - *not* removed, because the reprinter only makes
changes when a node in the AST indicates it has been refactored. Parts of a
source file that aren't captured in the AST will be printed with no changes.

Reprinter plumbing
------------------
We have an AST and a transformation on it. Next, we need to tell the reprinter
how to use our AST.

\begin{code}
-- FlexibleInstances used to define this without a wrapper
instance Refactorable (Expr Bool) where
  isRefactored (Plus True _ _ _) = Just Replace
  isRefactored (Var True _ _)    = Just Replace
  isRefactored (Const True _ _)  = Just Replace
  isRefactored _                 = Nothing

  getSpan (Plus _ s _ _) = s
  getSpan (Var _ s _)    = s
  getSpan (Const _ s _)  = s
\end{code}

Your AST's `Refactorable` instances will depend on how you store annotations in
your tree. Likely you store refactoring information inside a larger record type.
Perhaps you disallow refactoring at the type level for certain nodes. In this
case, we're only writing an instance for `Expr`s, because we don't reprint
`Decl`s directly. (If we wrote a variable renaming transformation, then it would
be needed.)

We're almost there. Next is defining a generic query telling what to
do when encountering a refactored expression during reprinting. When
this happens, we're going to need to turn ASTs (which may be
completely new and distinct from the original source code) into
concrete syntax, so we'll need an expression pretty printer as well.

TODO: This uses Scrap your Boilerplate (SYB) directly. Check the SYB
documentation and the 2017 paper.

\begin{code}
exprReprinter :: Reprinting String Identity
exprReprinter = catchAll `extQ` reprintExpr
  where
    reprintExpr x = genReprinting (return . prettyExpr) (x :: Expr Bool)

-- | Print an expression in canonical string form.
prettyExpr :: Expr a -> String
prettyExpr (Plus _ _ e1 e2) = "+(" <> prettyExpr e1 <> ", " <> prettyExpr e2 <> ")"
prettyExpr (Var _ _ n)      = n
prettyExpr (Const _ _ n)    = show n

-- Note that we don't define a pretty printer for declarations, as we're not
-- refactoring on that level, so won't ever reprint them.
\end{code}

Finally, we put together a function that parses, runs our refactoring, then
reprints.

\begin{code}
-- | Parse and refactor, then run the reprinter with the original source and
--   updated AST.
refactor :: String -> String
refactor s =
      runIdentity
    . flip (reprint exprReprinter) s
    . refactorZero
    . parse $ s

\end{code}

Further example: reprinting `After`
-----------------------------------
Comment reprinter, rewritten using version in tests. Actually runs on every
expression, not just refactored ones?

TODO: Explained in 2017 paper, Section 3.4.

\begin{code}
commentPrinter :: Reprinting String (State [(String, Int)])
commentPrinter = catchAll `extQ` decl
  where
    decl (Decl _ s v e) = do
      val <- eval (e :: Expr Bool)
      case val of
        Nothing -> return $ Nothing
        Just val -> do
          modify ((v,val) :)
          let msg = " // " <> v <>" = " <> show val
          return $ Just (After, msg, s)

eval :: Expr a -> State [(String, Int)] (Maybe Int)
eval (Plus _ _ e1 e2) = do
  e1' <- eval e1
  e2' <- eval e2
  return $ (+) <$> e1' <*> e2'
eval (Const _ _ i) = return $ Just i
eval (Var _ _ s) = get >>= return . lookup s

refactorComment :: String -> String
refactorComment input =
      flip evalState []
    . flip (reprint commentPrinter) input
    . parse $ input
\end{code}

The remainder of this module defines a simple monadic parser for the language.
It attempts to generate a position-tagged AST from a `String`.

\begin{code}
parse :: String -> AST Bool
parse s = evalState parseDecl (s, initPosition)

type Parser = State (String, Position)

parseDecl :: Parser (AST Bool)
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
             return $ Decl False (p1, p2) name expr : rest

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
       [] -> error $ "Expecting " ++ [c] ++ " but got empty"

charP :: Char -> Parser Bool
charP c =  do
    (xs, pos) <- get
    case xs of
       (x:xs') -> if x == c
                then do
                   put (xs', advanceCol pos)
                   return True
                else return False
       [] -> error $ "Expecting " ++ (c : " but got empty")

peekChar :: (Char -> Bool) -> Parser Bool
peekChar p =  do
    (xs, pos) <- get
    case xs of
       (x:_) -> if p x
                then return True
                else return False
\end{code}
