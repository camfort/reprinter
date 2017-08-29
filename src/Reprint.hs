{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Reprint
  ( reprint
  , splitBySpan
  , Position(..)
  , Source
  , Reprinting
  , initPosition
  , catchAll
  , genReprinting
  , Refactorable(..)
  , RefactorType(..)
  ) where

import Data.Generics.Zipper
import Debug.Trace
import qualified Data.ByteString.Char8 as B
import Data.Data
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy

type Source    = B.ByteString

data Position = Position { posColumn :: Int, posLine :: Int }
  deriving (Data, Show)

initPosition = Position { posColumn = 1, posLine = 1 }

type Reprinting m =
 forall b . Typeable b
         => b -> m (Maybe (RefactorType, Source, (Position, Position)))

-- | The reprint algorithm takes a refactoring (parameteric in
-- | some monad m) and turns an arbitrary pretty-printable type 'p'
-- | into a monadic Source transformer.
reprint :: (Monad m, Data p)
        => Reprinting m -> p -> Source -> m Source
reprint reprinting tree input
  -- If the inupt is null then null is returned
  | B.null input = return B.empty

  -- Otherwise go with the normal algorithm
  | otherwise = do
   -- Initial state comprises start cursor and input source
   let state0 = (initPosition, input)
   -- Enter the top-node of a zipper for 'tree'
   (out, (_, remaining)) <- runStateT (enter reprinting (toZipper tree)) state0
   -- Add to the output source the reamining input source
   return $ out `B.append` remaining

-- The enter, enterDown, enterRight each take a refactoring and a
-- zipper producing a stateful Source transformer with Position
-- state.

enter, enterDown, enterRight
  :: Monad m
  => Reprinting m -> Zipper a -> StateT (Position, Source) m Source

-- `enter` applies the generic refactoring to the current context
-- of the zipper
enter reprinting z = do

  -- Step 1.
  -- Apply a refactoring
  refactoringInfo <- lift $ query reprinting z

  -- Step 2.
  output <-
    case refactoringInfo of
      -- No refactoring, so go into the children
      Nothing -> enterDown reprinting z

      -- A refactoring was applied
      Just (typ, output, (lb, ub)) -> do
        (cursor, inp) <- get
        case typ of
          Replace -> do
             -- Get the soure text up to the start of the refactored expr
             let (p0, inp') = splitBySpan (cursor, lb) inp
             -- Cut out the portion of source text consumed by the refactoring
             let (_, inp'') = splitBySpan (lb, ub) inp'
             put (ub, inp'')
             return $ B.concat [p0, output]
          After -> do
             -- Get the soure text up to the end of the refactored expr
             let (p0, inp') = splitBySpan (cursor, ub) inp
             put (ub, inp')
             return $ B.concat [p0, output]
          Before -> do
             -- Get the soure text up to the start of the refactored expr
             let (p0, inp')  = splitBySpan (cursor, lb) inp
             -- Cut out the portion of source text consumed by the refactoring
             let (p1, inp'') = splitBySpan (lb, ub) inp'
             put (ub, inp'')
             return $ B.concat [p0, output, p1]

  -- Part 3.
  -- Enter the right sibling of the current context
  output' <- enterRight reprinting z

  -- Concat the output for the current context, children, and right sibling
  return $ B.concat [output, output']

-- `enterDown` navigates to the children of the current context
enterDown reprinting z =
  case down' z of
    -- Go to children
    Just dz -> enter reprinting dz
    -- No children
    Nothing -> return B.empty

-- `enterRight` navigates to the right sibling of the current context
enterRight reprinting z =
  case right z of
    -- Go to right sibling
    Just rz -> enter reprinting rz
    -- No right sibling
    Nothing -> return B.empty

-- | Given a lower-bound and upper-bound pair of Positions, split the
-- | incoming Source based on the distanceF between the Position pairs
splitBySpan :: (Position, Position) -> Source -> (Source, Source)
splitBySpan (l, u) = subtext (ll, lc) (ll, lc) (ul, uc)
  where (Position lc ll) = l
        (Position uc ul) = u

{-
  Split a text.

  Returns a tuple containing:
    1. the bit of input text between upper and lower bounds
    2. the remaining input text

  Takes:
    1. current cursor position
    2. lower bound
    3. upper bound
    4. input text
-}
subtext :: (Int, Int) -> (Int, Int) -> (Int, Int) -> B.ByteString -> (B.ByteString, B.ByteString)
subtext cursor (lowerLn, lowerCol) (upperLn, upperCol) =
    subtext' B.empty cursor
  where
    subtext' acc (cursorLn, cursorCol) input

      | cursorLn <= lowerLn && (cursorCol >= lowerCol ==> cursorLn < lowerLn) =
        case B.uncons input of
          Nothing -> (B.reverse acc, input)
          Just ('\n', input') -> subtext' acc (cursorLn+1, 1) input'
          Just (_, input')    -> subtext' acc (cursorLn, cursorCol+1) input'

      | cursorLn <= upperLn && (cursorCol >= upperCol ==> cursorLn < upperLn) =
        case B.uncons input of
          Nothing -> (B.reverse acc, input)
          Just ('\n', input') -> subtext' (B.cons '\n' acc) (cursorLn+1, 1) input'
          Just (x, input')    -> subtext' (B.cons x acc) (cursorLn, cursorCol+1) input'

      | otherwise =
        (B.reverse acc, input)

-- Logical implication operator.
(==>) :: Bool -> Bool -> Bool; infix 2 ==>
a ==> b = a <= b

-- | Specify a refactoring type
data RefactorType = Before | After | Replace

-- | Infrastructure for building the reprinter "plugins"
class Refactorable t where
  isRefactored :: t -> Maybe RefactorType
  getSpan      :: t -> (Position, Position)

-- | Essentially wraps the refactorable interface
genReprinting :: (Monad m, Refactorable t, Typeable t)
    => (t -> m Source)
    -> t -> m (Maybe (RefactorType, Source, (Position, Position)))
genReprinting f z = do
  case isRefactored z of
    Nothing -> return Nothing
    Just refactorType -> do
      output <- f z
      return $ Just (refactorType, output, getSpan z)

-- | Catch all generic query
catchAll :: Monad m => a -> m (Maybe b)
catchAll _ = return Nothing
