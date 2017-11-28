{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Text.Reprinter
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

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import qualified Data.ByteString.Char8 as B
import Data.Data
import Data.Generics.Zipper
import Data.Monoid ((<>), mempty)

type Source = B.ByteString

newtype Line = Line Int deriving (Data, Enum, Eq, Ord, Show)
initLine = Line 1

newtype Col = Col Int deriving (Data, Enum, Eq, Ord, Show)
initCol = Col 1

data Position = Position Line Col deriving (Data, Show)
initPosition = Position initLine initCol

type Span = (Position, Position)

type Reprinting m = forall node . Typeable node => node -> m (Maybe (RefactorType, Source, Span))

-- | Specify a refactoring type
data RefactorType = Before | After | Replace

-- | The reprint algorithm takes a refactoring (parameteric in
-- | some monad m) and turns an arbitrary pretty-printable type 'ast'
-- | into a monadic Source transformer.
reprint :: (Monad m, Data ast) => Reprinting m -> ast -> Source -> m Source
reprint reprinting ast input
  | B.null input = return mempty
  | otherwise = do
    -- Initial state comprises start cursor and input source
    let state0 = (initPosition, input)
    -- Enter the top-node of a zipper for 'tree'
    (out, (_, remaining)) <- runStateT (enter reprinting (toZipper ast)) state0
    -- Add to the output source the reamining input source
    return (out <> remaining)

-- | Take a refactoring and a zipper producing a stateful Source transformer with Position state.
enter :: Monad m => Reprinting m -> Zipper a -> StateT (Position, Source) m Source
enter reprinting zipper = do
    -- Step 1.
    -- Apply a refactoring
    refactoringInfo <- lift (query reprinting zipper)

    -- Step 2.
    output <- case refactoringInfo of
      -- No refactoring, so go into the children
      Nothing -> go down'
      -- A refactoring was applied
      Just (typ, output, (lb, ub)) -> do
        (cursor, inp) <- get
        case typ of
          Replace -> do
            -- Get the soure text up to the start of the refactored expr
            let (pre, inp') = splitBySpan (cursor, lb) inp
            -- Discard the portion of source text consumed by the refactoring
            let (_, inp'') = splitBySpan (lb, ub) inp'
            put (ub, inp'')
            return (pre <> output)
          After -> do
            -- Get the soure text up to the end of the refactored expr
            let (pre, inp') = splitBySpan (cursor, ub) inp
            put (ub, inp')
            return (pre <> output)
          Before -> do
            -- Get the soure text up to the start of the refactored expr
            let (pre, inp') = splitBySpan (cursor, lb) inp
            -- Cut out the portion of source text consumed by the refactoring
            let (post, inp'') = splitBySpan (lb, ub) inp'
            put (ub, inp'')
            return (pre <> output <> post)

    -- Part 3.
    -- Enter the right sibling of the current context
    outputSib <- go right

    -- Concat the output for the current context, children, and right sibling
    return (output <> outputSib)
  where
    go direction =
      case direction zipper of
        -- Go to next node if there is one
        Just zipper -> enter reprinting zipper
        -- Otherwise return empty string
        Nothing -> return mempty


-- | Given a lower-bound and upper-bound pair of Positions, split the
-- | incoming Source based on the distance between the Position pairs
splitBySpan :: Span -> Source -> (Source, Source)
splitBySpan (Position lowerLn lowerCol, Position upperLn upperCol) =
    subtext mempty lowerLn lowerCol
  where
    subtext acc cursorLn cursorCol input
      | cursorLn <= lowerLn && (cursorCol >= lowerCol ==> cursorLn < lowerLn) =
          case B.uncons input of
            Nothing -> done
            Just ('\n', input') -> subtext acc (succ cursorLn) initCol input'
            Just (_, input')    -> subtext acc cursorLn (succ cursorCol) input'
      | cursorLn <= upperLn && (cursorCol >= upperCol ==> cursorLn < upperLn) =
          case B.uncons input of
            Nothing -> done
            Just ('\n', input') -> subtext (B.cons '\n' acc) (succ cursorLn) initCol input'
            Just (x, input')    -> subtext (B.cons x acc) cursorLn (succ cursorCol) input'
      | otherwise = done
      where done = (B.reverse acc, input)

-- Logical implication operator.
(==>) :: Bool -> Bool -> Bool
True ==> False = False
_    ==> _     = True
infix 2 ==>


-- | Infrastructure for building the reprinter "plugins"
class Refactorable t where
  isRefactored :: t -> Maybe RefactorType
  getSpan      :: t -> Span

-- | Essentially wraps the refactorable interface
genReprinting :: (Monad m, Refactorable t, Typeable t)
    => (t -> m Source)
    -> t -> m (Maybe (RefactorType, Source, Span))
genReprinting f z = do
  case isRefactored z of
    Nothing -> return Nothing
    Just refactorType -> do
      output <- f z
      return $ Just (refactorType, output, getSpan z)

-- | Catch all generic query
catchAll :: Monad m => a -> m (Maybe b)
catchAll _ = return Nothing
