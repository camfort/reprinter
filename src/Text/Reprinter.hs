{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Text.Reprinter
  (
  -- * The
    reprint
  , splitBySpan
  , mkLine
  , mkCol
  , Source
  , Position
  , Span
  , Reprinting
  , initPosition
  , advanceLine
  , advanceCol
  , catchAll
  , genReprinting
  , Refactorable(..)
  , RefactorType(..)
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import qualified Data.Text.Lazy as Text
import Data.Data
import Data.Generics.Zipper
import Data.Monoid ((<>), mempty)


type Source = Text.Text

newtype Line = Line Int deriving (Data, Eq, Ord, Show)

initLine :: Line
initLine = Line 1

mkLine :: Int -> Line
mkLine l
  | l < 1 = error $ "mkLine: called with: " <> show l <> ". Minimum is 1."
  | otherwise = Line  l

newtype Col = Col Int deriving (Data, Eq, Ord, Show)

initCol :: Col
initCol = Col 1

mkCol :: Int -> Col
mkCol l
  | l < 1 = error $ "mkCol: called with: " <> show l <> ". Minimum is 1."
  | otherwise = Col  l

type Position = (Line,Col)

initPosition :: Position
initPosition = (initLine,initCol)

advanceLine :: Position -> Position
advanceLine (Line x, _) = (Line (x+1), initCol)

advanceCol :: Position -> Position
advanceCol (ln, Col x) = (ln, Col (x+1))


type Span = (Position, Position)

type Reprinting m = forall node . Typeable node => node -> m (Maybe (RefactorType, Source, Span))

-- | Specify a refactoring type
data RefactorType = Before | After | Replace

-- | The reprint algorithm takes a refactoring (parameteric in
-- | some monad m) and turns an arbitrary pretty-printable type 'ast'
-- | into a monadic Source transformer.
reprint :: (Monad m, Data ast) => Reprinting m -> ast -> Source -> m Source
reprint reprinting ast input
  | Text.null input = return mempty
  | otherwise = do
    -- Initial state comprises start cursor and input source
    let state0 = (initPosition, input)
    -- Enter the top-node of a zipper for 'tree'
    (out, (_, remaining)) <- runStateT (enter reprinting (toZipper ast)) state0
    -- Add to the output source the reamining input source
    return (out <> remaining)

-- | Take a refactoring and a zipper producing a stateful Source transformer with Position state.
enter :: Monad m => Reprinting m -> Zipper ast -> StateT (Position, Source) m Source
enter reprinting zipper = do
    -- Step 1: Apply a refactoring
    refactoringInfo <- lift (query reprinting zipper)

    -- Step 2: Deal with refactored code or go to children
    output <- case refactoringInfo of
      -- No refactoring; go to children
      Nothing -> go down'
      -- A refactoring was applied
      Just (typ, output, (lb, ub)) -> do
        (cursor, inp) <- get
        case typ of
          Replace -> do
            -- Get soure up to start of refactored node
            let (pre, inp') = splitBySpan (cursor, lb) inp
            -- Remove source covered by refactoring
            let (_, inp'') = splitBySpan (lb, ub) inp'
            put (ub, inp'')
            return (pre <> output)
          After -> do
            -- Get source up to end of the refactored node
            let (pre, inp') = splitBySpan (cursor, ub) inp
            put (ub, inp')
            return (pre <> output)
          Before -> do
            -- Get source up to start of refactored node
            let (pre, inp') = splitBySpan (cursor, lb) inp
            -- Discard portion consumed by the refactoring
            let (post, inp'') = splitBySpan (lb, ub) inp'
            put (ub, inp'')
            return (pre <> output <> post)

    -- Step 3: Enter the right sibling of the current context
    outputSib <- go right

    -- Finally append output of current context/children
    -- and right sibling
    return (output <> outputSib)

  where
    go direction =
        case direction zipper of
          -- Go to next node if there is one
          Just zipper -> enter reprinting zipper
          -- Otherwise return the empty string
          Nothing -> return mempty


-- | Given a lower-bound and upper-bound pair of Positions, split the
-- | incoming Source based on the distance between the Position pairs
splitBySpan :: Span -> Source -> (Source, Source)
splitBySpan ((lowerLn, lowerCol), (upperLn, upperCol)) =
    subtext mempty (lowerLn, lowerCol)
  where
    subtext acc cursor@(cursorLn, cursorCol) input
      | cursorLn <= lowerLn && (cursorCol >= lowerCol ==> cursorLn < lowerLn) =
          case Text.uncons input of
            Nothing -> done
            Just ('\n', input') -> subtext acc (advanceLine cursor) input'
            Just (_, input')    -> subtext acc (advanceCol cursor) input'
      | cursorLn <= upperLn && (cursorCol >= upperCol ==> cursorLn < upperLn) =
          case Text.uncons input of
            Nothing -> done
            Just ('\n', input') -> subtext (Text.cons '\n' acc) (advanceLine cursor) input'
            Just (x, input')    -> subtext (Text.cons x acc) (advanceCol cursor) input'
      | otherwise = done
      where done = (Text.reverse acc, input)


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
              => (t -> m Source) -> t -> m (Maybe (RefactorType, Source, Span))
genReprinting f z = case isRefactored z of
    Nothing -> return Nothing
    Just refactorType -> do
      output <- f z
      return $ Just (refactorType, output, getSpan z)

-- | Catch all generic query
catchAll :: Monad m => a -> m (Maybe b)
catchAll _ = return Nothing
