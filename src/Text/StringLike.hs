{-# LANGUAGE TypeFamilies #-}

module Text.StringLike
  ( StringLike(..)
  , IsString(..)
  ) where

import           Data.List   (uncons)
import           Data.String (IsString(..))

import qualified Data.Text                  as TextStrict
import qualified Data.Text.Lazy             as TextLazy
import qualified Data.ByteString.Char8      as BSCStrict
import qualified Data.ByteString.Lazy.Char8 as BSCLazy

-- | Data types that can be used as a list-like structure of 'Char's.
--
-- Clumsy solution to allow parameterising over the input type (Text,
-- ByteString, String), rather than converting to and from an internal concrete
-- type. Only operations required by the reprinting algorithm are included.
-- Where possible, operations are prefilled using presumed-existing instances
-- (any @[Char]@-like should be a monoid and have a @String -> a@).
class (Monoid a, IsString a) => StringLike a where
    slCons :: Char -> a -> a
    slUncons :: a -> Maybe (Char, a)
    slNull :: a -> Bool
    slReverse :: a -> a
    slToString :: a -> String

-- same trick as used in IsString, to avoid possible ambiguity issues
instance (a ~ Char) => StringLike [a] where
    slCons = (:)
    slUncons = uncons
    slNull = null
    slReverse = reverse
    slToString = id

instance StringLike TextStrict.Text where
    slCons = TextStrict.cons
    slUncons = TextStrict.uncons
    slNull = TextStrict.null
    slReverse = TextStrict.reverse
    slToString = TextStrict.unpack

instance StringLike TextLazy.Text where
    slCons = TextLazy.cons
    slUncons = TextLazy.uncons
    slNull = TextLazy.null
    slReverse = TextLazy.reverse
    slToString = TextLazy.unpack

instance StringLike BSCStrict.ByteString where
    slCons = BSCStrict.cons
    slUncons = BSCStrict.uncons
    slNull = BSCStrict.null
    slReverse = BSCStrict.reverse
    slToString = BSCStrict.unpack

instance StringLike BSCLazy.ByteString where
    slCons = BSCLazy.cons
    slUncons = BSCLazy.uncons
    slNull = BSCLazy.null
    slReverse = BSCLazy.reverse
    slToString = BSCLazy.unpack
