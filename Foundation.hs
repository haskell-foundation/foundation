-- |
-- Module      : Foundation
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- I tried to picture clusters of information
-- As they moved through the computer
-- What do they look like?
--
-- Alternative Prelude
module Foundation
    ( -- * Standard
      -- ** Operators
      (Prelude.$)
    , (Prelude.$!)
    , (Prelude.&&)
    , (Prelude.||)
    , (Control.Category..)
      -- ** Functions
    , Prelude.not
    , Prelude.otherwise
    , module Foundation.Tuple
    , Control.Category.id
    , Prelude.maybe
    , Prelude.either
    , Prelude.flip
    , Prelude.const
    , Prelude.error
    , Foundation.IO.Terminal.putStr
    , Foundation.IO.Terminal.putStrLn
    --, print
    , getArgs
    , Prelude.uncurry
    , Prelude.curry
    , Data.Tuple.swap
    , Prelude.until
    , Prelude.asTypeOf
    , Prelude.undefined
    , Prelude.seq
      -- ** Type classes
    , Prelude.Show (..)
    , Prelude.Ord (..)
    , Prelude.Eq (..)
    , Prelude.Bounded (..)
    , Prelude.Enum (..)
    , Prelude.Functor (..)
    , Control.Applicative.Applicative (..)
    , Prelude.Monad (..)
    , (Control.Monad.=<<)
    --, Foundation.String.IsString (..)
    , IsString(..)
    , IsList(..)
      -- ** Numeric type classes
    , Foundation.Number.Number (..)
    , Foundation.Number.Signed (..)
    , Foundation.Number.Additive (..)
    , Foundation.Number.Subtractive (..)
    , Foundation.Number.Multiplicative (..)
      -- ** Data types
    , Prelude.Maybe (..)
    , Prelude.Ordering (..)
    , Prelude.Bool (..)
    , Prelude.Char
    , Prelude.IO
    , Prelude.Either (..)
      -- ** Numbers
    , Data.Int.Int8, Data.Int.Int16, Data.Int.Int32, Data.Int.Int64
    , Data.Word.Word8, Data.Word.Word16, Data.Word.Word32, Data.Word.Word64, Data.Word.Word
    , Prelude.Int
    , Prelude.Integer
    , Prelude.Rational
    , Prelude.Float
    , Prelude.Double
      -- ** Collection types
    , UArray
    , PrimType
    , Array
    , String
      -- ** Numeric functions
    -- , (Prelude.^)
    , (Prelude.^^)
    , Prelude.fromIntegral
    , Prelude.realToFrac
      -- ** Monoids
    , Monoid (..)
    , (<>)
      -- ** Folds and traversals
    , Data.Foldable.Foldable
    , Data.Foldable.asum
    , Data.Traversable.Traversable
      -- ** Maybe
    , Data.Maybe.mapMaybe
    , Data.Maybe.catMaybes
    , Data.Maybe.fromMaybe
    , Data.Maybe.isJust
    , Data.Maybe.isNothing
    , Data.Maybe.listToMaybe
    , Data.Maybe.maybeToList
      -- ** Either
    , Data.Either.partitionEithers
    , Data.Either.lefts
    , Data.Either.rights
      -- ** Function
    , Data.Function.on
      -- ** Applicative
    , (Control.Applicative.<$>)
    , (Control.Applicative.<|>)
      -- ** Monad
    , (Control.Monad.>=>)
      -- ** Exceptions
    , Control.Exception.Exception (..)
    , Data.Typeable.Typeable
    , Control.Exception.SomeException
    , Control.Exception.IOException
      -- ** Proxy
    , Foundation.Internal.Proxy.Proxy(..)
    , Foundation.Internal.Proxy.asProxyTypeOf
      -- ** Partial
    , Foundation.Partial.Partial
    , Foundation.Partial.partial
    , Foundation.Partial.PartialError
    , Foundation.Partial.fromPartial
      -- ** Old Prelude Strings as [Char] with bridge back and forth
    , LString
    ) where

import qualified Prelude
--import           Prelude (Char, (.), Eq, Bool, IO)

import           Data.Monoid (Monoid (..))
import           Control.Applicative
import qualified Control.Category
import qualified Control.Monad
import qualified Control.Exception
import qualified Data.Typeable

import qualified Data.Foldable
import qualified Data.Traversable

import           Data.Word (Word8, Word16, Word32, Word64, Word)
import           Data.Int (Int8, Int16, Int32, Int64)
import           Foundation.String (String)
import           Foundation.Array (UArray, Array, PrimType)
--import           Foundation.Collection
import qualified Foundation.IO.Terminal

import           GHC.Exts (IsString(..))
import           Foundation.Internal.IsList
import qualified Foundation.Internal.Proxy

import qualified Foundation.Number
import qualified Foundation.Partial
import           Foundation.Tuple

import qualified Data.Maybe
import qualified Data.Either
import qualified Data.Function
import qualified Data.Tuple

import qualified System.Environment
import qualified Data.List

import           Data.Monoid ((<>))

-- | Alias to Prelude String ([Char]) for compatibility purpose
type LString = Prelude.String

-- | Returns a list of the program's command line arguments (not including the program name).
getArgs :: Prelude.IO [String]
getArgs = (Data.List.map fromList <$> System.Environment.getArgs)
