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
    , Foundation.Primitive.Error.error
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
    , Foundation.Primitive.NormalForm
    , Foundation.Primitive.deepseq
    , Foundation.Primitive.force
      -- ** Type classes
    , Prelude.Show
    , show
    , Prelude.Ord (..)
    , Prelude.Eq (..)
    , Prelude.Bounded (..)
    , Prelude.Enum (..)
    , Prelude.Functor (..)
    , Integral (..)
    , Fractional (..)
    , HasNegation (..)
    , Foundation.Class.Bifunctor.Bifunctor (..)
    , Control.Applicative.Applicative (..)
    , Prelude.Monad (..)
    , (Control.Monad.=<<)
    --, Foundation.String.IsString (..)
    , IsString(..)
    , IsList(..)
      -- ** Numeric type classes
    , Foundation.Numerical.IsIntegral (..)
    , Foundation.Numerical.IsNatural (..)
    , Foundation.Numerical.Signed (..)
    , Foundation.Numerical.Additive (..)
    , Foundation.Numerical.Subtractive (..)
    , Foundation.Numerical.Multiplicative (..)
    , Foundation.Numerical.IDivisible(..)
    , Foundation.Numerical.Divisible(..)
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
    , Natural
    , Prelude.Rational
    , Prelude.Float
    , Prelude.Double
    , Size(..), Offset(..)
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
      -- ** Collection
    , Collection(..)
    , Sequential(..)
    , NonEmpty
    , nonEmpty
      -- ** Folds
    , Foldable(..)
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
    , Foundation.Internal.Base.ifThenElse
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

import           Data.Word (Word8, Word16, Word32, Word64, Word)
import           Data.Int (Int8, Int16, Int32, Int64)
import           Foundation.String (String)
import           Foundation.Array (UArray, Array, PrimType)
import           Foundation.Collection (Collection(..), Sequential(..), NonEmpty, nonEmpty, Foldable(..))
import qualified Foundation.IO.Terminal

import           GHC.Exts (IsString(..))
import           Foundation.Internal.IsList
import qualified Foundation.Internal.Base (ifThenElse)
import qualified Foundation.Internal.Proxy
import qualified Foundation.Primitive.Error

import qualified Foundation.Numerical
import qualified Foundation.Partial
import           Foundation.Tuple

import qualified Foundation.Class.Bifunctor
import           Foundation.Primitive.Types.OffsetSize (Size(..), Offset(..))
import qualified Foundation.Primitive
import           Foundation.Internal.NumLiteral
import           Foundation.Internal.Natural

import qualified Data.Maybe
import qualified Data.Either
import qualified Data.Function
import qualified Data.Tuple

import qualified System.Environment
import qualified Data.List

import           Data.Monoid ((<>))

default (Prelude.Integer, Prelude.Double)

-- | Alias to Prelude String ([Char]) for compatibility purpose
type LString = Prelude.String

-- | Use the Show class to create a String.
--
-- Note that this is not efficient, since
-- an intermediate [Char] is going to be
-- created before turning into a real String.
show :: Prelude.Show a => a -> String
show = fromList Prelude.. Prelude.show

-- | Returns a list of the program's command line arguments (not including the program name).
getArgs :: Prelude.IO [String]
getArgs = (Data.List.map fromList <$> System.Environment.getArgs)
