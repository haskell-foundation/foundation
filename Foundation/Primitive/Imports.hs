-- |
-- Module      : Foundation.Primitive.Imports
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- re-export of all the base prelude and basic primitive stuffs
module Foundation.Primitive.Imports
    ( (Prelude.$)
    , (Prelude.$!)
    , (Prelude.&&)
    , (Prelude.||)
    , (Control.Category..)
    , (Control.Applicative.<$>)
    , Prelude.not
    , Prelude.otherwise
    , Prelude.fst
    , Prelude.snd
    , Control.Category.id
    , Prelude.maybe
    , Prelude.either
    , Prelude.flip
    , Prelude.const
    , Foundation.Primitive.Error.error
    , Prelude.and
    , Prelude.undefined
    , Prelude.seq
    , Prelude.Show
    , Foundation.Primitive.Show.show
    , Prelude.Ord (..)
    , Prelude.Eq (..)
    , Prelude.Bounded (..)
    , Prelude.Enum (..)
    , Prelude.Functor (..)
    , Control.Applicative.Applicative (..)
    , Prelude.Monad (..)
    , Prelude.Maybe (..)
    , Prelude.Ordering (..)
    , Prelude.Bool (..)
    , Prelude.Int
    , Prelude.Integer
    , Foundation.Internal.Natural.Natural
    , Foundation.Primitive.Types.OffsetSize.Offset
    , Foundation.Primitive.Types.OffsetSize.CountOf
    , Prelude.Char
    , Foundation.Primitive.Types.Char7.Char7
    , Foundation.Primitive.Types.AsciiString.AsciiString
    , Foundation.Primitive.UTF8.Base.String
    , Foundation.Array.Unboxed.UArray
    , Foundation.Array.Boxed.Array
    , Foundation.Internal.NumLiteral.Integral (..)
    , Foundation.Internal.NumLiteral.Fractional (..)
    , Foundation.Internal.NumLiteral.HasNegation (..)
    , Data.Int.Int8, Data.Int.Int16, Data.Int.Int32, Data.Int.Int64
    , Data.Word.Word8, Data.Word.Word16, Data.Word.Word32, Data.Word.Word64, Data.Word.Word
    , Prelude.Double, Prelude.Float
    , Prelude.IO
    , FP32
    , FP64
    , Foundation.Internal.IsList.IsList (..)
    , GHC.Exts.IsString (..)
    , GHC.Generics.Generic (..)
    , Prelude.Either (..)
    , Data.Data.Data (..)
    , Data.Data.mkNoRepType
    , Data.Data.DataType
    , Data.Typeable.Typeable
    , Data.Monoid.Monoid (..)
    , (Data.Monoid.<>)
    , Control.Exception.Exception
    , Control.Exception.throw
    , Control.Exception.throwIO
    , GHC.Ptr.Ptr(..)
    , ifThenElse
    ) where

import qualified Prelude
import qualified Control.Category
import qualified Control.Applicative
import qualified Control.Exception
import qualified Data.Monoid
import qualified Data.Data
import qualified Data.Typeable
import qualified Data.Word
import qualified Data.Int
import qualified Foundation.Internal.IsList
import qualified Foundation.Internal.Natural
import qualified Foundation.Internal.NumLiteral
import qualified Foundation.Array.Unboxed
import qualified Foundation.Array.Boxed
import qualified Foundation.Primitive.UTF8.Base
import qualified Foundation.Primitive.Error
import qualified Foundation.Primitive.Show
import qualified Foundation.Primitive.Types.OffsetSize
import qualified Foundation.Primitive.Types.AsciiString
import qualified Foundation.Primitive.Types.Char7
import qualified GHC.Exts
import qualified GHC.Generics
import qualified GHC.Ptr
import           GHC.Exts (fromString)

-- | for support of if .. then .. else
ifThenElse :: Prelude.Bool -> a -> a -> a
ifThenElse Prelude.True  e1 _  = e1
ifThenElse Prelude.False _  e2 = e2

type FP32 = Prelude.Float
type FP64 = Prelude.Double
