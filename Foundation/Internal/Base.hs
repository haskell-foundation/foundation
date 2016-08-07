-- |
-- Module      : Foundation.Internal.Base
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- internal re-export of all the good base bits
module Foundation.Internal.Base
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
    , Prelude.error
    , Prelude.and
    , Prelude.undefined
    , Prelude.seq
    , Prelude.Show (..)
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
    , Prelude.Char
    , Data.Int.Int8, Data.Int.Int16, Data.Int.Int32, Data.Int.Int64
    , Data.Word.Word8, Data.Word.Word16, Data.Word.Word32, Data.Word.Word64, Data.Word.Word
    , Prelude.IO
    , Foundation.Internal.IsList.IsList (..)
    , GHC.Exts.IsString (..)
    , GHC.Generics.Generic (..)
    , Prelude.Either (..)
    , Data.Typeable.Typeable
    , Data.Monoid.Monoid (..)
    , (Data.Monoid.<>)
    , Control.Exception.Exception
    , Control.Exception.throw
    , Control.Exception.throwIO
    -- * Errors
    , internalError
    ) where

import qualified Prelude
import qualified Control.Category
import qualified Control.Applicative
import qualified Control.Exception
import qualified Data.Monoid
import qualified Data.Typeable
import qualified Data.Word
import qualified Data.Int
import qualified Foundation.Internal.IsList
import qualified GHC.Exts
import qualified GHC.Generics

-- | Only to use internally for internal error cases
internalError :: [Prelude.Char] -> a
internalError s = Prelude.error ("Internal Error: the impossible happened: " Prelude.++ s)
