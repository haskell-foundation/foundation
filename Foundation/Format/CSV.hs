-- |
-- Module      : Foundation.Format.CSV
-- License     : BSD-style
-- Maintainer  : Foundation
-- Stability   : experimental
-- Portability : portable
--
-- Provies the support for Comma Separated Value

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}

module Foundation.Format.CSV
    (-- * CSV
      CSV

    -- ** Builder
    -- ** String Bulider
    , csvStringBuilder
    , rowStringBuilder
    , fieldStringBuilder
    -- ** Block Builder
    , csvBlockBuilder
    , rowBlockBuilder
    , fieldBlockBuilder
    -- ** Conduit
    , rowC
    -- * Row
    , Row
    , ToRow(..)
    -- * Field
    , Field(..)
    , Escaping(..)
    , ToField(..)
    -- ** helpers
    , integral
    , float
    , string
    ) where

import           Basement.Imports -- hiding (throw)
import           Basement.BoxedArray              (Array)
import           Basement.NormalForm              (NormalForm(..))
import           Basement.From                    (Into, into)
import           Basement.String                  (String, replace, any, elem)
import qualified Basement.String        as String (singleton)
import           Basement.Types.Word128           (Word128)
import           Basement.Types.Word256           (Word256)
import           Basement.Types.OffsetSize        (Offset, CountOf)
import           Foundation.Collection.Element    (Element)
import           Foundation.Collection.Collection (Collection, nonEmpty_)
import           Foundation.Collection.Sequential (Sequential(intersperse))
import           Foundation.Collection.Indexed    (IndexedCollection)
import           Foundation.Check.Arbitrary       (Arbitrary(..), frequency)
import           Foundation.Conduit.Internal

import qualified Foundation.String.Builder as String
import           Basement.Block              (Block)
import qualified Basement.Block.Builder    as Block

import           GHC.ST (runST)

-- | CSV field
data Field
    = FieldInteger Integer
    | FieldDouble  Double
    | FieldString  String  Escaping
  deriving (Eq, Show, Typeable)
instance NormalForm Field where
    toNormalForm (FieldInteger i) = toNormalForm i
    toNormalForm (FieldDouble  d) = toNormalForm d
    toNormalForm (FieldString  s e) = toNormalForm s `seq` toNormalForm e
instance Arbitrary Field where
    arbitrary = frequency $ nonEmpty_ [ (1, FieldInteger <$> arbitrary)
                                      , (1, FieldDouble <$> arbitrary)
                                      , (3, string <$> arbitrary)
                                      ]

data Escaping = NoEscape | Escape | DoubleEscape
  deriving (Eq, Ord, Enum, Bounded, Show, Typeable)
instance NormalForm Escaping where
    toNormalForm !_ = ()

class ToField a where
    toField :: a -> Field
instance ToField Field where
    toField = id
instance ToField a => ToField (Maybe a) where
    toField Nothing  = FieldString mempty NoEscape
    toField (Just a) = toField a

instance ToField Int8 where
    toField = FieldInteger . into
instance ToField Int16 where
    toField = FieldInteger . into
instance ToField Int32 where
    toField = FieldInteger . into
instance ToField Int64 where
    toField = FieldInteger . into
instance ToField Int where
    toField = FieldInteger . into

instance ToField Word8 where
    toField = FieldInteger . into
instance ToField Word16 where
    toField = FieldInteger . into
instance ToField Word32 where
    toField = FieldInteger . into
instance ToField Word64 where
    toField = FieldInteger . into
instance ToField Word where
    toField = FieldInteger . into
instance ToField Word128 where
    toField = FieldInteger . into
instance ToField Word256 where
    toField = FieldInteger . into

instance ToField Integer where
    toField = FieldInteger
instance ToField Natural where
    toField = FieldInteger . into

instance ToField Double where
    toField = FieldDouble

instance ToField Char where
    toField = string . String.singleton

instance ToField (Offset a) where
    toField = FieldInteger . into
instance ToField (CountOf a) where
    toField = FieldInteger . into

instance ToField [Char] where
    toField = string . fromString
instance ToField String where
    toField = string

-- | helper function to create a `FieldInteger`
--
integral :: Into Integer a => a -> Field
integral = FieldInteger . into

float :: Double -> Field
float = FieldDouble

-- | heler function to create a FieldString.
--
-- This function will findout automatically if an escaping is needed.
-- if you wish to perform the escaping manually, do not used this function
--
string :: String -> Field
string s = FieldString s encoding
  where
    encoding
        | any g s   = DoubleEscape
        | any f s   = Escape
        | otherwise = NoEscape
    f c = c == '\"'
    g c = c `elem` ",\r\n"

-- | CSV Row
--
newtype Row = Row { unRow :: Array Field }
  deriving (Eq, Show, Typeable, Semigroup, Monoid, Collection, NormalForm, Sequential, IndexedCollection)

type instance Element Row = Field
instance IsList Row where
    type Item Row = Field
    toList = toList . unRow
    fromList = Row . fromList

class ToRow a where
    toRow :: a -> Row
instance ToRow Row where
    toRow = id
instance (ToField a, ToField b) => ToRow (a,b) where
    toRow (a,b) = fromList [toField a, toField b]
instance (ToField a, ToField b, ToField c) => ToRow (a,b,c) where
    toRow (a,b,c) = fromList [toField a, toField b, toField c]
instance (ToField a, ToField b, ToField c, ToField d) => ToRow (a,b,c,d) where
    toRow (a,b,c,d) = fromList [toField a, toField b, toField c, toField d]
instance (ToField a, ToField b, ToField c, ToField d, ToField e) => ToRow (a,b,c,d,e) where
    toRow (a,b,c,d,e) = fromList [toField a, toField b, toField c, toField d, toField e]
instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f) => ToRow (a,b,c,d,e,f) where
    toRow (a,b,c,d,e,f) = fromList [toField a, toField b, toField c, toField d, toField e, toField f]

-- | CSV Type
newtype CSV = CSV { unCSV :: Array Row }
  deriving (Eq, Show, Typeable, Semigroup, Monoid, Collection, NormalForm, Sequential, IndexedCollection)

type instance Element CSV = Row

instance IsList CSV where
    type Item CSV = Row
    toList = toList . unCSV
    fromList = CSV . fromList

-- | serialise the CSV document into a UTF8 string
csvStringBuilder :: CSV -> String.Builder
csvStringBuilder = String.unsafeStringBuilder . csvBlockBuilder

rowStringBuilder :: Row -> String.Builder
rowStringBuilder = String.unsafeStringBuilder . rowBlockBuilder

fieldStringBuilder :: Field -> String.Builder
fieldStringBuilder = String.unsafeStringBuilder . fieldBlockBuilder

-- | serialise the CSV document into a UTF8 encoded (Block Word8)
csvBlockBuilder :: CSV -> Block.Builder
csvBlockBuilder = mconcat . intersperse (Block.emitString "\r\n") . fmap rowBlockBuilder . toList . unCSV

rowBlockBuilder :: Row -> Block.Builder
rowBlockBuilder = mconcat . intersperse (Block.emitUTF8Char ',') . fmap fieldBlockBuilder . toList . unRow

fieldBlockBuilder :: Field -> Block.Builder
fieldBlockBuilder (FieldInteger i) = Block.emitString $ show i
fieldBlockBuilder (FieldDouble  d) = Block.emitString $ show d
fieldBlockBuilder (FieldString  s e) = case e of
    NoEscape     -> Block.emitString s
    Escape       -> Block.emitUTF8Char '"' <> Block.emitString s <> Block.emitUTF8Char '"'
    DoubleEscape -> Block.emitUTF8Char '"' <> Block.emitString (replace "\"" "\"\"" s) <> Block.emitUTF8Char '"'

rowC :: (ToRow row, Monad m) => Conduit row (Block Word8) m ()
rowC = await >>= go
  where
    go Nothing  = pure ()
    go (Just r) =
      let bytes = runST (Block.run $ rowBlockBuilder (toRow r) <> Block.emitString "\r\n")
         in yield bytes >> await >>= go
