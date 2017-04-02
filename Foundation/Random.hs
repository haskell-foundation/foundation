-- |
-- Module      : Foundation.Random
-- License     : BSD-style
-- Stability   : experimental
-- Portability : Good
--
-- This module deals with the random subsystem abstractions.
--
-- It provide 2 different set of abstractions:
--
-- * The first abstraction that allow a monad to generate random
--   through the 'MonadRandom' class.
--
-- * The second abstraction to make generic random generator 'RandomGen'
--   and a small State monad like wrapper 'MonadRandomState' to
--   abstract a generator.
--
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables      #-}
module Foundation.Random
    ( MonadRandom(..)
    , MonadRandomState(..)
    , RandomGen(..)
    , getRandomPrimType
    , withRandomGenerator
    , RNG
    , RNGv1
    ) where

import           Foundation.Internal.Base
import           Foundation.Internal.Types
import           Foundation.Internal.Proxy
import           Foundation.Primitive.Monad
import           Foundation.System.Entropy
import           Foundation.Array
import qualified Foundation.Array.Unboxed as A
import qualified Foundation.Array.Unboxed.Mutable as A
import           GHC.ST
import qualified Prelude

-- | A monad constraint that allows to generate random bytes
class (Functor m, Applicative m, Monad m) => MonadRandom m where
    getRandomBytes :: Size Word8 -> m (UArray Word8)

instance MonadRandom IO where
    getRandomBytes = getEntropy

-- | A Deterministic Random Generator (DRG) class
class RandomGen gen where
    -- | Initialize a new random generator
    randomNew :: MonadRandom m => m gen

    -- | Initialize a new random generator from a binary seed.
    --
    -- If `Nothing` is returned, then the data is not acceptable
    -- for creating a new random generator.
    randomNewFrom :: UArray Word8 -> Maybe gen

    -- | Generate N bytes of randomness from a DRG
    randomGenerate :: Size Word8 -> gen -> (UArray Word8, gen)

-- | A simple Monad class very similar to a State Monad
-- with the state being a RandomGenerator.
newtype MonadRandomState gen a = MonadRandomState { runRandomState :: gen -> (a, gen) }

instance Functor (MonadRandomState gen) where
    fmap f m = MonadRandomState $ \g1 ->
        let (a, g2) = runRandomState m g1 in (f a, g2)

instance Applicative (MonadRandomState gen) where
    pure a     = MonadRandomState $ \g -> (a, g)
    (<*>) fm m = MonadRandomState $ \g1 ->
        let (f, g2) = runRandomState fm g1
            (a, g3) = runRandomState m g2
         in (f a, g3)

instance Monad (MonadRandomState gen) where
    return a    = MonadRandomState $ \g -> (a, g)
    (>>=) m1 m2 = MonadRandomState $ \g1 ->
        let (a, g2) = runRandomState m1 g1
         in runRandomState (m2 a) g2

instance RandomGen gen => MonadRandom (MonadRandomState gen) where
    getRandomBytes n = MonadRandomState (randomGenerate n)

getRandomPrimType :: forall randomly ty . (PrimType ty, MonadRandom randomly) => randomly ty
getRandomPrimType =
    flip A.index 0 . A.unsafeRecast <$> getRandomBytes (A.primSizeInBytes (Proxy :: Proxy ty))

-- | Run a pure computation with a Random Generator in the 'MonadRandomState'
withRandomGenerator :: RandomGen gen
                    => gen
                    -> MonadRandomState gen a
                    -> (a, gen)
withRandomGenerator gen m = runRandomState m gen

-- | An alias to the default choice of deterministic random number generator
--
-- Unless, you want to have the stability of a specific random number generator,
-- e.g. for tests purpose, it's recommended to use this alias so that you would
-- keep up to date with possible bugfixes, or change of algorithms.
type RNG = RNGv1

-- | RNG based on ChaCha core.
--
-- The algorithm is identical to the arc4random found in recent BSDs,
-- namely a ChaCha core provide 64 bytes of random from 32 bytes of
-- key.
newtype RNGv1 = RNGv1 (UArray Word8)

instance RandomGen RNGv1 where
    randomNew = RNGv1 <$> getRandomBytes 32
    randomNewFrom bs
        | A.length bs == 32 = Just $ RNGv1 bs
        | otherwise         = Nothing
    randomGenerate = rngv1Generate

rngv1KeySize :: Size Word8
rngv1KeySize = 32

rngv1Generate :: Size Word8 -> RNGv1 -> (UArray Word8, RNGv1)
rngv1Generate n@(Size x) (RNGv1 key) = runST $ do
    dst    <- A.newPinned n
    newKey <- A.newPinned rngv1KeySize
    A.withMutablePtr dst        $ \dstP    ->
        A.withMutablePtr newKey $ \newKeyP ->
        A.withPtr key           $ \keyP    -> do
            _ <- unsafePrimFromIO $ c_rngv1_generate newKeyP dstP keyP (Prelude.fromIntegral x)
            return ()
    (,) <$> A.unsafeFreeze dst
        <*> (RNGv1 <$> A.unsafeFreeze newKey)

-- return 0 on success, !0 for failure
foreign import ccall unsafe "foundation_rngV1_generate"
   c_rngv1_generate :: Ptr Word8 -- new key
                    -> Ptr Word8 -- destination
                    -> Ptr Word8 -- current key
                    -> Word32    -- number of bytes to generate
                    -> IO Word32
