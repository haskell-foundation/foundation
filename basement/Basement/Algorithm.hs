{-# LANGUAGE MultiParamTypeClasses #-}
module Basement.Algorithm
    ( RandomAccess, read, write
    , inplaceSortBy
    ) where

import           GHC.Types
import           GHC.Prim
import           Basement.Compat.Base
import           Basement.Numerical.Additive
import           Basement.Numerical.Multiplicative
import           Basement.Types.OffsetSize
import           Basement.PrimType
import           Basement.Monad

class RandomAccess a ty where
    read  :: PrimMonad prim => a ty (PrimState prim) -> (Offset ty)       -> prim ty
    write :: PrimMonad prim => a ty (PrimState prim) -> (Offset ty) -> ty -> prim ()

inplaceSortBy :: (PrimMonad prim, RandomAccess a ty) 
              => (ty -> ty -> Ordering)
              -- ^ Function defining the ordering relationship
              -> (Offset ty) -- ^ Offset to first element to sort
              -> (CountOf ty) -- ^ Number of elements to sort
              -> a ty (PrimState prim) -- ^ Data to be sorted
              -> prim ()
inplaceSortBy ford start len mvec
    = qsort start (start `offsetPlusE` len `offsetSub` 1)
    where
        qsort lo hi
            | lo >= hi  = pure ()
            | otherwise = do
                p <- partition lo hi
                qsort lo (pred p)
                qsort (p+1) hi
        pivotStrategy (Offset low) hi@(Offset high) = do
            let mid = Offset $ (low + high) `div` 2
            pivot <- read mvec mid
            read mvec hi >>= write mvec mid
            write mvec hi pivot -- move pivot @ pivotpos := hi
            pure pivot
        partition lo hi = do
            pivot <- pivotStrategy lo hi
            -- RETURN: index of pivot with [<pivot | pivot | >=pivot]
            -- INVARIANT: i & j are valid array indices; pivotpos==hi
            let go i j = do
                    -- INVARIANT: k <= pivotpos
                    let fw k = do ak <- read mvec k
                                  if ford ak pivot == LT 
                                    then fw (k+1)
                                    else pure (k, ak)
                    (i, ai) <- fw i -- POST: ai >= pivot
                    -- INVARIANT: k >= i
                    let bw k | k==i = pure (i, ai)
                             | otherwise = do ak <- read mvec k
                                              if ford ak pivot /= LT
                                                then bw (pred k)
                                                else pure (k, ak)
                    (j, aj) <- bw j -- POST: i==j OR (aj<pivot AND j<pivotpos)
                    -- POST: ai>=pivot AND (i==j OR aj<pivot AND (j<pivotpos))
                    if i < j
                        then do -- (ai>=p AND aj<p) AND (i<j<pivotpos)
                            -- swap two non-pivot elements and proceed
                            write mvec i aj
                            write mvec j ai
                            -- POST: (ai < pivot <= aj)
                            go (i+1) (pred j)
                        else do -- ai >= pivot 
                            -- complete partitioning by swapping pivot to the center
                            write mvec hi ai 
                            write mvec i pivot
                            pure i
            go lo hi
{-# INLINE inplaceSortBy #-}