{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE CPP                        #-}
module Basement.Alg.Foreign.String
    ( copyFilter
    , validate
    , findIndexPredicate
    , revFindIndexPredicate
    ) where

import           GHC.Prim
import           GHC.ST
import           Basement.Compat.Base
import           Basement.Numerical.Additive
import           Basement.Types.OffsetSize

import qualified Basement.Alg.Native.Prim as PrimNative -- NO SUBST
import qualified Basement.Alg.Native.UTF8 as UTF8Native -- NO SUBST
import qualified Basement.Alg.Foreign.Prim as PrimBackend
import qualified Basement.Alg.Foreign.UTF8 as PrimBackend
import           Basement.UTF8.Helper
import           Basement.UTF8.Table
import           Basement.UTF8.Types

copyFilter :: (Char -> Bool)
           -> CountOf Word8
           -> MutableByteArray# s
           -> PrimBackend.Immutable
           -> Offset Word8
           -> ST s (CountOf Word8)
copyFilter predicate !sz dst src start = loop (Offset 0) start
  where
    !end = start `offsetPlusE` sz
    loop !d !s
        | s == end  = pure (offsetAsSize d)
        | otherwise =
            let !h = PrimBackend.primIndex src s
             in case headerIsAscii h of
                    True | predicate (toChar1 h) -> PrimNative.primWrite dst d h >> loop (d + Offset 1) (s + Offset 1)
                         | otherwise             -> loop d (s + Offset 1)
                    False ->
                        case PrimBackend.next src s of
                            Step c s' | predicate c -> UTF8Native.write dst d c >>= \d' -> loop d' s'
                                      | otherwise   -> loop d s'

validate :: Offset Word8
         -> PrimBackend.Immutable
         -> Offset Word8
         -> (Offset Word8, Maybe ValidationFailure)
validate end ba ofsStart = loop ofsStart
  where
    loop !ofs
        | ofs > end  = error ("validate: internal error: went pass offset : ofs=" <> show ofs <> " end=" <> show end)
        | ofs == end = (end, Nothing)
        | otherwise  =
            let !h = PrimBackend.primIndex ba ofs in
            case headerIsAscii h of
                True  -> loop (ofs + Offset 1)
                False ->
                    case one (CountOf $ getNbBytes h) ofs of
                        (nextOfs, Nothing)  -> loop nextOfs
                        (pos, Just failure) -> (pos, Just failure)

    one (CountOf 0xff) pos = (pos, Just InvalidHeader)
    one nbConts pos
        | ((pos+Offset 1) `offsetPlusE` nbConts) > end = (pos, Just MissingByte)
        | otherwise =
            case nbConts of
                CountOf 1 ->
                    let c1 = PrimBackend.primIndex ba (pos + Offset 1)
                    in if isContinuation c1
                        then (pos + Offset 2, Nothing)
                        else (pos, Just InvalidContinuation)
                CountOf 2 ->
                    let c1 = PrimBackend.primIndex ba (pos + Offset 1)
                        c2 = PrimBackend.primIndex ba (pos + Offset 2)
                     in if isContinuation c1 && isContinuation c2
                            then (pos + Offset 3, Nothing)
                            else (pos, Just InvalidContinuation)
                CountOf 3 ->
                    let c1 = PrimBackend.primIndex ba (pos + Offset 1)
                        c2 = PrimBackend.primIndex ba (pos + Offset 2)
                        c3 = PrimBackend.primIndex ba (pos + Offset 3)
                     in if isContinuation c1 && isContinuation c2 && isContinuation c3
                            then (pos + Offset 4, Nothing)
                            else (pos, Just InvalidContinuation)
                CountOf _ -> error "internal error"

findIndexPredicate :: (Char -> Bool)
                   -> PrimBackend.Immutable
                   -> Offset Word8
                   -> Offset Word8
                   -> Offset Word8
findIndexPredicate predicate ba !startIndex !endIndex = loop startIndex
  where
    loop !i
        | i < endIndex && not (predicate c) = loop (i')
        | otherwise                         = i
      where
        Step c i' = PrimBackend.next ba i
{-# INLINE findIndexPredicate #-}

revFindIndexPredicate :: (Char -> Bool)
                      -> PrimBackend.Immutable
                      -> Offset Word8
                      -> Offset Word8
                      -> Offset Word8
revFindIndexPredicate predicate ba startIndex endIndex
    | endIndex > startIndex = loop endIndex
    | otherwise             = endIndex
  where
    loop !i
        | predicate c     = i'
        | i' > startIndex = loop i'
        | otherwise       = endIndex
      where 
        StepBack c i' = PrimBackend.prev ba i
{-# INLINE revFindIndexPredicate #-}
