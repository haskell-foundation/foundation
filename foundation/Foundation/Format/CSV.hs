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

import Foundation.Format.CSV.Types
import Foundation.Format.CSV.Builder
