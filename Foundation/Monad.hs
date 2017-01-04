module Foundation.Monad
    ( MonadIO(..)
    , MonadFailure(..)
    , MonadThrow(..)
    , MonadCatch(..)
    , MonadTrans(..)
    ) where

import Foundation.Monad.MonadIO
import Foundation.Monad.Exception
import Foundation.Monad.Transformer
