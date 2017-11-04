{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Basement.Terminal
    ( initialize
    , size
    ) where

import Basement.Compat.Base
import Foreign.C
import qualified Prelude as P

foreign import ccall "get_terminal_width" get_terminal_width :: IO CInt
foreign import ccall "get_terminal_height" get_terminal_height :: IO CInt

size :: IO (Int, Int)
size = do
    width <- get_terminal_width
    height <- get_terminal_height
    return (P.fromIntegral width, P.fromIntegral height)

#ifdef mingw32_HOST_OS
import System.IO (hSetEncoding, utf8, hPutStrLn, stderr, stdin, stdout)
import System.Win32.Console (setConsoleCP, setConsoleOutputCP, getConsoleCP, getConsoleOutputCP)
#endif

initialize :: IO ()
initialize = do
#ifdef ming32_HOST_OS
    query getConsoleOutputCP (\e -> setConsoleOutputCP e >> hSetEncoding stdout utf8 >> hSetEncoding stderr utf8) utf8Code
    query getConsoleCP (\e -> setConsoleCP e >> hSetEncoding stdin utf8) utf8Code
  where
    utf8Code = 65001
    query get set expected = do
        v <- get
        if v == expected then pure () else set expected
#else
    pure ()
#endif
