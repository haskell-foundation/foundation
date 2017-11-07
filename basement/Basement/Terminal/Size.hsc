module Basement.Terminal.Size 
    ( size
    ) where
        
import Foreign
import Foreign.C
import Prelude
import Debug.Trace

#include "../../cbits/foundation_system.h"
#ifdef FOUNDATION_SYSTEM_WINDOWS
#include <windows.h>
#elif defined FOUNDATION_SYSTEM_UNIX
#include <sys/ioctl.h>
#endif 

#include <stdio.h>

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

#ifdef FOUNDATION_SYSTEM_UNIX
data Winsize = Winsize {
    ws_row :: CUShort,
    ws_col :: CUShort,
    ws_xpixel :: CUShort,
    ws_ypixel :: CUShort
}

instance Storable Winsize where
    sizeOf _ = #{size struct winsize}
    alignment _ = #{alignment struct winsize}
    peek ptr = do
        r <- #{peek struct winsize, ws_row} ptr
        c <- #{peek struct winsize, ws_col} ptr
        x <- #{peek struct winsize, ws_xpixel} ptr
        y <- #{peek struct winsize, ws_ypixel} ptr
        return (Winsize r c x y)
    poke ptr (Winsize r c x y) = do
        #{poke struct winsize, ws_row} ptr r
        #{poke struct winsize, ws_col} ptr c
        #{poke struct winsize, ws_xpixel} ptr x
        #{poke struct winsize, ws_ypixel} ptr y
        
#elif defined FOUNDATION_SYSTEM_WINDOWS
type Handle = Ptr CChar  -- void *

data SmallRect = SmallRect 
    { left :: CShort
    , top :: CShort
    , right :: CShort
    , bottom :: CShort
    } deriving (Show)

instance Storable SmallRect where
    sizeOf _ = #{size SMALL_RECT}
    alignment _ = #{alignment SMALL_RECT}
    peek ptr = do
        l <- #{peek SMALL_RECT, Left} ptr
        r <- #{peek SMALL_RECT, Right} ptr
        t <- #{peek SMALL_RECT, Top} ptr
        b <- #{peek SMALL_RECT, Bottom} ptr
        return (SmallRect l t r b)
    poke ptr (SmallRect l t r b) = do
        #{poke SMALL_RECT, Left} ptr l
        #{poke SMALL_RECT, Top} ptr t
        #{poke SMALL_RECT, Right} ptr r
        #{poke SMALL_RECT, Bottom} ptr b
        
data Coord = Coord 
    { x :: CShort
    , y :: CShort
    } deriving (Show)

instance Storable Coord where
    sizeOf _ = #{size COORD}
    alignment _ = #{alignment COORD}
    peek ptr = do
        x <- #{peek COORD, X} ptr
        y <- #{peek COORD, Y} ptr
        return (Coord x y)
    poke ptr (Coord x y) = do
        #{poke COORD, X} ptr x
        #{poke COORD, Y} ptr y

data ConsoleScreenBufferInfo = ConsoleScreenBufferInfo 
    { dwSize :: Coord
    , dwCursorPosition :: Coord
    , wAttributes :: CUShort
    , srWindow :: SmallRect
    , dwMaximumWindowSize :: Coord
    } deriving (Show)

instance Storable ConsoleScreenBufferInfo where
    sizeOf _ = #{size CONSOLE_SCREEN_BUFFER_INFO}
    alignment _ = #{alignment CONSOLE_SCREEN_BUFFER_INFO}
    peek ptr = do
        s <- #{peek CONSOLE_SCREEN_BUFFER_INFO, dwSize} ptr
        c <- #{peek CONSOLE_SCREEN_BUFFER_INFO, dwCursorPosition} ptr
        a <- #{peek CONSOLE_SCREEN_BUFFER_INFO, wAttributes} ptr
        w <- #{peek CONSOLE_SCREEN_BUFFER_INFO, srWindow} ptr
        m <- #{peek CONSOLE_SCREEN_BUFFER_INFO, dwMaximumWindowSize} ptr
        return (ConsoleScreenBufferInfo s c a w m)
    poke ptr (ConsoleScreenBufferInfo s c a w m) = do
        #{poke CONSOLE_SCREEN_BUFFER_INFO, dwSize} ptr s
        #{poke CONSOLE_SCREEN_BUFFER_INFO, dwCursorPosition} ptr c
        #{poke CONSOLE_SCREEN_BUFFER_INFO, wAttributes} ptr a
        #{poke CONSOLE_SCREEN_BUFFER_INFO, srWindow} ptr w
        #{poke CONSOLE_SCREEN_BUFFER_INFO, dwMaximumWindowSize} ptr m
    
invalidHandleValue :: IntPtr
invalidHandleValue = #{const INVALID_HANDLE_VALUE}

stdOutputHandle :: CULong
stdOutputHandle = #{const STD_OUTPUT_HANDLE}
#endif
-- defined FOUNDATION_SYSTEM_WINDOWS


foreign import ccall "popen" c_popen :: CString -> CString -> IO (Ptr CFile)
foreign import ccall "pclose" c_pclose :: Ptr CFile -> IO CInt
foreign import ccall "fscanf_int" c_fscanf_int :: Ptr CFile -> Ptr CInt -> IO CInt
#ifdef FOUNDATION_SYSTEM_UNIX
foreign import ccall "ioctl_winsize" c_ioctl_winsize :: CInt -> Ptr Winsize -> 
  IO CInt
#elif defined FOUNDATION_SYSTEM_WINDOWS
foreign import ccall "GetStdHandle" c_get_std_handle :: CULong -> IO Handle
foreign import ccall "GetConsoleScreenBufferInfo" c_get_console_screen_buffer_info 
  :: Handle -> Ptr ConsoleScreenBufferInfo -> IO CInt
#endif

#ifdef FOUNDATION_SYSTEM_UNIX
ioctlWinsize :: CInt -> IO (Maybe (Int, Int))
ioctlWinsize fd = do
    alloca (\winsizePtr -> do
      status <- c_ioctl_winsize fd winsizePtr
      winsize <- peek winsizePtr
      if' (fromIntegral status == (-1 :: Int))
       (return Nothing)
       (return $ Just (fromIntegral . ws_col $ winsize, fromIntegral . ws_row $ winsize))
       )
       
#elif defined FOUNDATION_SYSTEM_WINDOWS
getStdHandle :: CULong -> IO (Maybe Handle)
getStdHandle handleRef = do
    handle <- c_get_std_handle handleRef
    if' (handle == intPtrToPtr invalidHandleValue)
       (return Nothing)
       (return $ Just handle)

getConsoleScreenBufferInfo :: Handle -> IO (Maybe ConsoleScreenBufferInfo)
getConsoleScreenBufferInfo handle = do
    alloca ( \infoPtr -> do
      status <- c_get_console_screen_buffer_info handle infoPtr
      info <- peek infoPtr
      if' (status == 0)
        (return Nothing)
        (return $ Just info)
      )
       
winWinsize :: CULong -> IO (Maybe (Int, Int))
winWinsize handleRef = do
    maybeHandle <- getStdHandle handleRef
    case maybeHandle of
        Nothing -> return Nothing
        Just handle -> do
            maybeInfo <- getConsoleScreenBufferInfo handle
            case maybeInfo of
                Nothing -> return Nothing
                Just info -> do
                    let window = srWindow info
                        width = fromIntegral (right window - left window + 1)
                        height = fromIntegral (bottom window - top window + 1)
                    return $ Just (width, height)
#endif
-- defined FOUNDATION_SYSTEM_WINDOWS
            
if' :: Bool -> a -> a -> a
if' True t _ = t
if' False _ f = f

popen :: String -> String -> IO (Ptr CFile)
popen cmd_str mode_str = do
    withCAString mode_str (\mode ->
      withCAString cmd_str (\command ->
        c_popen command mode
        )
      )
    
pclose :: Ptr CFile -> IO ()
pclose fp = do
    _ <- c_pclose fp
    return ()
    
fscanfInt :: Ptr CFile -> IO (Int, Int)
fscanfInt fp = do
    alloca (\resultPtr -> do
      status <- c_fscanf_int fp resultPtr
      result <- peek resultPtr
      return (fromIntegral status, fromIntegral result)
      )
        
tputWinsize :: IO (Maybe (Int, Int))
tputWinsize = do
    colFile <- popen "tput cols" "r"
    (colStatus, colResult) <- if' (colFile /= nullPtr) 
        (fscanfInt colFile) 
        (return (0, 0))
    pclose colFile
    rowFile <- popen "tput lines" "r"
    (rowStatus, rowResult) <- if' (rowFile /= nullPtr)
        (fscanfInt rowFile)
        (return (0, 0))
    pclose rowFile
    if' (rowStatus /= 1 || colStatus /= 1)
       (return Nothing)
       (return $ Just (colResult, rowResult))
    

size :: IO (Int, Int)
size = do
#if defined FOUNDATION_SYSTEM_WINDOWS
    maybeWinsize <- winWinsize stdOutputHandle
#elif defined FOUNDATION_SYSTEM_UNIX
    maybeWinsize <- ioctlWinsize 0
#endif
    case maybeWinsize of
        Just winsize1 -> return winsize1
        Nothing -> return (80, 24)