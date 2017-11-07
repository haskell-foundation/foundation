module Basement.Terminal.Size 
    ( size
    ) where
        
import Foreign
import Foreign.C
import Prelude

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
newtype Handle = Ptr ()

data SmallRect = SmallRect {
    left :: CShort,
    top :: CShort,
    right :: CShort,
    bottom :: CShort
}

instance Storable SmallRect where
    sizeOf _ = #{size SMALL_RECT}
    alignment _ = #{alignment SMALL_RECT}
    peek ptr = do
        l <- #{peek SMALL_RECT, Left} ptr
        t <- #{peek SMALL_RECT, Right} ptr
        r <- #{peek SMALL_RECT, Top} ptr
        b <- #{peek SMALL_RECT, Bottom} ptr
        return (SmallRect l t r b)
    poke ptr (SmallRect l t r b) = do
        #{poke SMALL_RECT, Left} ptr l
        #{poke SMALL_RECT, Top} ptr t
        #{poke SMALL_RECT, Right} ptr r
        #{poke SMALL_RECT, Bottom} ptr b
        
data Coord = Coord {
    x :: CShort,
    y :: CShort
}

instance Storable Coord where
    sizeOf _ = #{size Coord}
    alignment _ = #{alignment Coord}
    peek ptr = do
        x <- #{peek Coord, X} ptr
        y <- #{peek Coord, Y} ptr
        return (Coord x y)
    poke ptr (Coord x y) = do
        #{poke Coord, x} ptr x
        #{poke Coord, x} ptr y

data ConsoleScreenBufferInfo = ConsoleScreenBufferInfo {
    dwSize :: Coord,
    dwCursorPosition :: Coord,
    wAttributes :: CUShort,
    srWindow :: SmallRect,
    dwMaximumWindowSize :: Coord,
}

instance Storable ConsoleScreenBufferInfo where
    sizeOf _ = #{size ConsoleScreenBufferInfo}
    alignment _ = #{alignment ConsoleScreenBufferInfo}
    peek ptr = do
        s <- #{peek ConsoleScreenBufferInfo, dwSize} ptr
        c <- #{peek ConsoleScreenBufferInfo, dwCursorPosition} ptr
        a <- #{peek ConsoleScreenBufferInfo, wAttributes} ptr
        w <- #{peek ConsoleScreenBufferInfo, srWindow} ptr
        m <- #{peek ConsoleScreenBufferInfo, dwMaximumWindowSize} ptr
        return (ConsoleScreenBufferInfo s c a w m)
    poke ptr (ConsoleScreenBufferInfo s c a w m) = do
        #{poke ConsoleScreenBufferInfo, dwSize} ptr s
        #{poke ConsoleScreenBufferInfo, dwCursorPosition} ptr c
        #{poke ConsoleScreenBufferInfo, wAttributes} ptr a
        #{poke ConsoleScreenBufferInfo, srWindow} ptr w
        #{poke ConsoleScreenBufferInfo, dwMaximumWindowSize} ptr m
    
invalidHandleValue :: IntPtr
invalidHandleValue = #{const INVALID_HANDLE_VALUE}

stdInputHandle :: CULong
stdInputHandle = #{const STD_INPUT_HANDLE}
#endif
-- defined FOUNDATION_SYSTEM_WINDOWS


foreign import ccall "popen" c_popen :: CString -> CString -> IO (Ptr CFile)
foreign import ccall "pclose" c_pclose :: Ptr CFile -> IO CInt
foreign import ccall "fscanf_int" c_fscanf_int :: Ptr CFile -> Ptr CInt -> IO CInt
#ifdef FOUNDATION_SYSTEM_UNIX
foreign import ccall "ioctl_winsize" c_ioctl_winsize :: CInt -> Ptr Winsize -> IO CInt
#elif defined FOUNDATION_SYSTEM_WINDOWS
foreign import ccall "GetStdHandle" c_get_std_handle :: CULong -> IO Handle
foreign import ccall "GetConsoleScreenBufferInfo" c_get_console_screen_buffer_info :: Handle -> Ptr ConsoleScreenBufferInfo -> IO CInt
#endif

#ifdef FOUNDATION_SYSTEM_UNIX
ioctlWinsize :: CInt -> IO (Maybe (Int, Int))
ioctlWinsize fd = do
    winsizePtr <- malloc :: IO (Ptr Winsize)
    status <- c_ioctl_winsize fd winsizePtr
    winsize <- peek winsizePtr
    free winsizePtr
    if' (fromIntegral status == (-1 :: Int))
       (return Nothing)
       (return $ Just (fromIntegral . ws_col $ winsize, fromIntegral . ws_row $ winsize))
       
#elif defined FOUNDATION_SYSTEM_WINDOWS
getStdHandle :: CULong -> IO (Maybe Handle)
getStdHandle handleRef = do
    handle <- c_get_std_handle handleRef
    if' (handle == intPtrToPtr invalidHandleValue)
       (return Nothing)
       (return Just handle)

getConsoleScreenBufferInfo :: Handle -> IO (Maybe ConsoleScreenBufferInfo)
getConsoleScreenBufferInfo handle = do
    infoPtr <- malloc :: IO (Ptr ConsoleScreenBufferInfo)
    status <- c_get_console_screen_buffer_info handle infoPtr
    info <- peek infoPtr
    free infoPtr
    if' (status == 0)
       (return Nothing)
       (return Just info)
       
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
                        width = (right window - left window + 1)
                        height = (bottom window - top window + 1)
                    return (width height)
#endif
-- defined FOUNDATION_SYSTEM_WINDOWS
            
if' :: Bool -> a -> a -> a
if' True t _ = t
if' False _ f = f

popen :: String -> String -> IO (Ptr CFile)
popen cmd_str mode_str = do
    mode <- newCAString mode_str
    command <- newCAString cmd_str
    filePtr <- c_popen command mode
    free command
    free mode
    return filePtr
    
pclose :: Ptr CFile -> IO ()
pclose fp = do
    _ <- c_pclose fp
    return ()
    
fscanfInt :: Ptr CFile -> IO (Int, Int)
fscanfInt fp = do
    resultPtr <- malloc :: IO (Ptr CInt)
    status <- c_fscanf_int fp resultPtr
    result <- peek resultPtr
    free resultPtr
    return (fromIntegral status, fromIntegral result)
        
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
    maybeWinsize <- winWinsize stdInputHandle
#elif defined FOUNDATION_SYSTEM_UNIX
    maybeWinsize <- ioctlWinsize 0
#endif
    case maybeWinsize of
         Just winsize1 -> return winsize1
         Nothing -> do
             maybeWinsize2 <- tputWinsize
             case maybeWinsize2 of
                  Just winsize2 -> return winsize2
                  Nothing -> return (80, 25)
                  
        

    
