-- coming from <https://en.wikipedia.org/wiki/ANSI_escape_code>
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Foundation.IO.Terminal.ANSI
    where

import Foundation.Internal.Base
import Foundation.Numerical
import Foundation.String.UTF8

{-
type ASCII = Word8

esc :: ASCII
esc = 0x1b

data AnsiEscape =
      CursorUp
    | CursorDown
    | CursorForward
    | EraseLine

sEscape :: AnsiEscape -> String
sEscape CursorUp = eraseLine
-}

cursorUp n = csi1 n "A"
cursorDown n = csi1 n "B"
cursorForward n = csi1 n "C"
cursorBack n = csi1 n "D"
cursorNextLine n = csi1 n "E"
cursorPrevLine n = csi1 n "F"
cursorHorizontalAbsolute n = csi1 n "G"
cursorPosition row col = csi2 row col "H"
eraseScreenFromCursor = csi1 0 "J"
eraseScreenToCursor = csi1 1 "J"
eraseScreenAll = csi1 2 "J"
eraseLineFromCursor = csi1 0 "K"
eraseLineToCursor = csi1 1 "K"
eraseLineAll = csi1 2 "K"
scrollUp n = csi1 n "S"
scrollDown n = csi1 n "T"

sgrReset = csi1 0 "m"
sgrForeground n bold
    | n > 7     = error "invalid color"
    | otherwise = if bold then csi2 (30+n) 1 "m" else csi1 (30+n) "m"
sgrBackground n bold
    | n > 7     = error "invalid color"
    | otherwise = if bold then csi2 (40+n) 1 "m" else csi1 (40+n) "m"

-- 256 colors mode
sgrForegroundGray24 v
    | v > 23    = error "invalid gray: only 24 values"
    | otherwise = csi3 38 5 (0xE8 + v) "m"
sgrBackgroundGray24 v
    | v > 23    = error "invalid gray: only 24 values"
    | otherwise = csi3 48 5 (0xE8 + v) "m"
sgrForegroundColor216 r g b
    | r > 5 || g > 5 || b > 5 = error "invalid color: only 6 values per component"
    | otherwise               = csi3 38 5 (0x10 + 36 * r + 6 * g + b) "m"
sgrBackgroundColor216 r g b
    | r > 5 || g > 5 || b > 5 = error "invalid color: only 6 values per component"
    | otherwise               = csi3 48 5 (0x10 + 36 * r + 6 * g + b) "m"

csi0 :: String -> String
csi0 suffix = mconcat ["\ESC[", suffix]

csi1 :: Word -> String -> String
csi1 p1 suffix = mconcat ["\ESC[", pshow p1, suffix]

csi2 :: Word -> Word -> String -> String
csi2 p1 p2 suffix = mconcat ["\ESC[", pshow p1, ";", pshow p2, suffix]

csi3 :: Word -> Word -> Word -> String -> String
csi3 p1 p2 p3 suffix = mconcat ["\ESC[", pshow p1, ";", pshow p2, ";", pshow p3, suffix]

pshow = fromList . show
