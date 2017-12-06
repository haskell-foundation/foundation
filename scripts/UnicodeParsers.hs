{-# LANGUAGE OverloadedStrings #-}
module UnicodeParsers where

import Foundation 
import qualified Foundation.Parser as P
import Foundation.String as S
import Foundation.Collection (Element)

type Comment = String

hexDigits :: String
hexDigits = "1234567890ABCDEF"

comment :: P.Parser String Comment
comment = (P.string "#" *> P.takeWhile (/= '\n') <* P.string "\n") <|> (P.string "\n" *> pure "")

unichar :: P.Parser String String
unichar = P.takeWhile (`elem` hexDigits)

unichars :: P.Parser String [String]
unichars = P.repeat (P.Between $ 1 `P.And` 9) elemz
    where elemz = P.string " " *> unichar

semiCol :: P.Parser String ()
semiCol = P.string "; "

oneOf :: String -> P.Parser String Char
oneOf s = P.satisfy_ (`elem` s)

spaces :: P.Parser String ()
spaces = P.skipWhile (== ' ')
