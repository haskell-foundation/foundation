{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Foundation.Format.CSV
    ( testFormatCSV
    ) where

import Foundation
import Foundation.Format.CSV
import Foundation.Check
import Foundation.String.Builder (toString)

testFormatCSV :: Test
testFormatCSV = Group "CSV"
    [ Group "field unit tests" $ testFieldEncoding <$> fieldUnitTests
    , Group "row unit tests" $ testRowEncoding <$> rowUnitTests
    ]

  where
    testFieldEncoding (f,r) = Property (show f) $
      let str = toString (fieldStringBuilder f)
       in r === str
    testRowEncoding (row,result) = Property (show row) $
      let str = toString (rowStringBuilder row)
       in result === str

fieldUnitTests :: [(Field, String)]
fieldUnitTests =
    [ (FieldInteger 42, "42")
    , (FieldDouble  1, "1.0")
    , (FieldDouble  0.000001, "1.0e-6")
    , (FieldString  "String" NoEscape, "String")
    , (string "String", "String")
    , (string "with comma,string", "\"with comma,string\"")
    , (FieldString  "multiline\nstring" Escape, "\"multiline\nstring\"")
    , (FieldString  "piece of 12\" by 23\""  DoubleEscape, "\"piece of 12\"\" by 23\"\"\"")
    , (string "supported sizes are: 12\", 13\" and 14\"", "\"supported sizes are: 12\"\", 13\"\" and 14\"\"\"")
    ]

rowUnitTests :: [(Row, String)]
rowUnitTests =
    [ (fromList [toField (42 :: Int), toField ("some string" :: String)], "42,some string")
    , (toRow (42 :: Int, "some string" :: String), "42,some string")
    , ( toRow ( 42 :: Int
              , "some string" :: String
              , "supported sizes are: 12\", 13\" and 14\"" :: String
              )
      , "42,some string,\"supported sizes are: 12\"\", 13\"\" and 14\"\"\""
      )
    , ( toRow ( 42 :: Int
              , "some string" :: String
              , "supported sizes are: 12\", 13\" and 14\"" :: String
              , Just 0.000001 :: Maybe Double
              )
      , "42,some string,\"supported sizes are: 12\"\", 13\"\" and 14\"\"\",1.0e-6"
      )
    , ( toRow ( 42 :: Int
              , "some string" :: String
              , "supported sizes are: 12\", 13\" and 14\"" :: String
              , Just 0.000001 :: Maybe Double
              , Nothing       :: Maybe Char
              )
      , "42,some string,\"supported sizes are: 12\"\", 13\"\" and 14\"\"\",1.0e-6,"
      )
    ]
