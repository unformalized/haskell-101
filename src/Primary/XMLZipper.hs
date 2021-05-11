{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Primary.XMLZipper where

import Prelude hiding (readFile)
import Text.XML
import Text.XML.Cursor
import Text.RawString.QQ
import Data.ByteString.Lazy hiding (length, head)

-- data defintion

data Cursor' node = Cursor'
  { parent' :: Maybe (Cursor' node)
  , precedingSibling' :: DiffCursor' node
  , followingSibling' :: DiffCursor' node
  , child :: [Cursor' node]
  , node :: Cursor' node
  }

type DiffCursor' node = [Cursor' node] -> [Cursor' node]
type Axis node = Cursor' node -> [Cursor' node]

xml :: ByteString
xml = [r|
  <html>
    <head>
      <title>My<b> Title1 </b></title>
      <title>My<b> Title2 </b></title>
      <title>My<b> Title3 </b></title>
    </head>
    <body>
      <p>Foo</p>
      <p>Bar</p>
    </body>
  </html>
|]

cursor = fromDocument $ parseLBS_ def xml
title1 = head (cursor $/ element "head" &/ element "title" &/ element "b") $/ content


