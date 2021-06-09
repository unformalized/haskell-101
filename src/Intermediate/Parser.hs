{-# LANGUAGE LambdaCase #-}
module Intermediate.Parser where

-- 处理字符串信息的时候通常需要将这一系列的字符串分析成想要的结构。这种结构通常为树状，常常是上下文相关文法
-- 将 HTML 字符串分析为树状结构
-- 将数学表达式通过语法分析器分析成表达式树，或者分析为词法单元（token），进一步分析称为算术表达式树
-- 现在这些任务都可以通过 Parser Monad 实现，可以分析上下文无关文法也可以分析上下文相关文法

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Char

data Node = Tag String [Node]
          | Text String
          deriving (Show, Eq)

xml :: String
xml = "<html><head>hello world!</head><body>hello again!</body></html>"

type ParserM a = StateT String Maybe a

satisfy :: (Char -> Bool) -> ParserM Char
satisfy p = StateT $ \case
  [] -> Nothing
  (s:ss) -> if p s then Just (s, ss) else Nothing

charM :: Char -> ParserM Char
charM c = satisfy (==c)

letter :: ParserM Char
letter = satisfy isAlpha

string :: String -> ParserM String
string = mapM charM

runParser :: ParserM a -> String -> Maybe (a, String)
runParser = runStateT

textNode :: ParserM Node
textNode = fmap Text $ some $ satisfy (/= '<')

tagNode :: ParserM Node
tagNode = do
  tagName <- charM '<' *> many letter <* charM '>'
  subNode <- many $ tagNode <|> textNode
  string "</" >> string tagName >> charM '>'
  return $ Tag tagName subNode


