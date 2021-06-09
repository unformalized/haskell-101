module Intermediate.Parsec where

import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as T
import Data.Char

word :: Parsec String () String
word = many1 letter

testword :: IO ()
testword = parseTest word "hello world"

-- oneOf', noneOf' :: String -> ParsecT s u m Char
-- oneOf' cs = satisfy (`elem` cs)
-- noneOf' cs = satisfy (`notElem` cs)

separator :: Parsec String a ()
separator = skipMany1 (space <|> char ',')


-- 使用 p <|> q 运算符时，若是 p Parser 使用了一些输入后才产生错误，则不会在选择 q
test1 :: Parsec String () String
test1 = string "(a)" <|> string "(b)"

-- 可以用 try 函数保证当 p 失败后使用 q 解析 

test2 :: Parsec String () String
test2 = try (string "(a)") <|> string "(b)"


-- 接下来时 Parsec 的 stateUser 完成状态的记录

-- updateState 函数更新 stateUser
word' :: Parsec String Int String
word' = do
  word <- many1 letter
  updateState (+1)
  return word

separator' :: Parsec String Int ()
separator' = skipMany1  (space <|> char ',')

sentence' :: Parsec String Int [String]
sentence' = do
  words <- sepBy1 word' separator'
  oneOf ".?!"
  return words

wordCount :: Parsec String Int Int
wordCount = do
  words <- sentence'
  getState



lexer :: T.TokenParser ()
lexer = T.makeTokenParser emptyDef

lexeme :: Parsec String () a -> Parsec String () a
lexeme = T.lexeme lexer

float :: Parsec String () Double
float = T.float lexer

chars :: Parsec String () [Char]
chars = do
  c1 <- lexeme $ char 'a'
  c2 <- lexeme $ char 'b'
  return [c1, c2]


-- 有符号数

sign :: Num a => Parsec String () (a -> a)
sign = (char '-' >> return negate) <|> (char '+' >> return id) <|> return id

float1 :: Parsec String () Double 
float1 = do
  f <- lexeme sign
  n <- T.float lexer
  return (f n)

testFloat1 = parseTest float1 "1.2"

