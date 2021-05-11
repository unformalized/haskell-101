{-# LANGUAGE LambdaCase #-}

module Primary.Parser where

import Control.Applicative
import Data.Char
import Control.Monad (replicateM)

newtype Parser a = Parser
  {
    runParser :: String -> Maybe (a, String)
  }

instance Functor Parser where
  fmap f pa = Parser
    (\input ->
        let r = runParser pa input
        in case r of
          Nothing -> Nothing
          Just (a, rest) -> Just (f a, rest)
    )

instance Applicative Parser where
  pure a =
    Parser (\input -> Just (a, input))
  (<*>) pf pa =
    pf >>= (`fmap` pa)

instance Monad Parser where
  return = pure
  (>>=) pa f =
    Parser (\input ->
              case runParser pa input of
                Nothing -> Nothing
                Just (a, rest) ->
                  runParser (f a) rest
           )

instance Alternative Parser where
  empty = Parser (const Nothing)
  (<|>) pa pb =
    Parser (\input ->
              case runParser pa input of
                Nothing -> runParser pb input
                m -> m
           )

instance (Monoid a) => Semigroup (Parser a) where
  (<>) = liftA2 (<>)

instance (Monoid a) => Monoid (Parser a) where
  mempty = pure mempty
  mappend = liftA2 mappend


satisfy :: (Char -> Bool) -> Parser Char
satisfy f =
  Parser (\case
            [] -> Nothing
            x:xs -> if f x then Just (x,xs) else Nothing
         )

char c = satisfy (==c)

-- many >= 0, some >= 1
p1 = runParser (many (satisfy isDigit)) "123bac"
p2 = runParser (many (satisfy isDigit)) "bac"
p3 = runParser (some (satisfy isDigit)) "12abc"
p4 = runParser (some (satisfy isDigit)) "abc"

-- parser HTML Tag

parseHtml = char '<' *> many (satisfy isDigit) <* char '>'

parseNumber :: Parser Int
parseNumber =
  fmap (foldl (\x y -> 10 * x + y) 0) (many digit)
  where
    digit = fmap digitToInt (satisfy isDigit)


parseSeq :: Parser a -> Parser [a] -> Parser [a]
parseSeq = liftA2 (:)

parseStr :: String -> Parser String
parseStr = foldr (parseSeq . char) (pure "")

parseA = char 'a'

parseTenA :: Parser [Char]
parseTenA = replicateM 10 parseA

