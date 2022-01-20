{-# LANGUAGE DeriveFunctor #-}

module RealWorld.ParsePgm.Parse where

import qualified Data.ByteString.Lazy as L
import Data.Char (chr, isDigit, isSpace)
import Data.Word as W
import GHC.Int (Int64)
import RealWorld.ParsePgm.Pnm (GreyMap (GreyMap))

data ParseState = ParseState
  { string :: L.ByteString,
    offset :: Int64
  }
  deriving (Show)

simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined

betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined

newtype Parse a = Parse
  { runParse :: ParseState -> Either String (a, ParseState)
  }
  deriving (Functor)

instance Monad Parse where
  return = identity
  (>>=) ma f =
    Parse
      ( \initState ->
          case runParse ma initState of
            Left errMessage ->
              Left errMessage
            Right (result, newState) ->
              runParse (f result) newState
      )

instance Applicative Parse where
  pure = identity
  (<*>) kf ka = kf >>= (`fmap` ka)

identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState state = Parse (\_ -> Right ((), state))

parse :: Parse a -> L.ByteString -> Either String a
parse parser initState =
  case runParse parser (ParseState initState 0) of
    Left err -> Left err
    Right (result, _) -> Right result

modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset =
  initState {offset = newOffset}

parseByte :: Parse W.Word8
parseByte =
  getState >>= \initState ->
    case L.uncons (string initState) of
      Nothing -> bail "no more input"
      Just (byte, remainder) ->
        putState newState >> return byte
        where
          newState = initState {string = remainder, offset = newOffset}
          newOffset = offset initState + 1

bail :: String -> Parse a
bail err = Parse $ \s ->
  Left $
    "byte offset " ++ show (offset s) ++ ": " ++ err

peekByte :: Parse (Maybe W.Word8)
peekByte = fmap fst . L.uncons . string <$> getState

word2char :: W.Word8 -> Char
word2char = chr . fromIntegral

peekChar :: Parse (Maybe Char)
peekChar = fmap word2char <$> peekByte

parseWhile :: (W.Word8 -> Bool) -> Parse [W.Word8]
parseWhile p =
  peekByte
    >>= ( \pass ->
            if pass == Just True
              then parseByte >>= (\next -> (next :) <$> parseWhile p)
              else identity []
        )
      . fmap p

parseWhileWith :: (W.Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap (fmap f) (parseWhile (p . f))

parseNat :: Parse Int
parseNat =
  parseWhileWith word2char isDigit
    >>= ( \digits ->
            if null digits
              then bail "no more inputs"
              else
                let n = read digits
                 in if n < 0
                      then bail "integer overflow"
                      else identity n
        )

skipSpaces :: Parse ()
skipSpaces = parseWhileWith word2char isSpace >> identity ()

assert :: Bool -> String -> Parse ()
assert True _ = identity ()
assert False err = bail err

parseBytes :: Int -> Parse L.ByteString
parseBytes n =
  getState
    >>= ( \st ->
            let n' = fromIntegral n
                (h, t) = L.splitAt n' (string st)
                st' = st {offset = offset st + L.length h, string = t}
             in putState st' >> assert (L.length h == n') "end of input" >> identity h
        )

parseRawPGM :: Parse GreyMap
parseRawPGM =
  parseWhileWith word2char notWhite >>= \header ->
    skipSpaces
      >> assert (header == "P5") "invalid raw header"
      >> parseNat >>= \width ->
        skipSpaces
          >> parseNat >>= \height ->
            skipSpaces
              >> parseNat >>= \maxGrey ->
                parseByte
                  >> parseBytes (width * height) >>= \bitmap ->
                    identity (GreyMap width height maxGrey bitmap)
  where
    notWhite = (`notElem` " \r\n\t")

parseTimes :: Int -> Parse a -> Parse [a]
parseTimes 0 _ = identity []
parseTimes n p = p >>= \x -> (x :) <$> parseTimes (n - 1) p
