module RealWorld.FileIO.ByteStringIO where

import qualified Data.ByteString.Lazy as BSL

hasElfMagic :: BSL.ByteString -> Bool
hasElfMagic content = BSL.take 4 content == elfmagic
  where
    elfmagic = BSL.pack [0x7f, 0x45, 0x4c, 0x46]

isElfFile :: FilePath -> IO Bool
isElfFile path = do
  content <- BSL.readFile path
  return (hasElfMagic content)
